use std::f32::consts::PI;

use bevy::{
    prelude::*,
    render::camera::Camera,
    input::{
        mouse::{MouseMotion, MouseWheel, MouseButton},
        keyboard::KeyCode,
    },
    pbr::{AlphaMode, StandardMaterial},
    render::mesh::{Indices, PrimitiveTopology},
    render::render_asset::RenderAssetUsages,
    window::PrimaryWindow,
};
use std::{
    path::{Path, PathBuf},
    collections::{HashMap, BTreeMap},
    time::SystemTime,
    env,
    fs,
    io::Write,
    process,
};
use hooded_crow_modeller::{Model, Group};
use bevy_egui::{egui, EguiPlugin, EguiContexts};
use gltf::json::{self, validation::{USize64, Checked}};
use gltf::json::buffer::Stride;
use bytemuck;
use serde_json;

#[derive(Debug, Clone, Copy)]
struct BoneColor(pub [f32; 3]);

#[derive(Debug, Clone)]
struct BonePosition {
    start: Vec3,
    end: Vec3,
    color: BoneColor,
}

#[derive(Resource, Clone)]
struct MeshData {
    positions: HashMap<String, BonePosition>,
    transforms: HashMap<String, Mat4>,
    skin_vertex_positions: Vec<(Vec3, [f32; 3], Option<String>, [f32; 4])>, // Store skin vertex positions, colors and IDs
    triangles: HashMap<String, [Vec3; 3]>, // Store triangles as (name, [vertex positions])
    skin_vertex_colors: Vec<[f32; 4]>, // Store colors for skin vertices
}

impl Default for MeshData {
    fn default() -> Self {
        Self {
            positions: HashMap::new(),
            transforms: HashMap::new(),
            skin_vertex_positions: Vec::new(),
            triangles: HashMap::new(),
            skin_vertex_colors: Vec::new(),
        }
    }
}

#[derive(Resource, PartialEq, Clone, Debug)]
enum BoneVisualization {
    Solid,
    ByDepth,
    ByChain,
}

#[derive(Resource)]
struct ExportPath(PathBuf);

#[derive(Resource, Clone)]
struct MeshFile(PathBuf);

#[derive(Resource)]
struct VisualizationSettings {
    visualization_mode: BoneVisualization,
    line_width: f32,
    show_triangles: bool,
}

#[derive(Resource)]
struct ColorCache {
    colors: BTreeMap<String, BoneColor>,
}

#[derive(Resource)]
struct ExportState {
    path: PathBuf,
    exported: bool,
}

#[derive(Component)]
struct CameraController {
    orbit_sensitivity: f32,
    pan_sensitivity: f32,
    zoom_sensitivity: f32,
    orbit_button: MouseButton,
    pan_button: MouseButton,
}

impl Default for CameraController {
    fn default() -> Self {
        Self {
            orbit_sensitivity: 1.0,
            pan_sensitivity: 0.002,
            zoom_sensitivity: 0.5,
            orbit_button: MouseButton::Left,
            pan_button: MouseButton::Right,
        }
    }
}

#[derive(Resource)]
struct ShowAxes(bool);

#[derive(Resource)]
struct LastModified(SystemTime);

#[derive(Event)]
struct FileChangedEvent {
    model: Model,
}

#[derive(Resource)]
struct TriangleMeshHandles {
    handles: Vec<Entity>,
}

impl Default for TriangleMeshHandles {
    fn default() -> Self {
        Self {
            handles: Vec::new(),
        }
    }
}

fn hash_content(content: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}

fn calculate_transform(orientation: f32, slope: f32, rotation: f32) -> Mat4 {
    // Convert angles to radians
    let orientation_rad = orientation.to_radians();
    let slope_rad = slope.to_radians();
    let rotation_rad = rotation.to_radians();

    // For vertical slopes (±90°), use the raw slope value
    if (slope - 90.0).abs() < 0.001 {
        // For slope = 90° (pointing up), create a transform that points directly up
        return Mat4::from_rotation_y(orientation_rad) * Mat4::from_rotation_x(-PI/2.0) * Mat4::from_rotation_z(rotation_rad);
    } else if (slope + 90.0).abs() < 0.001 {
        // For slope = -90° (pointing down), create a transform that points directly down
        return Mat4::from_rotation_y(orientation_rad) * Mat4::from_rotation_x(PI/2.0) * Mat4::from_rotation_z(rotation_rad);
    } else {
        // For non-vertical slopes, create a transform that:
        // 1. Points along +Z at 0 orientation
        // 2. Rotates by orientation around Y (positive = towards +X)
        // 3. Tilts up/down by slope around the local X axis (INVERTED to match vertical case)
        // 4. Applies final rotation around the bone's axis
        let slope_rot = Mat4::from_rotation_x(-slope_rad); // Invert sign so positive slopes point up
        let orientation_rot = Mat4::from_rotation_y(orientation_rad);
        let rotation_rot = Mat4::from_rotation_z(rotation_rad);
        
        orientation_rot * slope_rot * rotation_rot
    }
}

fn process_bone_group(
    group: &Group,
    parent_transform: Mat4,
    _parent_end: Vec3,
    parent_path: &str,
    transforms: &mut HashMap<String, (Mat4, Vec3)>,
    positions: &mut HashMap<String, (Vec3, Vec3)>,
    current_path: &str,
) {
    println!("\nProcessing group at: {}", current_path);
    println!("Group bones: {:?}", group.bones.keys());
    println!("Group subgroups: {:?}", group.subgroups.keys());
    println!("Parent path: {}", parent_path);

    // Process bones in current group
    for (bone_path, bone) in &group.bones {
        println!("\nProcessing bone: {}", bone_path);
        println!("Looking for parent: {}", parent_path);

        // Get the parent bone's end position
        let start = if bone_path.starts_with("body.") && bone_path.matches('.').count() == 1 {
            // Any direct child of "body" starts at the origin
            println!("Root bone, starting at origin: {}", bone_path);
            Vec3::ZERO
        } else {
            // Get parent's end position from the positions map
            match positions.get(parent_path) {
                Some((_, end)) => {
                    println!("Found parent {} at end: {:?}", parent_path, end);
                    *end
                }
                None => {
                    println!("Parent {} not found, using zero", parent_path);
                    Vec3::ZERO
                }
            }
        };

        // Calculate bone's local transform
        let local_transform = calculate_transform(bone.resolved_orientation, bone.resolved_slope, bone.resolved_rotation);
        
        // Combine with parent transform
        let world_transform = parent_transform * local_transform;
        
        // Calculate direction using the world transform
        let direction = world_transform.transform_vector3(Vec3::Z).normalize();
        
        // Calculate end position using the transformed direction
        let end = start + direction * bone.length;
        println!("Bone {} from {:?} to {:?}", bone_path, start, end);

        // Store positions and transforms
        positions.insert(bone_path.clone(), (start, end));
        transforms.insert(bone_path.clone(), (world_transform, end));

        // Process any subgroups
        for (subgroup_path, subgroup) in &group.subgroups {
            println!("\nChecking subgroup: {} for bone: {}", subgroup_path, bone_path);
            
            // Get just the bone name without the path
            let bone_name = bone_path.split('.').last().unwrap_or(bone_path);
            println!("Bone name: {}", bone_name);
            
            if subgroup_path.starts_with(bone_name) {
                println!("Found matching subgroup");
                let full_subgroup_path = format!("{}.{}", current_path, subgroup_path);
                println!("Full subgroup path: {}", full_subgroup_path);
                
                process_bone_group(
                    subgroup,
                    world_transform, // Pass the bone's world transform to children
                    end,
                    bone_path,
                    transforms,
                    positions,
                    &full_subgroup_path,
                );
            }
        }
    }
}

fn get_bone_depth(bone_path: &str) -> usize {
    // Split the path and count actual bone levels
    // Skip the first "body" since it's just a container
    let parts: Vec<&str> = bone_path.split('.').collect();
    if parts.is_empty() {
        return 0;
    }
    
    // Subtract 1 to account for "body" prefix
    parts.len().saturating_sub(1)
}

fn get_rainbow_color(depth: usize, max_depth: usize) -> BoneColor {
    // Map depth to hue (0.0 to 1.0)
    // Reverse the hue so that deeper bones are redder (more attention-grabbing)
    let hue = if max_depth > 1 {
        1.0 - (depth as f32 / (max_depth - 1) as f32)
    } else {
        0.0
    };
    
    // Convert HSV to RGB with full saturation and value
    let (r, g, b) = hsv_to_rgb(hue, 0.8, 0.9);
    BoneColor([r, g, b])
}

fn find_group<'a>(model: &'a Model, path: &str) -> Option<&'a Group> {
    let parts: Vec<_> = path.split('.').collect();
    if parts.is_empty() || parts[0] != "body" {
        return None;
    }

    let mut current = model.0.get("body")?;
    
    for part in parts.iter().skip(1) {
        current = current.subgroups.get(*part)?;
    }
    
    Some(current)
}

fn process_bones_in_order(
    model: &Model,
    transforms: &mut HashMap<String, (Mat4, Vec3)>,
    positions: &mut HashMap<String, (Vec3, Vec3)>,
) {
    // First find all bones and their depths
    let mut bones_by_depth: BTreeMap<usize, Vec<String>> = BTreeMap::new();
    
    fn collect_bones(
        group: &Group,
        path: &str,
        bones_by_depth: &mut BTreeMap<usize, Vec<String>>,
    ) {
        let depth = path.matches('.').count();
        
        // Add this group if it has bones
        if !group.bones.is_empty() {
            bones_by_depth
                .entry(depth)
                .or_default()
                .push(path.to_string());
        }
        
        // Recursively process subgroups
        for (name, subgroup) in &group.subgroups {
            let subpath = if path.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", path, name)
            };
            collect_bones(subgroup, &subpath, bones_by_depth);
        }
    }

    // Collect all bones by their depth in the hierarchy
    if let Some(body) = model.0.get("body") {
        collect_bones(body, "body", &mut bones_by_depth);
    }

    println!("Found bones at depths: {:?}", bones_by_depth.keys().collect::<Vec<_>>());
    for (depth, paths) in &bones_by_depth {
        println!("Depth {}: {:?}", depth, paths);
    }

    // Process bones level by level, starting from the root (lowest depth)
    for (depth, paths) in bones_by_depth {
        for path in paths {
            // Find parent path by removing the last segment
            let parent_path = if depth > 1 {
                path.rsplit_once('.')
                    .map(|(parent, _)| parent)
                    .unwrap_or("")
                    .to_string()
            } else {
                String::new()
            };

            if let Some(group) = find_group(model, &path) {
                println!("Processing bone at depth {}: {} (parent: {})", depth, path, parent_path);
                
                // Get parent transform if available, otherwise use identity
                let parent_transform = if !parent_path.is_empty() {
                    transforms.get(parent_path.as_str())
                        .map(|(transform, _)| *transform)
                        .unwrap_or(Mat4::IDENTITY)
                } else {
                    Mat4::IDENTITY
                };
                
                // Get parent end position if available
                let parent_end = if !parent_path.is_empty() {
                    transforms.get(parent_path.as_str())
                        .map(|(_, end)| *end)
                        .unwrap_or(Vec3::ZERO)
                } else {
                    Vec3::ZERO
                };
                
                process_bone_group(
                    group,
                    parent_transform, // Use parent transform instead of identity
                    parent_end,
                    parent_path.as_str(),
                    transforms,
                    positions,
                    &path,
                );
            } else {
                println!("Warning: Could not find group for path: {}", path);
            }
        }
    }

    println!("Processed {} bones", positions.len());
    for (name, pos) in positions.iter() {
        let (start, end) = pos;
        println!("Bone {}: {} -> {}", name, start, end);
    }
}

fn handle_file_changes(
    mut file_events: EventReader<FileChangedEvent>,
    mut commands: Commands,
    export_path: Option<Res<ExportPath>>,
    mut camera_query: Query<(&mut Transform, &CameraController), With<Camera>>,
    light_query: Query<Entity, With<DirectionalLight>>,
) {
    for event in file_events.read() {
        let model = &event.model;
        println!("*** File changed! Reloading model... ***");
        
        // Clean up existing lights
        for light in light_query.iter() {
            commands.entity(light).despawn();
        }
        
        // Store current camera position
        let (mut camera_transform, _controller) = camera_query.single_mut();
        let camera_position = camera_transform.translation;
        
        // Process bones to build positions and skin vertices
        let bones = model.get_bones();
        // println!("Found {} bones in the model", bones.len());
        // for (path, bone) in &bones {
        //     println!("Processing bone: {} with {} skin vertices", path, bone.skin_verts.len());
        // }

        // Initialize transform and position maps
        let mut transforms = HashMap::new();
        let mut positions = HashMap::new();
        let mut color_map = BTreeMap::new();

        // Process bones in hierarchical order
        process_bones_in_order(model, &mut transforms, &mut positions);

        // Find maximum bone depth for color mapping
        let max_depth = positions.keys()
            .map(|path| get_bone_depth(path))
            .max()
            .unwrap_or(0);

        // Combine bone hierarchy with calculated positions
        let mut positions_with_colors: HashMap<String, (Vec3, Vec3, [f32; 3])> = HashMap::new();
        let mut skin_vertex_positions = Vec::new();
        let mut triangles: HashMap<String, [Vec3; 3]> = HashMap::new();
        let mut skin_vertex_colors = Vec::new();
        
        // Iterate through all bones in the model
        // println!("SETUP: Processing bones for positions and skin vertices");
        let all_bones = model.get_bones();
        // println!("SETUP: Model has {} total bones", all_bones.len());
        for (full_path, bone) in &all_bones {
            // println!("SETUP: Processing bone {} with {} skin verts", full_path, bone.skin_verts.len());
            
            // Store bone position and color
            if let Some((start, end)) = positions.get(full_path.as_str()) {
                let bone_depth = get_bone_depth(full_path.as_str());
                let color = get_rainbow_color(bone_depth, max_depth).0;
                positions_with_colors.insert(full_path.clone(), (*start, *end, color));
                
                // Process skin vertices for this bone
                // println!("Processing skin vertices for bone: {}", full_path);
                // println!("  Bone has {} skin vertices", bone.skin_verts.len());
                // println!("  Bone position: start={:?}, end={:?}", start, end);
                
                for skin_vert in &bone.skin_verts {
                    // Calculate position of skin vertex using the bone's start and end
                    let position = skin_vert.calculate_position(*start, *end, bone.resolved_rotation);
                    
                    // Use the skin vertex's color if available, otherwise use bone's color or default
                    let color = skin_vert.color
                        .or_else(|| bone.color)
                        .unwrap_or_else(|| [0.5, 0.5, 0.5, 0.5]);
                    
                    // Store the vertex position and ID
                    let id = skin_vert.id.clone().unwrap_or_else(|| full_path.clone());
                    
                    // Add skin vertex to our collection
                    skin_vertex_positions.push((position, [1.0, 1.0, 1.0], Some(id.clone()), color));
                    skin_vertex_colors.push(color);
                    // println!("PROCESSING: Added skin vertex at {:?} for bone '{}' with id {:?}", position, full_path, id);
                }
            }
        }

        println!("Loaded skin vertex colors from TOML: {:?}", skin_vertex_colors);

        // Get triangles directly from calculated vertices rather than from model.collect_triangles()
        
        // Create a map of vertex IDs to positions and colors for faster lookup
        let vertex_id_map: HashMap<String, (Vec3, [f32; 4])> = skin_vertex_positions.iter()
            .filter_map(|(pos, _, id, color)| {
                id.as_ref().map(|id| (id.clone(), (*pos, *color)))
            })
            .collect();

        // Get triangle definitions from the model
        let bones = model.get_bones();
        let mut triangle_colors: HashMap<String, [f32; 4]> = HashMap::new();
        
        for (bone_name, bone) in &bones {
            for skin_vert in &bone.skin_verts {
                // For each triangle this vertex is part of
                for (face_name, position_in_triangle) in &skin_vert.triangles {
                    // Get the ID for this vertex
                    let vertex_id = skin_vert.id.clone().unwrap_or_else(|| format!("{}_{}", bone_name, position_in_triangle));
                    
                    // Create a face entry if it doesn't exist
                    if !triangles.contains_key(face_name) {
                        triangles.insert(face_name.clone(), [Vec3::ZERO, Vec3::ZERO, Vec3::ZERO]);
                    }
                    
                    // Set the vertex position and color in the triangle
                    if let Some((position, color)) = vertex_id_map.get(&vertex_id) {
                        if let Some(triangle_verts) = triangles.get_mut(face_name) {
                            // Update the vertex at its position in the triangle
                            if *position_in_triangle < 3 {
                                triangle_verts[*position_in_triangle] = *position;
                                
                                // If this is the first vertex (position 0), store its color for the whole triangle
                                if *position_in_triangle == 0 {
                                    triangle_colors.insert(face_name.clone(), *color);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Store triangle colors in skin_vertex_colors in the same order as triangles
        skin_vertex_colors.clear();
        for (name, vertices) in &triangles {
            // Find the color of the first vertex in this triangle
            let first_vertex_color = skin_vertex_positions.iter()
                .find(|(pos, _, _, color)| *pos == vertices[0])
                .map(|(_, _, _, color)| *color)
                .unwrap_or([0.5, 0.5, 0.5, 0.5]); // Default gray if no color found
            skin_vertex_colors.push(first_vertex_color);
        }
        
        // Debug print what triangles we've collected
        println!("TRIANGLES:");
        for (name, verts) in &triangles {
            println!("Triangle {}: {:?}, {:?}, {:?}", name, verts[0], verts[1], verts[2]);
        }

        // Create the final MeshData with debug output
        // println!("*** SKIN VERTEX SUMMARY ***");
        // println!("Total skin vertices processed: {}", skin_vertex_positions.len());
        // for (i, (pos, _, id, _)) in skin_vertex_positions.iter().enumerate() {
        //     println!("Vertex {}: position={:?}, id={:?}", i, pos, id);
        // }

        // Create mesh data
        let mut mesh_data = MeshData::default();
        
        // First update bone positions
        for (name, (start, end, color)) in &positions_with_colors {
            mesh_data.positions.insert(name.clone(), BonePosition {
                start: *start,
                end: *end,
                color: BoneColor(*color),
            });
            color_map.insert(name.clone(), BoneColor(*color));
            
            // Store transforms
            if let Some((transform, _)) = transforms.get(name.as_str()) {
                mesh_data.transforms.insert(name.clone(), *transform);
            }
        }

        mesh_data.skin_vertex_positions = skin_vertex_positions;
        mesh_data.triangles = triangles;
        mesh_data.skin_vertex_colors = skin_vertex_colors;

        // Set up camera
        camera_transform.translation = camera_position;
        camera_transform.look_at(Vec3::ZERO, Vec3::Y);

        // Add ambient light
        commands.insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 0.5,
        });
        
        // Add a directional light to aid in visibility
        commands.spawn(DirectionalLightBundle {
            directional_light: DirectionalLight {
                illuminance: 10000.0,
                shadows_enabled: true,
                ..default()
            },
            transform: Transform {
                translation: Vec3::new(0.0, 2.0, 0.0),
                rotation: Quat::from_rotation_x(-std::f32::consts::FRAC_PI_4),
                ..default()
            },
            ..default()
        });

        // Insert resources
        commands.insert_resource(mesh_data);
        commands.insert_resource(ShowAxes(true));
        commands.insert_resource(VisualizationSettings {
            visualization_mode: BoneVisualization::Solid,
            line_width: 5.0,
            show_triangles: true,
        });
        commands.insert_resource(ColorCache { colors: color_map.clone() });

        if let Some(ref export_res) = export_path {
            commands.insert_resource(ExportState {
                path: export_res.0.clone(),
                exported: false,
            });
        }
    }
}

fn setup(
    mut commands: Commands,
    mesh_file: Res<MeshFile>,
    export_path: Option<Res<ExportPath>>,
) {
    // Initialize LastModified with current file modification time
    let modified = fs::metadata(&mesh_file.0)
        .and_then(|m| m.modified())
        .unwrap_or_else(|_| SystemTime::now());
    commands.insert_resource(LastModified(modified));

    // Set up camera
    commands.spawn((
        Camera3dBundle {
            transform: Transform::from_xyz(5.0, 5.0, 5.0).looking_at(Vec3::ZERO, Vec3::Y),
            ..default()
        },
        CameraController::default(),
    ));

    // Add ambient light
    commands.insert_resource(AmbientLight {
        color: Color::WHITE,
        brightness: 0.5,
    });
    
    // Add a directional light to aid in visibility
    commands.spawn(DirectionalLightBundle {
        directional_light: DirectionalLight {
            illuminance: 10000.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform {
            translation: Vec3::new(0.0, 2.0, 0.0),
            rotation: Quat::from_rotation_x(-std::f32::consts::FRAC_PI_4),
            ..default()
        },
        ..default()
    });

    // Initial file read and model setup
    let content = match fs::read_to_string(&mesh_file.0) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            process::exit(1);
        }
    };

    let model = match Model::from_toml(&content) {
        Ok(model) => model,
        Err(e) => {
            eprintln!("Error parsing TOML: {}", e);
            process::exit(1);
        }
    };

    // Initialize resources
    commands.insert_resource(ShowAxes(true));
    commands.insert_resource(VisualizationSettings {
        visualization_mode: BoneVisualization::Solid,
        line_width: 5.0,
        show_triangles: true,
    });
    commands.insert_resource(TriangleMeshHandles::default());

    // Process initial model
    let mut transforms = HashMap::new();
    let mut positions = HashMap::new();
    process_bones_in_order(&model, &mut transforms, &mut positions);

    let mut mesh_data = MeshData::default();
    
    // Process bones and create initial mesh data
    let mut positions_with_colors = HashMap::new();
    let mut skin_vertex_positions = Vec::new();
    let mut triangles = HashMap::new();
    let mut skin_vertex_colors = Vec::new();

    let max_depth = positions.keys()
        .map(|path| get_bone_depth(path))
        .max()
        .unwrap_or(0);

    let all_bones = model.get_bones();
    for (full_path, bone) in &all_bones {
        if let Some((start, end)) = positions.get(full_path.as_str()) {
            let bone_depth = get_bone_depth(full_path.as_str());
            let color = get_rainbow_color(bone_depth, max_depth).0;
            positions_with_colors.insert(full_path.clone(), (*start, *end, color));
            
            for skin_vert in &bone.skin_verts {
                let position = skin_vert.calculate_position(*start, *end, bone.resolved_rotation);
                let color = skin_vert.color
                    .or_else(|| bone.color)
                    .unwrap_or_else(|| [0.5, 0.5, 0.5, 0.5]);
                let id = skin_vert.id.clone().unwrap_or_else(|| full_path.clone());
                skin_vertex_positions.push((position, [1.0, 1.0, 1.0], Some(id.clone()), color));
                skin_vertex_colors.push(color);
            }
        }
    }

    // Update mesh data with initial state
    for (name, (start, end, color)) in &positions_with_colors {
        mesh_data.positions.insert(name.clone(), BonePosition {
            start: *start,
            end: *end,
            color: BoneColor(*color),
        });
        
        if let Some((transform, _)) = transforms.get(name.as_str()) {
            mesh_data.transforms.insert(name.clone(), *transform);
        }
    }

    mesh_data.skin_vertex_positions = skin_vertex_positions;
    mesh_data.triangles = triangles;
    mesh_data.skin_vertex_colors = skin_vertex_colors;

    commands.insert_resource(mesh_data);
}

fn monitor_file(
    mesh_file: Res<MeshFile>,
    mut last_modified: ResMut<LastModified>,
    mut file_events: EventWriter<FileChangedEvent>,
) {
    if let Ok(metadata) = fs::metadata(&mesh_file.0) {
        if let Ok(modified) = metadata.modified() {
            if modified > last_modified.0 {
                if let Ok(content) = fs::read_to_string(&mesh_file.0) {
                    match Model::from_toml(&content) {
                        Ok(model) => {
                            println!("\nFile changed, reloading model...");
                            last_modified.0 = modified;
                            file_events.send(FileChangedEvent { model });
                        }
                        Err(e) => {
                            println!("\nInvalid TOML: {}", e);
                        }
                    }
                }
            }
        }
    }
}

fn export_system(
    export_state: Option<ResMut<ExportState>>,
    mesh_data: Option<Res<MeshData>>,
) {
    if let (Some(mut export_state), Some(mesh_data)) = (export_state, mesh_data) {
        if !export_state.exported {
            if let Err(err) = export_to_glb(&mesh_data, &export_state.path) {
                eprintln!("Failed to export GLB: {}", err);
            } else {
                println!("Successfully exported GLB to {:?}", export_state.path);
                export_state.exported = true;
            }
        }
    }
}

fn mouse_wheel(
    mut scroll_evr: EventReader<MouseWheel>,
    mut query: Query<(&mut Transform, &CameraController), With<Camera>>,
) {
    let (mut transform, controller) = query.single_mut();
    for ev in scroll_evr.read() {
        let forward = transform.forward();
        transform.translation += forward * ev.y * controller.zoom_sensitivity;
    }
}

fn update_camera(
    time: Res<Time>,
    keyboard: Res<ButtonInput<KeyCode>>,
    mut query: Query<&mut Transform, With<Camera>>,
) {
    let mut transform = query.single_mut();
    let speed = 5.0;
    let delta = time.delta_seconds();

    let forward = transform.forward();
    let right = transform.right();

    if keyboard.pressed(KeyCode::KeyW) {
        transform.translation += forward * speed * delta;
    }
    if keyboard.pressed(KeyCode::KeyS) {
        transform.translation -= forward * speed * delta;
    }
    if keyboard.pressed(KeyCode::KeyA) {
        transform.translation -= right * speed * delta;
    }
    if keyboard.pressed(KeyCode::KeyD) {
        transform.translation += right * speed * delta;
    }
    if keyboard.pressed(KeyCode::KeyQ) {
        transform.translation += Vec3::Y * speed * delta;
    }
    if keyboard.pressed(KeyCode::KeyE) {
        transform.translation -= Vec3::Y * speed * delta;
    }
}

fn keyboard_input(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut show_axes: ResMut<ShowAxes>,
    _commands: Commands,
) {
    if keyboard.just_pressed(KeyCode::KeyG) {
        show_axes.0 = !show_axes.0;
    }
}

fn mouse_motion(
    mut motion_evr: EventReader<MouseMotion>,
    mouse_button_input: Res<ButtonInput<MouseButton>>,
    mut query: Query<(&mut Transform, &CameraController), With<Camera>>,
) {
    let mouse_sensitivity = 0.001;
    let origin = Vec3::ZERO;
    
    for ev in motion_evr.read() {
        if mouse_button_input.pressed(MouseButton::Left) {
            for (mut transform, _controller) in query.iter_mut() {
                let delta_x = ev.delta.x * mouse_sensitivity;
                let delta_y = ev.delta.y * mouse_sensitivity;
                
                // Store current distance from origin
                let distance = (transform.translation - origin).length();
                
                // Create rotation quaternions
                let y_rotation = Quat::from_rotation_y(-delta_x);
                let right: Vec3 = transform.right().into();
                let x_rotation = Quat::from_axis_angle(right, -delta_y);

                // Apply rotations around origin
                let rotation = y_rotation * x_rotation;
                transform.rotate_around(origin, rotation);
                
                // Maintain distance from origin
                let dir = (transform.translation - origin).normalize();
                transform.translation = origin + dir * distance;
            }
        }
    }
}

fn mouse_button_input(
    mouse_button_input: Res<ButtonInput<MouseButton>>,
    mut query: Query<&mut Transform, With<Camera>>,
    _commands: Commands,
) {
    if mouse_button_input.just_pressed(MouseButton::Middle) {
        let mut transform = query.single_mut();
        transform.translation = Vec3::new(0.0, 0.0, 5.0);
        transform.look_at(Vec3::ZERO, Vec3::Y);
    }
}

fn update_triangle_meshes(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_data: Option<Res<MeshData>>,
    settings: Res<VisualizationSettings>,
    mut triangle_handles: ResMut<TriangleMeshHandles>,
    mut file_events: EventReader<FileChangedEvent>,
) {
    // Only update if we received a file change event
    if file_events.is_empty() {
        return;
    }

    // Clear previous triangle meshes
    for entity in triangle_handles.handles.drain(..) {
        commands.entity(entity).despawn();
    }

    // If we don't have mesh data or triangles aren't visible, return
    if mesh_data.is_none() || !settings.show_triangles {
        return;
    }

    if let Some(mesh_data) = mesh_data {
        println!("UPDATING: Creating {} triangle meshes", mesh_data.triangles.len());

        // Create a semi-transparent green material
        for (i, (name, vertices)) in mesh_data.triangles.iter().enumerate() {
            println!("MESH: Creating triangle mesh '{}'", name);
            
            // Create a mesh for this triangle
            let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default());
            
            // Set vertex positions (just the 3 vertices)
            mesh.insert_attribute(
                Mesh::ATTRIBUTE_POSITION, 
                vec![vertices[0], vertices[1], vertices[2]]
            );
            
            // Set normals (all facing the same direction for simplicity)
            let normal = (vertices[1] - vertices[0]).cross(vertices[2] - vertices[0]).normalize();
            mesh.insert_attribute(
                Mesh::ATTRIBUTE_NORMAL, 
                vec![normal, normal, normal]
            );
            
            // Set UV coordinates (not really used, but required)
            mesh.insert_attribute(
                Mesh::ATTRIBUTE_UV_0, 
                vec![[0.0, 0.0], [1.0, 0.0], [0.5, 1.0]]
            );
            
            // Set indices
            mesh.insert_indices(Indices::U32(vec![0, 1, 2]));
            
            // Assign color from skin vertex colors
            let color = mesh_data.skin_vertex_colors.get(i).unwrap_or(&[0.5, 0.5, 0.5, 0.5]); // Default to gray if no color
            let triangle_material = materials.add(StandardMaterial {
                base_color: Color::rgba(color[0], color[1], color[2], color[3]),
                alpha_mode: AlphaMode::Blend,
                double_sided: true,  // Make sure both sides are visible
                cull_mode: None,     // Disable face culling
                unlit: true,         // Make it unlit so it's always visible regardless of lighting
                ..default()
            });
            
            // Spawn the mesh entity
            let mesh_handle = meshes.add(mesh);
            let entity = commands.spawn(PbrBundle {
                mesh: mesh_handle,
                material: triangle_material,
                ..default()
            }).id();
            
            // Store the entity handle for cleanup later
            triangle_handles.handles.push(entity);
        }
    }
}

fn draw_bones(
    mut gizmos: Gizmos,
    mesh_data: Option<Res<MeshData>>,
    _settings: Res<VisualizationSettings>,
) {
    if let Some(mesh_data) = mesh_data {
        // Draw bones
        for bone in mesh_data.positions.values() {
            // Draw bone using pre-transformed positions
            gizmos.line(
                bone.start,
                bone.end,
                Color::rgb(bone.color.0[0], bone.color.0[1], bone.color.0[2])
            );
        }
        
        // Draw skin vertices as spheres
        for (i, (position, color, _, _)) in mesh_data.skin_vertex_positions.iter().enumerate() {
            // Draw a sphere at the vertex position
            let sphere_radius = 0.005; // Size of the sphere (reduced to 10% of original 0.05)
            gizmos.sphere(
                *position,
                Quat::IDENTITY,
                sphere_radius,
                Color::rgb(color[0], color[1], color[2])
            );
        }
    }
}

fn draw_triangles(
    mut gizmos: Gizmos,
    mesh_data: Option<Res<MeshData>>,
    settings: Res<VisualizationSettings>,
) {
    // If we don't have the mesh data or triangles aren't visible, don't draw
    if mesh_data.is_none() || !settings.show_triangles {
        return;
    }
    
    if let Some(mesh_data) = mesh_data {
        // Draw triangle outlines only - the mesh system handles the filling
        for (i, (name, vertices)) in mesh_data.triangles.iter().enumerate() {
            // Draw the triangle outline only
            let color = mesh_data.skin_vertex_colors.get(i).unwrap_or(&[0.5, 0.5, 0.5, 0.5]); // Default to gray if no color
            gizmos.line(vertices[0], vertices[1], Color::rgba(color[0], color[1], color[2], color[3]));
            gizmos.line(vertices[1], vertices[2], Color::rgba(color[0], color[1], color[2], color[3]));
            gizmos.line(vertices[2], vertices[0], Color::rgba(color[0], color[1], color[2], color[3]));
        }
    }
}

fn draw_axes(mut gizmos: Gizmos, show_axes: Res<ShowAxes>) {
    if !show_axes.0 {
        return;
    }

    let grid_size = 10;
    let grid_spacing = 0.5;

    // Draw grid on XZ plane
    let grid_extent = grid_size as f32 * grid_spacing;
    for i in -grid_size..=grid_size {
        let offset = i as f32 * grid_spacing;
        let alpha = if i == 0 { 0.5 } else { 0.2 };
        let color = Color::rgba(0.5, 0.5, 0.5, alpha);

        // X-parallel lines
        gizmos.line(
            Vec3::new(-grid_extent, 0.0, offset),
            Vec3::new(grid_extent, 0.0, offset),
            color,
        );

        // Z-parallel lines
        gizmos.line(
            Vec3::new(offset, 0.0, -grid_extent),
            Vec3::new(offset, 0.0, grid_extent),
            color,
        );
    }
}

fn update_ui(
    mut contexts: EguiContexts,
    mut settings: ResMut<VisualizationSettings>,
    mesh_data: Option<Res<MeshData>>,
    camera_query: Query<(&Camera, &GlobalTransform), With<Camera>>,
    _windows: Query<&Window>,
) {
    let ctx = contexts.ctx_mut();

    egui::Window::new("Settings")
        .show(ctx, |ui| {
            ui.heading("Visualization Mode");
            ui.horizontal(|ui| {
                ui.radio_value(
                    &mut settings.visualization_mode,
                    BoneVisualization::Solid,
                    "Solid",
                );
                ui.radio_value(
                    &mut settings.visualization_mode,
                    BoneVisualization::ByDepth,
                    "By Depth",
                );
                ui.radio_value(
                    &mut settings.visualization_mode,
                    BoneVisualization::ByChain,
                    "By Chain",
                );
            });

            ui.add(
                egui::Slider::new(&mut settings.line_width, 1.0..=10.0)
                    .text("Bone Thickness"),
            );
            
            ui.checkbox(&mut settings.show_triangles, "Show Triangles");
        });

    // Draw bone labels using egui
    if let (Some(mesh_data), Ok((camera, camera_transform))) = (mesh_data.as_ref(), camera_query.get_single()) {
        // First collect all the label data we need
        let label_data: Vec<_> = mesh_data.positions.iter()
            .filter_map(|(path, bone)| {
                let name = path.split('.').last()?;
                let center = bone.start.lerp(bone.end, 0.5);
                let screen_pos = camera.world_to_viewport(camera_transform, center)?;
                Some((name.to_string(), screen_pos))
            })
            .collect();

        // Then render the labels
        for (name, screen_pos) in label_data {
            egui::Area::new(egui::Id::new(format!("bone_{}", name.as_str())))
                .fixed_pos(egui::pos2(screen_pos.x, screen_pos.y))
                .show(ctx, |ui| {
                    ui.horizontal(|ui| {
                        ui.colored_label(
                            egui::Color32::WHITE,
                            name
                        );
                    });
                });
        }
        
        // Draw skin vertex labels using the same approach
        let skin_vertex_label_data: Vec<_> = mesh_data.skin_vertex_positions.iter()
            .filter_map(|(position, _, id, _)| {
                // Only show labels for vertices that have an ID
                let id = id.as_ref()?;
                // Extract just the last part of the ID (after the last dot)
                let name = id.split('.').last().unwrap_or(id);
                let screen_pos = camera.world_to_viewport(camera_transform, *position)?;
                Some((name.to_string(), screen_pos))
            })
            .collect();
            
        // Then render the skin vertex labels
        for (name, screen_pos) in skin_vertex_label_data {
            egui::Area::new(egui::Id::new(format!("skinvert_{}", name.as_str())))
                .fixed_pos(egui::pos2(screen_pos.x, screen_pos.y))
                .show(ctx, |ui| {
                    ui.horizontal(|ui| {
                        ui.colored_label(
                            egui::Color32::LIGHT_BLUE, // Use a different color to distinguish from bone labels
                            name
                        );
                    });
                });
        }
    }
}

fn export_to_glb(mesh_data: &MeshData, export_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    if mesh_data.skin_vertex_positions.is_empty() {
        return Err("No vertices to export".into());
    }

    println!("Starting GLB export to {:?}", export_path);
    
    // Create buffer for vertex data
    let mut buffer_data = Vec::new();
    
    // Add vertices
    let _positions_view_offset = 0;
    for (position, _, _, _) in &mesh_data.skin_vertex_positions {
        buffer_data.extend_from_slice(bytemuck::cast_slice(&[position.x, position.y, position.z]));
    }
    let positions_view_length = buffer_data.len();
    
    // Add vertex colors
    let colors_view_offset = buffer_data.len();
    for (_, _, _, color) in &mesh_data.skin_vertex_positions {
        buffer_data.extend_from_slice(bytemuck::cast_slice(&[color[0], color[1], color[2], color[3]]));
    }
    let colors_view_length = buffer_data.len() - colors_view_offset;
    
    // Add indices for triangles
    let indices_view_offset = buffer_data.len();
    
    // Create a mapping of vertex positions to their indices
    let mut vertex_to_index = HashMap::new();
    for (i, (pos, _, _, _)) in mesh_data.skin_vertex_positions.iter().enumerate() {
        // Use rounded values for position to handle float comparison issues
        let key = format!("{:.6},{:.6},{:.6}", pos.x, pos.y, pos.z);
        vertex_to_index.insert(key, i as u16);
    }
    
    let mut indices_data = Vec::new();
    
    // If we have defined triangles, use them
    if !mesh_data.triangles.is_empty() {
        println!("Using {} defined triangles from skin vertices", mesh_data.triangles.len());
        
        for (_, vertices) in &mesh_data.triangles {
            for vert in vertices {
                let key = format!("{:.6},{:.6},{:.6}", vert.x, vert.y, vert.z);
                if let Some(&index) = vertex_to_index.get(&key) {
                    indices_data.push(index);
                } else {
                    // If vertex not found, add it
                    let new_index = vertex_to_index.len() as u16;
                    vertex_to_index.insert(key, new_index);
                    
                    // Also add its data to the buffer
                    buffer_data.extend_from_slice(bytemuck::cast_slice(&[vert.x, vert.y, vert.z]));
                    buffer_data.extend_from_slice(bytemuck::cast_slice(&[0.2f32, 0.8f32, 0.2f32, 1.0f32])); // Green color
                }
            }
        }
    } else {
        // If no defined triangles, create simple point cloud
        println!("No triangles defined, creating point cloud");
        for i in 0..mesh_data.skin_vertex_positions.len() as u16 {
            indices_data.push(i);
        }
    }
    
    buffer_data.extend_from_slice(bytemuck::cast_slice(&indices_data));
    let indices_view_length = buffer_data.len() - indices_view_offset;
    
    // Pad buffer to 4-byte alignment
    while buffer_data.len() % 4 != 0 {
        buffer_data.push(0);
    }
    
    // Create a JSON structure for the GLB
    let mut root = json::Root::default();
    root.asset.version = "2.0".to_string();
    root.asset.generator = Some("Hooded Crow Modeller".to_string());
    
    // Create buffer and get vertex and index data
    let buffer = json::Buffer {
        byte_length: USize64(buffer_data.len() as u64),
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        uri: None,
    };
    root.buffers.push(buffer);

    // Create buffer views
    let positions_view = json::buffer::View {
        buffer: json::Index::new(0),
        byte_length: USize64(positions_view_length as u64),
        byte_offset: Some(USize64(0)),
        byte_stride: Some(Stride(12)),
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        target: Some(Checked::Valid(json::buffer::Target::ArrayBuffer)),
    };
    let positions_view_index = root.buffer_views.len();
    root.buffer_views.push(positions_view);

    let colors_view = json::buffer::View {
        buffer: json::Index::new(0),
        byte_length: USize64(colors_view_length as u64),
        byte_offset: Some(USize64(positions_view_length as u64)),
        byte_stride: Some(Stride(16)),
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        target: Some(Checked::Valid(json::buffer::Target::ArrayBuffer)),
    };
    let colors_view_index = root.buffer_views.len();
    root.buffer_views.push(colors_view);

    let indices_view = json::buffer::View {
        buffer: json::Index::new(0),
        byte_length: USize64(indices_view_length as u64),
        byte_offset: Some(USize64((positions_view_length + colors_view_length) as u64)),
        byte_stride: None,
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        target: Some(Checked::Valid(json::buffer::Target::ElementArrayBuffer)),
    };
    let indices_view_index = root.buffer_views.len();
    root.buffer_views.push(indices_view);

    // Create accessors
    let positions_accessor = json::Accessor {
        buffer_view: Some(json::Index::new(positions_view_index as u32)),
        byte_offset: Some(USize64(0)),
        count: USize64(mesh_data.skin_vertex_positions.len() as u64),
        component_type: Checked::Valid(json::accessor::GenericComponentType(
            json::accessor::ComponentType::F32,
        )),
        extensions: Default::default(),
        extras: Default::default(),
        type_: Checked::Valid(json::accessor::Type::Vec3),
        min: Some(json::Value::from(vec![-1.0, -1.0, -1.0])),
        max: Some(json::Value::from(vec![1.0, 1.0, 1.0])),
        name: None,
        normalized: false,
        sparse: None,
    };
    let positions_accessor_index = root.accessors.len();
    root.accessors.push(positions_accessor);

    let colors_accessor = json::Accessor {
        buffer_view: Some(json::Index::new(colors_view_index as u32)),
        byte_offset: Some(USize64(0)),
        count: USize64(mesh_data.skin_vertex_positions.len() as u64),
        component_type: Checked::Valid(json::accessor::GenericComponentType(
            json::accessor::ComponentType::F32,
        )),
        extensions: Default::default(),
        extras: Default::default(),
        type_: Checked::Valid(json::accessor::Type::Vec4),
        min: None,
        max: None,
        name: None,
        normalized: false,
        sparse: None,
    };
    let colors_accessor_index = root.accessors.len();
    root.accessors.push(colors_accessor);

    let indices_accessor = json::Accessor {
        buffer_view: Some(json::Index::new(indices_view_index as u32)),
        byte_offset: Some(USize64(0)),
        count: USize64(indices_data.len() as u64),
        component_type: Checked::Valid(json::accessor::GenericComponentType(
            json::accessor::ComponentType::U16,
        )),
        extensions: Default::default(),
        extras: Default::default(),
        type_: Checked::Valid(json::accessor::Type::Scalar),
        min: None,
        max: None,
        name: None,
        normalized: false,
        sparse: None,
    };
    let indices_accessor_index = root.accessors.len();
    root.accessors.push(indices_accessor);

    // Create primitive
    let primitive = json::mesh::Primitive {
        attributes: {
            let mut map = BTreeMap::new();
            map.insert(
                Checked::Valid(json::mesh::Semantic::Positions),
                json::Index::new(positions_accessor_index as u32),
            );
            map.insert(
                Checked::Valid(json::mesh::Semantic::Colors(0)),
                json::Index::new(colors_accessor_index as u32),
            );
            map
        },
        extensions: Default::default(),
        extras: Default::default(),
        indices: Some(json::Index::new(indices_accessor_index as u32)),
        material: None,
        mode: Checked::Valid(json::mesh::Mode::Triangles),
        targets: None,
    };

    // Create mesh
    let mesh = json::Mesh {
        extensions: Default::default(),
        extras: Default::default(),
        name: Some("Bone Mesh".to_string()),
        primitives: vec![primitive],
        weights: None,
    };
    let mesh_index = root.meshes.len();
    root.meshes.push(mesh);

    // Create node
    let node = json::Node {
        camera: None,
        children: None,
        extensions: Default::default(),
        extras: Default::default(),
        matrix: None,
        mesh: Some(json::Index::new(mesh_index as u32)),
        name: Some("Bone Node".to_string()),
        rotation: None,
        scale: None,
        skin: None,
        translation: None,
        weights: None,
    };
    let node_index = root.nodes.len();
    root.nodes.push(node);

    // Create scene
    let scene = json::Scene {
        extensions: Default::default(),
        extras: Default::default(),
        name: Some("Bone Scene".to_string()),
        nodes: vec![json::Index::new(node_index as u32)],
    };
    root.scenes.push(scene);
    root.scene = Some(json::Index::new(0));

    // Write GLB file
    let file = std::fs::File::create(export_path)?;
    let mut writer = std::io::BufWriter::new(file);

    // Convert JSON to string
    let json_string = serde_json::to_string(&root)?;
    let json_bytes = json_string.as_bytes();
    let json_length = json_bytes.len();
    let aligned_json_length = align_to_4(json_length);
    let padding_bytes = vec![0u8; aligned_json_length - json_length];

    // Write GLB header
    writer.write_all(b"glTF")?;  // Magic
    write_u32(&mut writer, 2)?;  // Version
    write_u32(&mut writer, (
        12 +                     // Header
        8 + aligned_json_length + // JSON chunk
        8 + buffer_data.len()    // BIN chunk
    ) as u32)?;

    // Write JSON chunk
    write_u32(&mut writer, json_length as u32)?;
    writer.write_all(b"JSON")?;
    writer.write_all(json_bytes)?;
    writer.write_all(&padding_bytes)?;

    // Write BIN chunk
    write_u32(&mut writer, buffer_data.len() as u32)?;
    writer.write_all(b"BIN\0")?;
    writer.write_all(&buffer_data)?;

    writer.flush()?;
    Ok(())
}

fn write_u32<W: Write>(writer: &mut W, value: u32) -> std::io::Result<()> {
    writer.write_all(&value.to_le_bytes())
}

fn align_to_4(n: usize) -> usize {
    (n + 3) & !3
}

fn hsv_to_rgb(h: f32, s: f32, v: f32) -> (f32, f32, f32) {
    let h = h * 6.0;
    let i = h.floor();
    let f = h - i;
    let p = v * (1.0 - s);
    let q = v * (1.0 - (s * f));
    let t = v * (1.0 - (s * (1.0 - f)));

    match i as i32 % 6 {
        0 => (v, t, p),
        1 => (q, v, p),
        2 => (p, v, t),
        3 => (p, q, v),
        4 => (t, p, v),
        _ => (v, p, q),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <mesh_file> [--export <export_path>]", args[0]);
        process::exit(1);
    }

    let mesh_file = PathBuf::from(&args[1]);
    let export_path = if args.len() >= 4 && args[2] == "--export" {
        Some(PathBuf::from(&args[3]))
    } else {
        None
    };

    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(EguiPlugin)
        .insert_resource(MeshFile(mesh_file))
        .insert_resource(if let Some(path) = export_path {
            ExportPath(path)
        } else {
            ExportPath(PathBuf::from("output.glb"))
        })
        .add_event::<FileChangedEvent>()  // Register the event
        .insert_resource(TriangleMeshHandles::default())
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (
                monitor_file,
                handle_file_changes,
                keyboard_input,
                mouse_motion,
                mouse_wheel,
                mouse_button_input,
                update_camera,
                export_system,
                update_ui,
                update_triangle_meshes,
            ).chain(),
        )
        .add_systems(PostUpdate, (draw_bones, draw_triangles, draw_axes))
        .run();
}