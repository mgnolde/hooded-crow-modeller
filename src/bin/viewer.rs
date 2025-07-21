use std::f32::consts::PI;

use bevy::{
    prelude::*,
    render::camera::Camera,
    input::{
        mouse::{MouseMotion, MouseWheel, MouseButton},
        keyboard::{KeyCode, KeyboardInput},
    },
    pbr::{AlphaMode, StandardMaterial, wireframe::Wireframe},
    render::mesh::{Indices, PrimitiveTopology},
    render::render_asset::RenderAssetUsages,
    window::PrimaryWindow,
    gltf::Gltf,
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

#[derive(Clone, Debug, PartialEq)]
enum CameraMode {
    Free,  // Free camera movement
    Orbit, // Orbit around target
}

#[derive(Resource, Clone, Debug)]
struct CameraState {
    default_position: Vec3,
    default_target: Vec3,
    mode: CameraMode,
}

impl Default for CameraState {
    fn default() -> Self {
        Self {
            default_position: Vec3::new(5.0, 5.0, 5.0),
            default_target: Vec3::ZERO,
            mode: CameraMode::Free,
        }
    }
}

#[derive(Resource, Clone, Debug)]
struct ModelRotation {
    rotation: Quat,
}

impl Default for ModelRotation {
    fn default() -> Self {
        Self {
            rotation: Quat::IDENTITY,
        }
    }
}
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
    triangles: HashMap<String, ([Vec3; 3], [f32; 4])>, // Store triangles as (name, ([vertex positions], color))
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

#[derive(Resource, Clone, Debug, PartialEq)]
enum TriangleOutlineMode {
    Black,    // Use black outline
    Matching, // Use same color as fill
}

#[derive(Resource)]
struct VisualizationSettings {
    show_skin_vertices: bool,
    show_triangles: bool,
    show_mesh: bool,
    visualization_mode: BoneVisualization,
    line_width: f32,
    triangle_outline: TriangleOutlineMode,
}

impl Default for VisualizationSettings {
    fn default() -> Self {
        let settings = Self {
            show_skin_vertices: true,
            show_triangles: true, // Always show triangles by default for debugging
            show_mesh: true,
            visualization_mode: BoneVisualization::Solid,
            line_width: 5.0,
            triangle_outline: TriangleOutlineMode::Black,
        };
        println!("Created default visualization settings: show_triangles={}", settings.show_triangles);
        settings
    }
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
    look_at_target: Vec3,
    movement_speed: f32,
    zoom_speed: f32,
    rotation_speed: f32,
}

impl Default for CameraController {
    fn default() -> Self {
        Self {
            orbit_sensitivity: 1.0,
            pan_sensitivity: 0.002,
            zoom_sensitivity: 0.5,
            orbit_button: MouseButton::Left,
            pan_button: MouseButton::Right,
            look_at_target: Vec3::ZERO,
            movement_speed: 5.0,
            zoom_speed: 2.0,
            rotation_speed: 1.0,
        }
    }
}

#[derive(Resource)]
struct ShowAxes(bool);

#[derive(Resource)]
struct LastModified(SystemTime);

#[derive(Resource)]
struct TrianglesLastUpdated(SystemTime);

#[derive(Resource, Clone)]
struct TemplateModel {
    path: PathBuf,
    scale: f32,
    rotation: Vec3, // Euler angles in degrees
    offset: Vec3, // Translation offset
    transparency: f32,
}

#[derive(Resource)]
struct QuickExit {
    timer: f32,
    duration: f32,
}

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
        // Reordered: first orientation, then slope (vertical), then rotation
        return Mat4::from_rotation_z(rotation_rad) * Mat4::from_rotation_x(-PI/2.0) * Mat4::from_rotation_y(orientation_rad);
    } else if (slope + 90.0).abs() < 0.001 {
        // For slope = -90° (pointing down), create a transform that points directly down
        // Reordered: first orientation, then slope (vertical), then rotation
        return Mat4::from_rotation_z(rotation_rad) * Mat4::from_rotation_x(PI/2.0) * Mat4::from_rotation_y(orientation_rad);
    } else {
        // For non-vertical slopes, create a transform that:
        // 1. Points along +Z at 0 orientation
        // 2. Rotates by orientation around Y (positive = towards +X) - FIRST
        // 3. Tilts up/down by slope around the local X axis - SECOND
        // 4. Applies final rotation around the bone's axis - THIRD
        let orientation_rot = Mat4::from_rotation_y(orientation_rad);
        let slope_rot = Mat4::from_rotation_x(-slope_rad); // Invert sign so positive slopes point up
        let rotation_rot = Mat4::from_rotation_z(rotation_rad);
        
        // Apply transformations in logical order: orientation first, then slope, then rotation
        rotation_rot * slope_rot * orientation_rot
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
    settings: Option<Res<VisualizationSettings>>,
) {
    for event in file_events.read() {
        let model = &event.model;
        println!("*** File changed! Reloading model... ***");
        println!("*** EVENT DEBUG: About to process file change event ***");
        
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
        let mut triangles: HashMap<String, ([Vec3; 3], [f32; 4])> = HashMap::new();
        let mut triangle_colors: HashMap<String, [f32; 4]> = HashMap::new();
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
                    
                    // Use the bone's color or default (triangle colors are defined separately)
                    let color = bone.color.unwrap_or_else(|| [0.5, 0.5, 0.5, 0.5]);
                    
                    // Store the vertex position and ID
                    let id = skin_vert.id.clone().unwrap_or_else(|| full_path.clone());
                    
                    // Extract RGB components for the vertex color (for rendering)
                    let rgb_color = [color[0], color[1], color[2]];
                    
                    // Add skin vertex to our collection with the correct colors
                    skin_vertex_positions.push((position, rgb_color, Some(id.clone()), color));
                    skin_vertex_colors.push(color);
                    // println!("PROCESSING: Added skin vertex at {:?} for bone '{}' with id {:?}", position, full_path, id);
                }
            }
        }

        println!("Loaded skin vertex colors from TOML: {:?}", skin_vertex_colors);

        // Create triangles directly from bone triangle definitions
        println!("\n===== TRIANGLE COLLECTION DEBUGGING =====\n");
        println!("TRIANGLE DEBUG: Creating triangles directly from bone definitions");
        
        // Create a map of vertex IDs to positions for faster lookup
        let vertex_id_map: HashMap<String, (Vec3, [f32; 4])> = skin_vertex_positions.iter()
            .filter_map(|(pos, _, id, color)| {
                id.as_ref().map(|id| (id.clone(), (*pos, *color)))
            })
            .collect();

        println!("TRIANGLE DEBUG: Built vertex map with {} vertices", vertex_id_map.len());
        for (id, (pos, _)) in &vertex_id_map {
            println!("TRIANGLE DEBUG: Vertex '{}' at {:?}", id, pos);
        }
        
        // Process triangles directly from bones
        for (bone_name, bone) in &all_bones {
            println!("TRIANGLE DEBUG: Processing bone '{}' with {} triangle_defs", bone_name, bone.triangle_defs.len());
            
            for (tri_name, tri_def) in &bone.triangle_defs {
                println!("TRIANGLE DEBUG: Processing triangle '{}' with vertices: {:?}", tri_name, tri_def.verts);
                
                if tri_def.verts.len() == 3 {
                    let mut triangle_verts = [Vec3::ZERO; 3];
                    let mut all_found = true;
                    
                    for (i, vertex_id) in tri_def.verts.iter().enumerate() {
                        if let Some((pos, _)) = vertex_id_map.get(vertex_id) {
                            triangle_verts[i] = *pos;
                            println!("TRIANGLE DEBUG: Found vertex '{}' at {:?}", vertex_id, pos);
                        } else {
                            println!("TRIANGLE DEBUG: ERROR: Vertex '{}' not found in vertex map", vertex_id);
                            all_found = false;
                            break;
                        }
                    }
                    
                    if all_found {
                        let full_name = format!("{}.{}", bone_name, tri_name);
                        let triangle_color = tri_def.color.unwrap_or([0.7, 0.7, 0.7, 0.5]);
                        triangles.insert(full_name.clone(), (triangle_verts, triangle_color));
                        triangle_colors.insert(full_name.clone(), triangle_color);
                        println!("TRIANGLE DEBUG: Added triangle '{}' with vertices: {:?}, color: {:?}", full_name, triangle_verts, triangle_color);
                    }
                }
            }
        }
        
        println!("TRIANGLE DEBUG: Added {} triangles to MeshData", triangles.len());
        
        // Print detailed debug info about triangles
        for (tri_name, (tri_vertices, color)) in &triangles {
            println!("TRIANGLE MESHDATA: '{}' has vertices: [{:?}, {:?}, {:?}], color: {:?}", 
                tri_name, tri_vertices[0], tri_vertices[1], tri_vertices[2], color);
        }
        
        // Then try the old method (recalculating from vertices)
        // Print the skin vertex positions and IDs that were loaded
        println!("DEBUG: Loaded {} skin vertices with these IDs:", skin_vertex_positions.len());
        for (i, (pos, _, id, color)) in skin_vertex_positions.iter().enumerate() {
            if let Some(id) = id {
                println!("  Vertex {}: ID='{}', pos={:?}", i, id, pos);
            }
        }

        // Create a map of vertex IDs to positions and colors for faster lookup
        let vertex_id_map: HashMap<String, (Vec3, [f32; 4])> = skin_vertex_positions.iter()
            .filter_map(|(pos, _, id, color)| {
                id.as_ref().map(|id| {
                    // Explicitly check if this is one of our specific triangle vertices from example.toml
                    let fixed_color = if id == "ls1" || id == "ls2" || id == "ls3" {
                        [0.0, 0.0, 1.0, 0.5] // Force blue color for the triangle vertices
                    } else {
                        *color
                    };
                    (id.clone(), (*pos, fixed_color))
                })
            })
            .collect();

        // Get triangle definitions from the model - only for debugging purposes
        let bones = model.get_bones();
        
        for (bone_name, bone) in &bones {
            for skin_vert in &bone.skin_verts {
                // For each triangle this vertex is part of
                for (face_name, position_in_triangle) in &skin_vert.triangles {
                    // Get the ID for this vertex
                    let vertex_id = skin_vert.id.clone().unwrap_or_else(|| format!("{}_{}", bone_name, position_in_triangle));
                    
                    // Create a face entry if it doesn't exist
                    if !triangles.contains_key(face_name) {
                        triangles.insert(face_name.clone(), ([Vec3::ZERO, Vec3::ZERO, Vec3::ZERO], [0.7, 0.7, 0.7, 0.5]));
                    }
                    
                    // Set the vertex position and color in the triangle
                    if let Some((position, color)) = vertex_id_map.get(&vertex_id) {
                        if let Some((triangle_verts, triangle_color)) = triangles.get_mut(face_name) {
                            // Update the vertex at its position in the triangle
                            if *position_in_triangle < 3 {
                                triangle_verts[*position_in_triangle] = *position;
                                
                                // For all vertices, store the color for the triangle
                                *triangle_color = *color;
                                triangle_colors.insert(face_name.clone(), *color);
                                println!("Assigning color {:?} to triangle {}", *color, face_name);
                            }
                        }
                    }
                }
            }

            // Print the vertex_id_map keys so we can verify what IDs are available
            println!("DEBUG: vertex_id_map contains {} IDs: {:?}", vertex_id_map.len(), vertex_id_map.keys().collect::<Vec<_>>());
            
            // IMPORTANT: We no longer process triangles here since we now use model.collect_triangles() exclusively
            // This avoids the duplicate triangle processing that was causing issues
            println!("DEBUG: Bone {} has {} triangle definitions (which were already processed via model.collect_triangles())", 
                     bone_name, bone.triangle_defs.len());
        }

        // Store triangle colors in skin_vertex_colors in the same order as triangles
        skin_vertex_colors.clear();
        for (name, _) in &triangles {
            // Use the color directly from the triangle_colors map
            let triangle_color = triangle_colors
                .get(name)
                .copied()
                .unwrap_or([0.0, 0.0, 1.0, 0.5]); // Default to blue with 50% opacity
            
            skin_vertex_colors.push(triangle_color);
        }
        
        // Debug print what triangles we've collected
        println!("TRIANGLES:");
        for (name, (verts, color)) in &triangles {
            println!("Triangle {}: {:?}, {:?}, {:?}, color: {:?}", name, verts[0], verts[1], verts[2], color);
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
        
        println!("FINAL MESHDATA: {} triangles stored in MeshData", mesh_data.triangles.len());
        for (name, (verts, color)) in &mesh_data.triangles {
            println!("FINAL TRIANGLE: '{}' vertices: [{:?}, {:?}, {:?}], color: {:?}", name, verts[0], verts[1], verts[2], color);
        }
        
        println!("INITIAL TRIANGLES: {}", mesh_data.triangles.len());
        for (name, (verts, color)) in &mesh_data.triangles {
            println!("TRIANGLE in MeshData: {} - {:?}, {:?}, {:?}, color: {:?}", name, verts[0], verts[1], verts[2], color);
        }

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

        // Debug print before inserting resources
        println!("[RESOURCE INSERT] About to insert MeshData with {} triangles", mesh_data.triangles.len());
        
        // Insert resources with explicit debug
        commands.insert_resource(mesh_data.clone());
        println!("[RESOURCE INSERT] Inserted MeshData resource");
        commands.insert_resource(ShowAxes(true));
        // Get the current settings to preserve user preferences if available
        let triangle_outline = if let Some(settings) = settings.as_ref() {
            settings.triangle_outline.clone()
        } else {
            TriangleOutlineMode::Black // Default if no settings exist yet
        };
        
        commands.insert_resource(VisualizationSettings {
            visualization_mode: BoneVisualization::Solid,
            line_width: 5.0,
            show_triangles: true,
            show_mesh: true,
            show_skin_vertices: true,
            triangle_outline, // Use preserved or default setting
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
        show_skin_vertices: true,
        show_triangles: true,
        show_mesh: true,
        visualization_mode: BoneVisualization::Solid,
        line_width: 5.0,
        triangle_outline: TriangleOutlineMode::Black, // Default to black outlines
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
    let mut triangles: HashMap<String, ([Vec3; 3], [f32; 4])> = HashMap::new();
    let mut skin_vertex_colors = Vec::new();

    let max_depth = positions.keys()
        .map(|path| get_bone_depth(path))
        .max()
        .unwrap_or(0);

    let all_bones = model.get_bones();
    for (full_path, bone) in &all_bones {
        // println!("SETUP: Processing bone {} with {} skin verts", full_path, bone.skin_verts.len());
        
        // Store bone position and color
        if let Some((start, end)) = positions.get(full_path.as_str()) {
            let bone_depth = get_bone_depth(full_path.as_str());
            let color = get_rainbow_color(bone_depth, max_depth).0;
            positions_with_colors.insert(full_path.clone(), (*start, *end, color));
            
            for skin_vert in &bone.skin_verts {
                // Calculate position of skin vertex using the bone's start and end
                let position = skin_vert.calculate_position(*start, *end, bone.resolved_rotation);
                
                // Use the bone's rainbow color for skin vertices
                let bone_depth = get_bone_depth(full_path.as_str());
                let bone_color = get_rainbow_color(bone_depth, max_depth).0;
                let color = [bone_color[0], bone_color[1], bone_color[2], 1.0];
                
                // Store the vertex position and ID
                let id = skin_vert.id.clone().unwrap_or_else(|| full_path.clone());
                
                // Extract RGB components for the vertex color (for rendering)
                let rgb_color = [color[0], color[1], color[2]];
                
                // Add skin vertex to our collection with the correct colors
                skin_vertex_positions.push((position, rgb_color, Some(id.clone()), color));
                skin_vertex_colors.push(color);
                
                // Debug print the color
                println!("Setup: Vertex color for {}: {:?}", id, color);
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
    
    // Create a map of vertex IDs to positions and colors for faster lookup
    let vertex_id_map: HashMap<String, (Vec3, [f32; 4])> = skin_vertex_positions.iter()
        .filter_map(|(pos, _, id, color)| {
            id.as_ref().map(|id| (id.clone(), (*pos, *color)))
        })
        .collect();

    println!("[SETUP] TRIANGLE DEBUG: Built vertex map with {} vertices", vertex_id_map.len());
    for (id, (pos, _)) in &vertex_id_map {
        println!("[SETUP] TRIANGLE DEBUG: Vertex '{}' at {:?}", id, pos);
    }
    
    // Process triangles directly from bones (same logic as file change handler)
    for (bone_name, bone) in &all_bones {
        println!("[SETUP] TRIANGLE DEBUG: Processing bone '{}' with {} triangle_defs", bone_name, bone.triangle_defs.len());
        
        for (tri_name, tri_def) in &bone.triangle_defs {
            println!("[SETUP] TRIANGLE DEBUG: Processing triangle '{}' with vertices: {:?}", tri_name, tri_def.verts);
            
            if tri_def.verts.len() == 3 {
                let mut triangle_verts = [Vec3::ZERO; 3];
                let mut all_found = true;
                
                for (i, vertex_id) in tri_def.verts.iter().enumerate() {
                    if let Some((pos, _)) = vertex_id_map.get(vertex_id) {
                        triangle_verts[i] = *pos;
                        println!("[SETUP] TRIANGLE DEBUG: Found vertex '{}' at {:?}", vertex_id, pos);
                    } else {
                        println!("[SETUP] TRIANGLE DEBUG: ERROR: Vertex '{}' not found in vertex map", vertex_id);
                        all_found = false;
                        break;
                    }
                }
                
                if all_found {
                    let full_name = format!("{}.{}", bone_name, tri_name);
                    let triangle_color = tri_def.color.unwrap_or([0.7, 0.7, 0.7, 0.5]);
                    triangles.insert(full_name.clone(), (triangle_verts, triangle_color));
                    println!("[SETUP] TRIANGLE DEBUG: Added triangle '{}' with vertices: {:?}, color: {:?}", full_name, triangle_verts, triangle_color);
                }
            }
        }
    }

    // Process triangles from the skin vertices (legacy method)
    let mut triangle_colors: HashMap<String, [f32; 4]> = HashMap::new();
    
    for (bone_name, bone) in &all_bones {
        for skin_vert in &bone.skin_verts {
            // For each triangle this vertex is part of
            for (face_name, position_in_triangle) in &skin_vert.triangles {
                // Get the ID for this vertex
                let vertex_id = if let Some(id) = &skin_vert.id {
                    id.clone()
                } else {
                    format!("{}_{}" ,bone_name, position_in_triangle)
                };
                
                // Create a face entry if it doesn't exist
                if !triangles.contains_key(face_name) {
                    triangles.insert(face_name.clone(), ([Vec3::ZERO, Vec3::ZERO, Vec3::ZERO], [0.7, 0.7, 0.7, 0.5]));
                }
                
                // Set the vertex position and color in the triangle
                if let Some((position, color)) = vertex_id_map.get(&vertex_id) {
                    if let Some((triangle_verts, triangle_color)) = triangles.get_mut(face_name) {
                        // Update the vertex at its position in the triangle
                        if *position_in_triangle < 3 {
                            triangle_verts[*position_in_triangle] = *position;
                            
                            // If this is the first vertex (position 0), store its color for the whole triangle
                            if *position_in_triangle == 0 {
                                *triangle_color = *color;
                                triangle_colors.insert(face_name.clone(), *color);
                            }
                        }
                    }
                }
            }
        }
    }

    // Store triangle colors in skin_vertex_colors in the same order as triangles
    for (name, (vertices, color)) in &triangles {
        // Use the triangle's own color
        skin_vertex_colors.push(*color);
    }
    
    // Debug output for triangles
    println!("INITIAL TRIANGLES:");
    for (name, (verts, color)) in &triangles {
        println!("Triangle {}: {:?}, {:?}, {:?}, color: {:?}", name, verts[0], verts[1], verts[2], color);
    }

    mesh_data.skin_vertex_positions = skin_vertex_positions;
    mesh_data.triangles = triangles;
    mesh_data.skin_vertex_colors = skin_vertex_colors;

    println!("[SETUP] FINAL MESHDATA: {} triangles stored in MeshData", mesh_data.triangles.len());
    for (name, (verts, color)) in &mesh_data.triangles {
        println!("[SETUP] FINAL TRIANGLE: '{}' vertices: [{:?}, {:?}, {:?}], color: {:?}", name, verts[0], verts[1], verts[2], color);
    }

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
    mut camera_query: Query<(&mut Transform, &mut CameraController), With<Camera>>,
    mut model_rotation: ResMut<ModelRotation>,
    mut camera_state: ResMut<CameraState>,
    mesh_data: Option<Res<MeshData>>,
) {
    let (mut transform, mut controller) = camera_query.single_mut();
    let delta = time.delta_seconds();

    // Shift modifier - Faster movement
    let speed_multiplier = if keyboard.pressed(KeyCode::ShiftLeft) || keyboard.pressed(KeyCode::ShiftRight) {
        3.0 // 3x faster when holding Shift
    } else {
        1.0
    };

    // UNIFIED CONTROL SYSTEM:
    // Arrow keys (no modifier) - Model rotation around look_at_target
    // CTRL + keys - Camera movement  
    // ALT + keys - Look-at target movement
    
    let ctrl_pressed = keyboard.pressed(KeyCode::ControlLeft) || keyboard.pressed(KeyCode::ControlRight);
    let alt_pressed = keyboard.pressed(KeyCode::AltLeft) || keyboard.pressed(KeyCode::AltRight);
    
    
    
    
    
    // EXPLICIT CONTROL HANDLING - Test each key with exact conditions
    
    // CTRL + Arrow keys = Camera movement
    // Flag to track if we're doing camera movement (to completely disable model rotation)
    let mut camera_movement_active = false;
    
    // FIRST-PERSON FLY-THROUGH CAMERA CONTROLS
    // WASD + QE keys for camera movement relative to camera orientation
    if keyboard.pressed(KeyCode::KeyW) || keyboard.pressed(KeyCode::KeyA) || keyboard.pressed(KeyCode::KeyS) || keyboard.pressed(KeyCode::KeyD) || keyboard.pressed(KeyCode::KeyQ) || keyboard.pressed(KeyCode::KeyE) {
        camera_movement_active = true;
    }
    
    if keyboard.pressed(KeyCode::KeyW) {
        // W - Move forward in the direction the camera is looking
        let forward = transform.forward();
        transform.translation += forward * controller.movement_speed * speed_multiplier * delta * 1.0;
    }
    if keyboard.pressed(KeyCode::KeyS) {
        // S - Move backward (opposite to camera direction)
        let backward = -transform.forward();
        transform.translation += backward * controller.movement_speed * speed_multiplier * delta * 1.0;
    }
    if keyboard.pressed(KeyCode::KeyA) {
        // A - Strafe left (relative to camera orientation)
        let left = -transform.right();
        transform.translation += left * controller.movement_speed * speed_multiplier * delta * 1.0;
    }
    if keyboard.pressed(KeyCode::KeyD) {
        // D - Strafe right (relative to camera orientation)
        let right = transform.right();
        transform.translation += right * controller.movement_speed * speed_multiplier * delta * 1.0;
    }
    if keyboard.pressed(KeyCode::KeyQ) {
        // Q - Move down in world space (not relative to camera)
        transform.translation -= Vec3::Y * controller.movement_speed * speed_multiplier * delta * 1.0;
    }
    if keyboard.pressed(KeyCode::KeyE) {
        // E - Move up in world space (not relative to camera)
        transform.translation += Vec3::Y * controller.movement_speed * speed_multiplier * delta * 1.0;
    }
    
    // ARROW KEYS for camera rotation (look around)
    let look_sensitivity = 0.8; // degrees per frame - reduced for smoother control
    if keyboard.pressed(KeyCode::ArrowUp) {
        // Arrow Up - Look up
        let rotation = Quat::from_rotation_x(-look_sensitivity * delta);
        transform.rotate(rotation);
    }
    if keyboard.pressed(KeyCode::ArrowDown) {
        // Arrow Down - Look down  
        let rotation = Quat::from_rotation_x(look_sensitivity * delta);
        transform.rotate(rotation);
    }
    if keyboard.pressed(KeyCode::ArrowLeft) {
        // Arrow Left - Look left
        let rotation = Quat::from_rotation_y(look_sensitivity * delta);
        transform.rotate(rotation);
    }
    if keyboard.pressed(KeyCode::ArrowRight) {
        // Arrow Right - Look right
        let rotation = Quat::from_rotation_y(-look_sensitivity * delta);
        transform.rotate(rotation);
    }
    
    
    // X/Z keys for camera rotation (moved from keyboard_input function)
    if keyboard.pressed(KeyCode::KeyX) {
        let forward: Vec3 = (*transform.forward()).into();
        let rotation = Quat::from_axis_angle(forward, controller.rotation_speed * delta);
        let origin = Vec3::ZERO;
        let distance = (transform.translation - origin).length();
        transform.rotate_around(origin, rotation);
        // Maintain distance from origin after rotation
        let dir = (transform.translation - origin).normalize();
        transform.translation = origin + dir * distance;
    }
    if keyboard.pressed(KeyCode::KeyZ) {
        let forward: Vec3 = (*transform.forward()).into();
        let rotation = Quat::from_axis_angle(forward, -controller.rotation_speed * delta);
        let origin = Vec3::ZERO;
        let distance = (transform.translation - origin).length();
        transform.rotate_around(origin, rotation);
        // Maintain distance from origin after rotation
        let dir = (transform.translation - origin).normalize();
        transform.translation = origin + dir * distance;
    }
    if keyboard.pressed(KeyCode::PageUp) && ctrl_pressed && !alt_pressed {
        // Ctrl+PgUp - Move camera forward
        let forward = transform.forward();
        transform.translation += forward * controller.movement_speed * speed_multiplier * delta;
    }
    if keyboard.pressed(KeyCode::PageDown) && ctrl_pressed && !alt_pressed {
        // Ctrl+PgDown - Move camera backward
        let backward = -transform.forward();
        transform.translation += backward * controller.movement_speed * speed_multiplier * delta;
    }
    
    // ALT + Arrow keys = Look-at target movement
    if keyboard.pressed(KeyCode::ArrowUp) && alt_pressed && !ctrl_pressed {
        // Alt+↑ - Move look-at target up
        controller.look_at_target.y += controller.movement_speed * speed_multiplier * delta;
        *transform = transform.looking_at(controller.look_at_target, Vec3::Y);
    }
    if keyboard.pressed(KeyCode::ArrowDown) && alt_pressed && !ctrl_pressed {
        // Alt+↓ - Move look-at target down
        controller.look_at_target.y -= controller.movement_speed * speed_multiplier * delta;
        *transform = transform.looking_at(controller.look_at_target, Vec3::Y);
    }
    if keyboard.pressed(KeyCode::ArrowLeft) && alt_pressed && !ctrl_pressed {
        // Alt+← - Move look-at target left
        controller.look_at_target.x -= controller.movement_speed * speed_multiplier * delta;
        *transform = transform.looking_at(controller.look_at_target, Vec3::Y);
    }
    if keyboard.pressed(KeyCode::ArrowRight) && alt_pressed && !ctrl_pressed {
        // Alt+→ - Move look-at target right
        controller.look_at_target.x += controller.movement_speed * speed_multiplier * delta;
        *transform = transform.looking_at(controller.look_at_target, Vec3::Y);
    }
    if keyboard.pressed(KeyCode::PageUp) && alt_pressed && !ctrl_pressed {
        // Alt+PgUp - Move look-at target forward (away from camera)
        controller.look_at_target.z += controller.movement_speed * speed_multiplier * delta;
        *transform = transform.looking_at(controller.look_at_target, Vec3::Y);
    }
    if keyboard.pressed(KeyCode::PageDown) && alt_pressed && !ctrl_pressed {
        // Alt+PgDown - Move look-at target backward (toward camera)
        controller.look_at_target.z -= controller.movement_speed * speed_multiplier * delta;
        *transform = transform.looking_at(controller.look_at_target, Vec3::Y);
    }
    
    // Arrow keys (no modifier) = Model rotation around look_at_target
    // IMPORTANT: Model rotation must be COMPLETELY DISABLED when camera movement is active
    
    // Re-check modifier keys to ensure they're current
    let ctrl_currently_pressed = keyboard.pressed(KeyCode::ControlLeft) || keyboard.pressed(KeyCode::ControlRight);
    let alt_currently_pressed = keyboard.pressed(KeyCode::AltLeft) || keyboard.pressed(KeyCode::AltRight);
    
    // NUMPAD KEYS for model rotation (separate from camera controls)
    if keyboard.pressed(KeyCode::Numpad8) {
        // Numpad 8 - Rotate model forward around X-axis (pitch)
        let rotation = Quat::from_rotation_x(-controller.rotation_speed * speed_multiplier * delta);
        model_rotation.rotation = rotation * model_rotation.rotation;
    }
    if keyboard.pressed(KeyCode::Numpad2) {
        // Numpad 2 - Rotate model backward around X-axis (pitch)
        let rotation = Quat::from_rotation_x(controller.rotation_speed * speed_multiplier * delta);
        model_rotation.rotation = rotation * model_rotation.rotation;
    }
    if keyboard.pressed(KeyCode::Numpad4) {
        // Numpad 4 - Rotate model left around Y-axis (yaw)
        let rotation = Quat::from_rotation_y(controller.rotation_speed * speed_multiplier * delta);
        model_rotation.rotation = rotation * model_rotation.rotation;
    }
    if keyboard.pressed(KeyCode::Numpad6) {
        // Numpad 6 - Rotate model right around Y-axis (yaw)
        let rotation = Quat::from_rotation_y(-controller.rotation_speed * speed_multiplier * delta);
        model_rotation.rotation = rotation * model_rotation.rotation;
    }
    if keyboard.pressed(KeyCode::Numpad7) {
        // Numpad 7 - Rotate model around Z-axis (roll left)
        let rotation = Quat::from_rotation_z(controller.rotation_speed * speed_multiplier * delta);
        model_rotation.rotation = rotation * model_rotation.rotation;
    }
    if keyboard.pressed(KeyCode::Numpad9) {
        // Numpad 9 - Rotate model around Z-axis (roll right)
        let rotation = Quat::from_rotation_z(-controller.rotation_speed * speed_multiplier * delta);
        model_rotation.rotation = rotation * model_rotation.rotation;
    }
    if keyboard.pressed(KeyCode::PageUp) && !ctrl_currently_pressed && !alt_currently_pressed {
        // PgUp - Rotate model around Z-axis (roll)
        let rotation = Quat::from_rotation_z(controller.rotation_speed * speed_multiplier * delta);
        model_rotation.rotation = rotation * model_rotation.rotation;
    }
    if keyboard.pressed(KeyCode::PageDown) && !ctrl_currently_pressed && !alt_currently_pressed {
        // PgDown - Rotate model around Z-axis (roll, opposite)
        let rotation = Quat::from_rotation_z(-controller.rotation_speed * speed_multiplier * delta);
        model_rotation.rotation = rotation * model_rotation.rotation;
    }

    // ZOOM (NumPad +/- and +/- keys only, no Page Up/Down to avoid conflicts)
    // Note: + key requires checking both Equal key and Shift modifier
    let plus_pressed = keyboard.pressed(KeyCode::Equal) && 
                      (keyboard.pressed(KeyCode::ShiftLeft) || keyboard.pressed(KeyCode::ShiftRight));
    if keyboard.pressed(KeyCode::NumpadAdd) || plus_pressed {
        // Zoom in
        let forward = (controller.look_at_target - transform.translation).normalize();
        transform.translation += forward * controller.zoom_speed * speed_multiplier * delta;
    }
    if keyboard.pressed(KeyCode::NumpadSubtract) || keyboard.pressed(KeyCode::Minus) {
        // Zoom out
        let forward = (controller.look_at_target - transform.translation).normalize();
        transform.translation -= forward * controller.zoom_speed * speed_multiplier * delta;
    }

    // R key - Reset camera to default position and orientation
    if keyboard.just_pressed(KeyCode::KeyR) {
        transform.translation = camera_state.default_position;
        controller.look_at_target = camera_state.default_target;
        model_rotation.rotation = Quat::IDENTITY;
        
        // Reset camera orientation to look at the target
        *transform = transform.looking_at(camera_state.default_target, Vec3::Y);
    }
    
    // F key - Focus/frame the model
    if keyboard.just_pressed(KeyCode::KeyF) {
        if let Some(mesh_data) = mesh_data.as_ref() {
            // Calculate model bounds
            let mut min_bounds = Vec3::splat(f32::INFINITY);
            let mut max_bounds = Vec3::splat(f32::NEG_INFINITY);
            
            // Check triangles
            let pivot = controller.look_at_target;
            for (_, (vertices, _)) in &mesh_data.triangles {
                for vertex in vertices {
                    let rotated_vertex = model_rotation.rotation * ((*vertex) - pivot) + pivot;
                    min_bounds = min_bounds.min(rotated_vertex);
                    max_bounds = max_bounds.max(rotated_vertex);
                }
            }
            
            // Check bone positions
            for (_, bone_pos) in &mesh_data.positions {
                let rotated_start = model_rotation.rotation * (bone_pos.start - pivot) + pivot;
                let rotated_end = model_rotation.rotation * (bone_pos.end - pivot) + pivot;
                min_bounds = min_bounds.min(rotated_start);
                max_bounds = max_bounds.max(rotated_start);
                min_bounds = min_bounds.min(rotated_end);
                max_bounds = max_bounds.max(rotated_end);
            }
            
            if min_bounds.is_finite() && max_bounds.is_finite() {
                let center = (min_bounds + max_bounds) * 0.5;
                let size = (max_bounds - min_bounds).length();
                let distance = size * 1.5; // Add some padding
                
                controller.look_at_target = center;
                transform.translation = center + Vec3::new(distance, distance, distance).normalize() * distance;
            }
        }
    }
    
    // Tab key - Toggle camera mode
    if keyboard.just_pressed(KeyCode::Tab) {
        camera_state.mode = match camera_state.mode {
            CameraMode::Free => CameraMode::Orbit,
            CameraMode::Orbit => CameraMode::Free,
        };
    }

    // Update camera to look at target only when CTRL+arrows are pressed (look_at_target changes)
    if keyboard.pressed(KeyCode::ControlLeft) || keyboard.pressed(KeyCode::ControlRight) {
        if keyboard.pressed(KeyCode::ArrowUp) || keyboard.pressed(KeyCode::ArrowDown) ||
           keyboard.pressed(KeyCode::ArrowLeft) || keyboard.pressed(KeyCode::ArrowRight) {
            transform.look_at(controller.look_at_target, Vec3::Y);
        }
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
    
    // All camera controls are now handled in update_camera function
    // This function only handles non-camera related keyboard input
}

fn handle_character_zoom(
    mut char_input_events: EventReader<KeyboardInput>,
    mut camera_query: Query<(&mut Transform, &CameraController), With<Camera>>,
    time: Res<Time>,
) {
    if let Ok((mut transform, controller)) = camera_query.get_single_mut() {
        let delta = time.delta_seconds();
        
        for event in char_input_events.read() {
            // Only process key presses, not releases
            if !event.state.is_pressed() {
                continue;
            }
            
            if let bevy::input::keyboard::Key::Character(character) = &event.logical_key {
                match character.as_str() {
                    "+" => {
                        // Zoom in
                        let forward = (controller.look_at_target - transform.translation).normalize();
                        transform.translation += forward * controller.zoom_speed * delta * 10.0; // Multiply by 10 for responsiveness
                    }
                    "-" => {
                        // Zoom out
                        let forward = (controller.look_at_target - transform.translation).normalize();
                        transform.translation -= forward * controller.zoom_speed * delta * 10.0; // Multiply by 10 for responsiveness
                    }
                    _ => {}
                }
            }
        }
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
                let right: Vec3 = (*transform.right()).into();
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
    model_rotation: Res<ModelRotation>,
    camera_query: Query<&CameraController>,
    last_modified: Option<Res<LastModified>>,
    mut triangles_last_updated: Local<Option<SystemTime>>,
) {
    // Check if mesh_data is available and triangles should be shown
    let Some(mesh_data) = mesh_data else {
        return;
    };
    
    // Only update if there are triangles to show and the setting is enabled
    if !settings.show_triangles || mesh_data.triangles.is_empty() {
        // Clear existing triangle meshes if triangles are disabled
        for entity in triangle_handles.handles.drain(..) {
            commands.entity(entity).despawn();
        }
        return;
    }

    // Check if we need to update triangles
    let file_was_modified = last_modified
        .as_ref()
        .map(|lm| triangles_last_updated.map_or(true, |tlu| lm.0 > tlu))
        .unwrap_or(false);
    
    let needs_update = triangle_handles.handles.is_empty() || file_was_modified;
    
    if !needs_update {
        return;
    }
    
    // Update the last updated timestamp
    if let Some(lm) = last_modified.as_ref() {
        *triangles_last_updated = Some(lm.0);
    }

    // Clear previous triangle meshes
    for entity in triangle_handles.handles.drain(..) {
        commands.entity(entity).despawn();
    }


    // Create triangle meshes
    for (i, (name, (vertices, color))) in mesh_data.triangles.iter().enumerate() {
            
                     
            // Create a mesh for this triangle
            let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default());
            
            // Apply model rotation to triangle vertices around look_at_target
            let pivot = camera_query.single().look_at_target;
            let rotated_v0 = model_rotation.rotation * (vertices[0] - pivot) + pivot;
            let rotated_v1 = model_rotation.rotation * (vertices[1] - pivot) + pivot;
            let rotated_v2 = model_rotation.rotation * (vertices[2] - pivot) + pivot;
            
            // Set vertex positions (just the 3 vertices)
            mesh.insert_attribute(
                Mesh::ATTRIBUTE_POSITION, 
                vec![rotated_v0, rotated_v1, rotated_v2]
            );
            
            // Set normals (all facing the same direction for simplicity)
            let normal = (rotated_v1 - rotated_v0).cross(rotated_v2 - rotated_v0).normalize();
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
            
            // Use the triangle's own color from the TOML file
            println!("Triangle {}: Using color {:?}", name, color);
            
            // Create a material with fixed color that's not affected by lighting
            let triangle_material = materials.add(StandardMaterial {
                // Use the exact color from the TOML file without any conversion
                base_color: Color::rgba(color[0], color[1], color[2], color[3]),
                // Make it unlit (not affected by lighting)
                unlit: true,                
                // Enable transparency
                alpha_mode: AlphaMode::Blend,
                // Make sure both sides are visible
                double_sided: true,         
                // Disable face culling
                cull_mode: None,            
                // Basic material properties
                metallic: 0.0,              
                perceptual_roughness: 1.0,  
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

fn draw_bones(
    mut gizmos: Gizmos,
    mesh_data: Option<Res<MeshData>>,
    _settings: Res<VisualizationSettings>,
    model_rotation: Res<ModelRotation>,
    camera_query: Query<&CameraController>,
) {
    if let Some(mesh_data) = mesh_data {
        // Draw bones
        let pivot = camera_query.single().look_at_target;
        for bone in mesh_data.positions.values() {
            // Apply model rotation to bone positions around look_at_target
            let rotated_start = model_rotation.rotation * (bone.start - pivot) + pivot;
            let rotated_end = model_rotation.rotation * (bone.end - pivot) + pivot;
            gizmos.line(
                rotated_start,
                rotated_end,
                Color::rgb(bone.color.0[0], bone.color.0[1], bone.color.0[2])
            );
        }
        
        // Draw bone end positions as arrow markers to indicate direction
        for bone in mesh_data.positions.values() {
            let rotated_start = model_rotation.rotation * (bone.start - pivot) + pivot;
            let rotated_end = model_rotation.rotation * (bone.end - pivot) + pivot;
            let bone_direction = (rotated_end - rotated_start).normalize();
            let bone_color = Color::rgb(bone.color.0[0], bone.color.0[1], bone.color.0[2]);
            
            // Create arrow marker at the end of the bone
            let arrow_size = 0.02;
            let arrow_back = rotated_end - bone_direction * arrow_size;
            
            // Create perpendicular vectors for arrow lines
            let up = if bone_direction.y.abs() < 0.9 { Vec3::Y } else { Vec3::X };
            let right = bone_direction.cross(up).normalize() * arrow_size * 0.5;
            let up_vec = right.cross(bone_direction).normalize() * arrow_size * 0.5;
            
            // Draw 4 lines forming a diamond-shaped arrow head
            gizmos.line(rotated_end, arrow_back + right, bone_color);
            gizmos.line(rotated_end, arrow_back - right, bone_color);
            gizmos.line(rotated_end, arrow_back + up_vec, bone_color);
            gizmos.line(rotated_end, arrow_back - up_vec, bone_color);
        }
        
        // Draw skin vertices as spheres
        for (i, (position, color, _, _)) in mesh_data.skin_vertex_positions.iter().enumerate() {
            // Apply model rotation to vertex position around look_at_target
            let rotated_position = model_rotation.rotation * ((*position) - pivot) + pivot;
            let sphere_radius = 0.0025; // Size of the sphere (half of previous 0.005)
            gizmos.sphere(
                rotated_position,
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
    model_rotation: Res<ModelRotation>,
    camera_query: Query<&CameraController>,
) {
             
    // For debugging purposes, always draw triangles regardless of settings
    if mesh_data.is_none() {
        return;
    }
    
    // Always draw triangles, ignoring settings.show_triangles for debugging
    
    if let Some(mesh_data) = mesh_data {
        // Draw triangle outlines only - the mesh system handles the filling
        for (i, (name, (vertices, fill_color))) in mesh_data.triangles.iter().enumerate() {
            // Determine outline color based on settings
            let outline_color = match settings.triangle_outline {
                TriangleOutlineMode::Black => Color::BLACK,
                TriangleOutlineMode::Matching => Color::rgba(fill_color[0], fill_color[1], fill_color[2], 1.0), // Full opacity
            };
            
            // Apply model rotation to triangle vertices around look_at_target
            let pivot = camera_query.single().look_at_target;
            let rotated_v0 = model_rotation.rotation * (vertices[0] - pivot) + pivot;
            let rotated_v1 = model_rotation.rotation * (vertices[1] - pivot) + pivot;
            let rotated_v2 = model_rotation.rotation * (vertices[2] - pivot) + pivot;
            
            // Draw the triangle outline
            gizmos.line(rotated_v0, rotated_v1, outline_color);
            gizmos.line(rotated_v1, rotated_v2, outline_color);
            gizmos.line(rotated_v2, rotated_v0, outline_color);
        }
    }
}

fn draw_axes(mut gizmos: Gizmos, show_axes: Res<ShowAxes>, model_rotation: Res<ModelRotation>, camera_query: Query<&CameraController>) {
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
        let pivot = camera_query.single().look_at_target;
        let start_x = model_rotation.rotation * (Vec3::new(-grid_extent, 0.0, offset) - pivot) + pivot;
        let end_x = model_rotation.rotation * (Vec3::new(grid_extent, 0.0, offset) - pivot) + pivot;
        gizmos.line(start_x, end_x, color);

        // Z-parallel lines
        let start_z = model_rotation.rotation * (Vec3::new(offset, 0.0, -grid_extent) - pivot) + pivot;
        let end_z = model_rotation.rotation * (Vec3::new(offset, 0.0, grid_extent) - pivot) + pivot;
        gizmos.line(start_z, end_z, color);
    }
}

fn update_ui(
    mut contexts: EguiContexts,
    mut settings: ResMut<VisualizationSettings>,
    mesh_data: Option<Res<MeshData>>,
    camera_query: Query<(&Camera, &GlobalTransform), With<Camera>>,
    _windows: Query<&Window>,
    model_rotation: Res<ModelRotation>,
    camera_controller_query: Query<&CameraController>,
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
            
            // Add triangle outline mode options when triangles are enabled
            if settings.show_triangles {
                ui.horizontal(|ui| {
                    ui.label("Triangle outline:");
                    ui.radio_value(
                        &mut settings.triangle_outline,
                        TriangleOutlineMode::Black,
                        "Black",
                    );
                    ui.radio_value(
                        &mut settings.triangle_outline,
                        TriangleOutlineMode::Matching,
                        "Same as fill",
                    );
                });
            }
            
            // Display triangle information
            if let Some(mesh_data) = mesh_data.as_ref() {
                ui.separator();
                ui.heading("Triangles");
                ui.label(format!("Triangle count: {}", mesh_data.triangles.len()));
                for (name, (vertices, color)) in &mesh_data.triangles {
                    ui.label(format!("Triangle {}: {:?}, {:?}, {:?}, color: {:?}", name, vertices[0], vertices[1], vertices[2], color));
                }
            }
        });

    // Draw bone labels using egui
    if let (Some(mesh_data), Ok((camera, camera_transform))) = (mesh_data.as_ref(), camera_query.get_single()) {
        // First collect all the label data we need
        let label_data: Vec<_> = mesh_data.positions.iter()
            .filter_map(|(path, bone)| {
                let name = path.split('.').last()?;
                let center = bone.start.lerp(bone.end, 0.5);
                let pivot = camera_controller_query.single().look_at_target;
                let rotated_center = model_rotation.rotation * (center - pivot) + pivot;
                let screen_pos = camera.world_to_viewport(camera_transform, rotated_center)?;
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
                let pivot = camera_controller_query.single().look_at_target;
                let rotated_position = model_rotation.rotation * ((*position) - pivot) + pivot;
                let screen_pos = camera.world_to_viewport(camera_transform, rotated_position)?;
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
        
        for (_, (vertices, _color)) in &mesh_data.triangles {
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

fn load_template_model(
    mut commands: Commands,
    template_model: Option<Res<TemplateModel>>,
    asset_server: Res<AssetServer>,
    quick_exit: Option<Res<QuickExit>>,
) {
    if let Some(template) = template_model {
        if quick_exit.is_some() {
            println!("Loading GLTF from: {:?}", template.path);
        }
        
        // Convert relative paths to absolute paths for asset loading
        let absolute_path = if template.path.is_absolute() {
            template.path.clone()
        } else {
            std::env::current_dir().unwrap_or_default().join(&template.path)
        };
        let path_str = absolute_path.to_string_lossy().to_string();
        let gltf_handle: Handle<Gltf> = asset_server.load(path_str);
        
        if quick_exit.is_some() {
            println!("GLTF handle created: {:?}", gltf_handle);
        }
        
        let transform = Transform {
            translation: template.offset,
            rotation: Quat::from_euler(
                EulerRot::XYZ,
                template.rotation.x.to_radians(),
                template.rotation.y.to_radians(),
                template.rotation.z.to_radians(),
            ),
            scale: Vec3::splat(template.scale),
        };
        
        commands.spawn((
            TemplateModelEntity,
            TemplateMeshPending {
                gltf_handle,
                transform,
            },
        ));
        
        if quick_exit.is_some() {
            println!("Template entity spawned with scale: {}", template.scale);
            println!("Template parent transform: {:?}", transform);
        }
    }
}

#[derive(Component)]
struct TemplateMeshPending {
    gltf_handle: Handle<Gltf>,
    transform: Transform,
}

#[derive(Component)]
struct TemplateModelEntity;

#[derive(Component)]
struct TemplateMeshEntity;

#[derive(Component)]
struct TemplateWireframeMesh {
    mesh: Handle<Mesh>,
    transform: Transform,
}

fn quick_exit_system(
    mut exit: EventWriter<bevy::app::AppExit>,
    mut quick_exit: Option<ResMut<QuickExit>>,
    time: Res<Time>,
) {
    if let Some(ref mut quick_exit) = quick_exit {
        quick_exit.timer += time.delta_seconds();
        if quick_exit.timer >= quick_exit.duration {
            println!("Quick exit after {:.1} seconds", quick_exit.duration);
            exit.send(bevy::app::AppExit);
        }
    }
}

fn extract_template_meshes(
    mut commands: Commands,
    pending_query: Query<(Entity, &TemplateMeshPending), With<TemplateModelEntity>>,
    gltf_assets: Res<Assets<Gltf>>,
    gltf_mesh_assets: Res<Assets<bevy::gltf::GltfMesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    quick_exit: Option<Res<QuickExit>>,
) {
    for (entity, pending) in pending_query.iter() {
        if let Some(gltf) = gltf_assets.get(&pending.gltf_handle) {
            if quick_exit.is_some() {
                println!("GLTF loaded with {} meshes", gltf.meshes.len());
            }
            
            commands.entity(entity).remove::<TemplateMeshPending>();
            
            // Create a wireframe material for the template model
            let template_material = materials.add(StandardMaterial {
                base_color: Color::rgba(1.0, 0.0, 0.0, 0.0), // Fully transparent - don't render faces
                alpha_mode: AlphaMode::Blend,
                cull_mode: None,
                unlit: true,
                ..default()
            });
            
            let mut total_primitives = 0;
            
            for (i, gltf_mesh_handle) in gltf.meshes.iter().enumerate() {
                if let Some(gltf_mesh) = gltf_mesh_assets.get(gltf_mesh_handle) {
                    if quick_exit.is_some() {
                        println!("Mesh {}: {} primitives", i, gltf_mesh.primitives.len());
                    }
                    
                    for (j, primitive) in gltf_mesh.primitives.iter().enumerate() {
                        // Store mesh info for wireframe gizmo rendering instead of creating entities
                        commands.spawn((
                            TemplateWireframeMesh {
                                mesh: primitive.mesh.clone(),
                                transform: pending.transform,
                            },
                        ));
                        total_primitives += 1;
                        
                        if quick_exit.is_some() {
                            println!("Created wireframe reference for primitive {} of mesh {}", j, i);
                            println!("Mesh transform: {:?}", pending.transform);
                        }
                    }
                } else if quick_exit.is_some() {
                    println!("Warning: GltfMesh {} not yet loaded", i);
                }
            }
            
            if quick_exit.is_some() {
                println!("Total template mesh entities created: {}", total_primitives);
            }
        } else if quick_exit.is_some() {
            println!("GLTF asset not yet loaded");
        }
    }
}

fn debug_template_transforms(
    template_query: Query<Entity, With<TemplateModelEntity>>,
    children_query: Query<&Children>,
    child_transform_query: Query<&Transform, Without<TemplateModelEntity>>,
    quick_exit: Option<Res<QuickExit>>,
) {
    if quick_exit.is_some() {
        for entity in template_query.iter() {
            println!("[DEBUG] Template parent entity {:?} (no transform component)", entity);
            
            if let Ok(children) = children_query.get(entity) {
                for &child in children.iter() {
                    if let Ok(child_transform) = child_transform_query.get(child) {
                        println!("[DEBUG] Template child entity {:?} transform: {:?}", child, child_transform);
                    }
                }
            }
        }
    }
}

fn draw_template_wireframe(
    mut gizmos: Gizmos,
    wireframe_query: Query<&TemplateWireframeMesh>,
    meshes: Res<Assets<Mesh>>,
) {
    for wireframe_mesh in wireframe_query.iter() {
        if let Some(mesh) = meshes.get(&wireframe_mesh.mesh) {
            // Get mesh vertex positions
            if let Some(vertex_attribute) = mesh.attribute(Mesh::ATTRIBUTE_POSITION) {
                if let bevy::render::mesh::VertexAttributeValues::Float32x3(positions) = vertex_attribute {
                    // Get indices to draw edges
                    if let Some(indices) = mesh.indices() {
                        match indices {
                            bevy::render::mesh::Indices::U32(indices) => {
                                // Draw triangles as wireframe (3 lines per triangle)
                                for triangle in indices.chunks(3) {
                                    if triangle.len() == 3 {
                                        let v0 = Vec3::from(positions[triangle[0] as usize]);
                                        let v1 = Vec3::from(positions[triangle[1] as usize]);
                                        let v2 = Vec3::from(positions[triangle[2] as usize]);
                                        
                                        // Apply transform
                                        let t0 = wireframe_mesh.transform.transform_point(v0);
                                        let t1 = wireframe_mesh.transform.transform_point(v1);
                                        let t2 = wireframe_mesh.transform.transform_point(v2);
                                        
                                        // Draw triangle edges
                                        gizmos.line(t0, t1, Color::GRAY);
                                        gizmos.line(t1, t2, Color::GRAY);
                                        gizmos.line(t2, t0, Color::GRAY);
                                    }
                                }
                            }
                            bevy::render::mesh::Indices::U16(indices) => {
                                // Handle U16 indices similarly
                                for triangle in indices.chunks(3) {
                                    if triangle.len() == 3 {
                                        let v0 = Vec3::from(positions[triangle[0] as usize]);
                                        let v1 = Vec3::from(positions[triangle[1] as usize]);
                                        let v2 = Vec3::from(positions[triangle[2] as usize]);
                                        
                                        // Apply transform
                                        let t0 = wireframe_mesh.transform.transform_point(v0);
                                        let t1 = wireframe_mesh.transform.transform_point(v1);
                                        let t2 = wireframe_mesh.transform.transform_point(v2);
                                        
                                        // Draw triangle edges
                                        gizmos.line(t0, t1, Color::GRAY);
                                        gizmos.line(t1, t2, Color::GRAY);
                                        gizmos.line(t2, t0, Color::GRAY);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn make_template_transparent(
    template_mesh_query: Query<Entity, (With<TemplateMeshEntity>, Without<TransparentTemplateProcessed>)>,
    mut material_query: Query<&mut Handle<StandardMaterial>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut commands: Commands,
    template_config: Option<Res<TemplateModel>>,
) {
    if let Some(template) = &template_config {
        println!("[TEMPLATE] Making template transparent with alpha: {}", template.transparency);
        
        let mut materials_processed = 0;
        
        // Process all template mesh entities directly
        for entity in template_mesh_query.iter() {
            if let Ok(mut material_handle) = material_query.get_mut(entity) {
                // Clone the material so we can modify it
                if let Some(material) = materials.get(&*material_handle) {
                    println!("[TEMPLATE] Found material on entity {:?}, making transparent", entity);
                    let mut new_material = material.clone();
                    new_material.alpha_mode = AlphaMode::Blend;
                    new_material.base_color.set_a(template.transparency);
                    
                    // Create a new material asset and replace the handle
                    let new_handle = materials.add(new_material);
                    *material_handle = new_handle;
                    materials_processed += 1;
                }
            }
            
            // Mark this mesh entity as processed
            commands.entity(entity).insert(TransparentTemplateProcessed);
        }
        
        println!("[TEMPLATE] Processed {} materials for transparency", materials_processed);
    }
}

fn make_descendants_transparent(
    entity: Entity,
    children_query: &Query<&Children>,
    material_query: &mut Query<&mut Handle<StandardMaterial>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    transparency: f32,
) -> u32 {
    let mut materials_processed = 0;
    
    // Try to get the material handle for this entity
    if let Ok(mut material_handle) = material_query.get_mut(entity) {
        // Clone the material so we can modify it
        if let Some(material) = materials.get(&*material_handle) {
            println!("[TEMPLATE] Found material on entity {:?}, making transparent", entity);
            let mut new_material = material.clone();
            new_material.alpha_mode = AlphaMode::Blend;
            new_material.base_color.set_a(transparency);
            
            // Create a new material asset and replace the handle
            let new_handle = materials.add(new_material);
            *material_handle = new_handle;
            materials_processed += 1;
        }
    }
    
    // Recursively process children
    if let Ok(children) = children_query.get(entity) {
        println!("[TEMPLATE] Entity {:?} has {} children", entity, children.len());
        for &child in children.iter() {
            materials_processed += make_descendants_transparent(child, children_query, material_query, materials, transparency);
        }
    }
    
    materials_processed
}

#[derive(Component)]
struct TransparentTemplateProcessed;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <mesh_file> [options]", args[0]);
        eprintln!("Options:");
        eprintln!("  --export <path>     Export to GLB file");
        eprintln!("  --t <path>          Template GLTF model path");
        eprintln!("  --tx <degrees>      Template X rotation (default: 0)");
        eprintln!("  --ty <degrees>      Template Y rotation (default: 0)");
        eprintln!("  --tz <degrees>      Template Z rotation (default: 0)");
        eprintln!("  --ts <scale>        Template scale factor (default: 1.0)");
        eprintln!("  --q <seconds>       Quick exit after specified seconds (for testing)");
        process::exit(1);
    }

    let mesh_file = PathBuf::from(&args[1]);
    let mut export_path = None;
    let mut template_path = None;
    let mut template_rotation = Vec3::ZERO;
    let mut template_scale = 1.0;
    let mut template_offset = Vec3::ZERO;
    let mut quick_exit_duration: Option<f32> = None;

    // Parse remaining arguments
    let mut i = 2;
    while i < args.len() {
        match args[i].as_str() {
            "--export" if i + 1 < args.len() => {
                export_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            }
            "--ref" if i + 1 < args.len() => {
                template_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            }
            "--ref_rot_x" if i + 1 < args.len() => {
                template_rotation.x = args[i + 1].parse().unwrap_or(0.0);
                i += 2;
            }
            "--ref_rot_y" if i + 1 < args.len() => {
                template_rotation.y = args[i + 1].parse().unwrap_or(0.0);
                i += 2;
            }
            "--ref_rot_z" if i + 1 < args.len() => {
                template_rotation.z = args[i + 1].parse().unwrap_or(0.0);
                i += 2;
            }
            "--ref_scale" if i + 1 < args.len() => {
                template_scale = args[i + 1].parse().unwrap_or(1.0);
                i += 2;
            }
            "--ref_off_x" if i + 1 < args.len() => {
                template_offset.x = args[i + 1].parse().unwrap_or(0.0);
                i += 2;
            }
            "--ref_off_y" if i + 1 < args.len() => {
                template_offset.y = args[i + 1].parse().unwrap_or(0.0);
                i += 2;
            }
            "--ref_off_z" if i + 1 < args.len() => {
                template_offset.z = args[i + 1].parse().unwrap_or(0.0);
                i += 2;
            }
            "--q" if i + 1 < args.len() => {
                quick_exit_duration = Some(args[i + 1].parse().unwrap_or(3.0));
                i += 2;
            }
            _ => {
                eprintln!("Unknown argument: {}", args[i]);
                i += 1;
            }
        }
    }

    let mut app = App::new();
    app.add_plugins(DefaultPlugins)
        .add_plugins(EguiPlugin)
        .add_plugins(bevy::pbr::wireframe::WireframePlugin)
        .insert_resource(MeshFile(mesh_file))
        .insert_resource(ModelRotation::default())
        .insert_resource(CameraState::default())
        .insert_resource(if let Some(path) = export_path {
            ExportPath(path)
        } else {
            ExportPath(PathBuf::from("output.glb"))
        });
    
    // Add template model resource if path is provided
    if let Some(template_path) = template_path {
        app.insert_resource(TemplateModel {
            path: template_path,
            scale: template_scale,
            rotation: template_rotation,
            offset: template_offset,
            transparency: 0.5, // Default transparency
        });
    }
    
    if let Some(duration) = quick_exit_duration {
        app.insert_resource(QuickExit { timer: 0.0, duration });
    }
    
    app
        .add_event::<FileChangedEvent>()  // Register the event
        .insert_resource(TriangleMeshHandles::default())
        .add_systems(Startup, (setup, load_template_model))
        .add_systems(
            Update,
            (
                monitor_file,
                handle_file_changes,
                keyboard_input,
                handle_character_zoom,
                mouse_motion,
                mouse_wheel,
                mouse_button_input,
                update_camera,
                export_system,
            ).chain(),
        )
        .add_systems(Update, update_triangle_meshes.after(handle_file_changes))
        .add_systems(Update, draw_triangles)
        .add_systems(Update, extract_template_meshes)
        .add_systems(Update, draw_template_wireframe.after(extract_template_meshes))
        .add_systems(Update, debug_template_transforms.after(extract_template_meshes))
        .add_systems(Update, make_template_transparent.after(debug_template_transforms))
        .add_systems(Update, quick_exit_system)
        // Make sure the UI system runs after EguiSet::InitContexts
        .add_systems(Update, update_ui.after(bevy_egui::EguiSet::InitContexts))
        .add_systems(PostUpdate, (draw_bones, draw_axes))
        .run();
}