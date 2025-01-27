use bevy::{
    prelude::*,
    input::mouse::{MouseMotion, MouseWheel},
    render::{
        mesh::{shape, Indices},
        render_resource::PrimitiveTopology,
        render_asset::RenderAssetUsages,
    },
    window::PresentMode,
};
use serde::Deserialize;
use std::{
    collections::{HashMap, BTreeMap},
    path::{Path, PathBuf},
    time::SystemTime,
};
use hooded_crow_modeller::{Model, Group};
use bevy_egui::{egui, EguiPlugin, EguiContexts};
use gltf_json::{
    self as json,
    validation::{Checked, USize64},
    Index,
    accessor,
    mesh,
    Accessor,
    Buffer,
    Node,
    Scene,
    Root,
};

#[derive(Resource)]
struct FileWatcherResource {
    path: PathBuf,
    last_modified: SystemTime,
}

impl FileWatcherResource {
    fn new(path: PathBuf) -> Self {
        let last_modified = std::fs::metadata(&path)
            .expect("Failed to get file metadata")
            .modified()
            .expect("Failed to get last modified time");
        Self { path, last_modified }
    }
}

#[derive(Resource)]
struct MeshFile(PathBuf);

#[derive(Resource)]
struct ExportPath(PathBuf);

#[derive(Resource)]
struct ShowAxes(bool);

#[derive(Resource)]
struct ShowLabels(bool);

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Value {
    Absolute(f32),
    Relative {
        reference: String,
        offset: f32,
    },
}

#[derive(Debug, Deserialize)]
struct Bone {
    connection: Option<String>,
    length: f32,
    orientation: Value,
    slope: Value,
    rotation: Value,
    parent: String,
}

#[derive(Resource, Clone)]
struct MeshData {
    positions: HashMap<String, BonePosition>,
    vertices: Vec<[f32; 3]>,
    indices: Vec<u32>,
}

#[derive(Clone)]
struct BonePosition {
    start: Vec3,
    end: Vec3,
}

impl From<Model> for MeshData {
    fn from(model: Model) -> Self {
        let positions = calculate_positions(&model.0);
        let (vertices, indices) = create_mesh_data(&positions);
        
        Self {
            positions,
            vertices,
            indices,
        }
    }
}

#[derive(Component)]
struct BoneLabel {
    bone_name: String,
    world_position: Vec3,
}

#[derive(Component)]
struct UiCamera;

#[derive(Component)]
struct UiRoot;

#[derive(Component)]
struct SceneReady;

#[derive(Component)]
struct CameraInitialized;

#[derive(Component)]
struct CleanupMarker;

#[derive(Component)]
struct LabelText;

#[derive(Component)]
struct BoneText {
    bone_name: String,
    position: Vec3,
}

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
enum ViewerSystem {
    Camera,
    Labels,
}

#[derive(States, Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
enum ViewerState {
    #[default]
    Loading,
    Viewing,
}

impl MeshData {
    fn resolve_positions(&self) -> HashMap<String, BonePosition> {
        self.positions.clone()
    }

    fn get_vertices(&self) -> Vec<[f32; 3]> {
        self.vertices.clone()
    }

    fn get_indices(&self) -> Vec<u32> {
        self.indices.clone()
    }
}

fn calculate_direction(orientation: f32, slope: f32) -> Vec3 {
    let orientation_rad = orientation.to_radians();
    let slope_rad = slope.to_radians();

    Vec3::new(
        slope_rad.sin() * orientation_rad.sin(),
        slope_rad.cos(),
        slope_rad.sin() * orientation_rad.cos()
    )
}

#[derive(Resource)]
struct CameraState {
    transform: Transform,
}

#[derive(Resource, Clone)]
struct PendingScene {
    mesh_data: MeshData,
    camera_transform: Transform,
}

fn check_file_changes(
    mut commands: Commands,
    mut file_watcher: ResMut<FileWatcherResource>,
    mesh_file: Res<MeshFile>,
    to_cleanup: Query<Entity, With<CleanupMarker>>,
    mut next_state: ResMut<NextState<ViewerState>>,
    camera_query: Query<&Transform, With<Camera>>,
) {
    if let Ok(metadata) = std::fs::metadata(&file_watcher.path) {
        if let Ok(modified) = metadata.modified() {
            if modified > file_watcher.last_modified {
                // Read and parse the TOML file first
                if let Ok(mesh_toml) = std::fs::read_to_string(&mesh_file.0) {
                    if let Ok(model) = toml::from_str::<Model>(&mesh_toml) {
                        // Prepare new mesh data before cleanup
                        let mesh_data = MeshData::from(model);
                        
                        // Store camera transform
                        if let Ok(camera_transform) = camera_query.get_single() {
                            commands.insert_resource(CameraState {
                                transform: *camera_transform,
                            });
                        }
                        
                        // Update timestamp before cleanup
                        file_watcher.last_modified = modified;
                        
                        // Quick cleanup and resource update
                        for entity in to_cleanup.iter() {
                            commands.entity(entity).despawn_recursive();
                        }
                        
                        // Insert new data and trigger reload immediately
                        commands.insert_resource(mesh_data);
                        next_state.set(ViewerState::Loading);
                    }
                }
            }
        }
    }
}

fn handle_loading_state(
    mut commands: Commands,
    to_cleanup: Query<Entity, With<CleanupMarker>>,
    pending_scene: Option<Res<PendingScene>>,
    mut next_state: ResMut<NextState<ViewerState>>,
) {
    if let Some(pending_scene) = pending_scene {
        println!("Handling loading state with pending scene");
        // Clean up old scene
        for entity in to_cleanup.iter() {
            commands.entity(entity).despawn_recursive();
        }
        
        if to_cleanup.is_empty() {
            println!("Cleanup complete, inserting new resources");
            // Insert the new resources
            commands.insert_resource(pending_scene.mesh_data.clone());
            commands.insert_resource(CameraState {
                transform: pending_scene.camera_transform,
            });
            
            // Remove the pending scene
            commands.remove_resource::<PendingScene>();
            
            // Transition to viewing state
            next_state.set(ViewerState::Viewing);
            println!("Transitioned to Viewing state");
        }
    }
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_data: Res<MeshData>,
    show_axes: Res<ShowAxes>,
    show_labels: Res<ShowLabels>,
    camera_state: Option<Res<CameraState>>,
    mut next_state: ResMut<NextState<ViewerState>>,
) {
    println!("\n=== Setup Starting ===");

    // Find the pelvis position to use as offset
    let offset = mesh_data.positions.iter()
        .find(|(name, _)| name.contains("body.left_leg.pelvis"))
        .map(|(_, pos)| pos.start)
        .unwrap_or(Vec3::ZERO);

    // Add coordinate axes only if enabled
    if show_axes.0 {
        spawn_coordinate_axes(&mut commands, &mut meshes, &mut materials);
    }

    // Add 3D camera, using stored transform if available
    let camera_transform = camera_state
        .map(|state| state.transform)
        .unwrap_or_else(|| Transform::from_xyz(-2.0, 2.5, 5.0).looking_at(Vec3::ZERO, Vec3::Y));

    commands.spawn((
        Camera3dBundle {
            transform: camera_transform,
            camera: Camera {
                order: 0,
                ..default()
            },
            ..default()
        },
        CleanupMarker,
    ));
    println!("Camera spawned");

    // Light setup
    commands.spawn((
        PointLightBundle {
            point_light: PointLight {
                intensity: 1500.0,
                shadows_enabled: true,
                ..default()
            },
            transform: Transform::from_xyz(4.0, 8.0, 4.0),
            ..default()
        },
        CleanupMarker,
    ));
    println!("Light spawned");

    // Create and spawn the bone mesh
    let mesh = create_mesh(&mesh_data);
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(mesh),
            material: materials.add(create_material()),
            transform: Transform::from_scale(Vec3::splat(5.0))
                .with_translation(-offset * 5.0),  // Offset the model to center on pelvis
            ..default()
        },
        CleanupMarker,
    ));
    println!("Bone mesh spawned");

    // Mark scene as ready
    commands.spawn((SceneReady, CleanupMarker));
    
    // Transition to viewing state
    next_state.set(ViewerState::Viewing);
    println!("=== Setup Complete, transitioning to Viewing state ===\n");
}

fn spawn_coordinate_axes(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
) {
    // X axis - Red
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(create_axis_mesh()),
            material: materials.add(StandardMaterial {
                base_color: Color::RED,
                unlit: true,
                ..default()
            }),
            transform: Transform::from_xyz(0.0, 0.0, 0.0),
            ..default()
        },
        CleanupMarker,
    ));

    // Y axis - Green
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(create_axis_mesh()),
            material: materials.add(StandardMaterial {
                base_color: Color::GREEN,
                unlit: true,
                ..default()
            }),
            transform: Transform::from_rotation(Quat::from_rotation_z(std::f32::consts::FRAC_PI_2)),
            ..default()
        },
        CleanupMarker,
    ));

    // Z axis - Blue
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(create_axis_mesh()),
            material: materials.add(StandardMaterial {
                base_color: Color::BLUE,
                unlit: true,
                ..default()
            }),
            transform: Transform::from_rotation(Quat::from_rotation_y(-std::f32::consts::FRAC_PI_2)),
            ..default()
        },
        CleanupMarker,
    ));
}

fn create_axis_mesh() -> Mesh {
    let mut mesh = Mesh::new(PrimitiveTopology::LineList, RenderAssetUsages::RENDER_WORLD);
    
    // Create a line from origin to 10 units in the X direction
    mesh.insert_attribute(
        Mesh::ATTRIBUTE_POSITION, 
        vec![[0.0, 0.0, 0.0], [10.0, 0.0, 0.0]]
    );
    
    mesh.insert_indices(Indices::U32(vec![0, 1]));
    mesh
}

fn cleanup_scene(
    commands: &mut Commands,
    to_cleanup: Query<Entity, With<CleanupMarker>>,
) {
    println!("Cleaning up scene...");
    for entity in to_cleanup.iter() {
        commands.entity(entity).despawn_recursive();
    }
    println!("Scene cleanup complete");
}

fn orbit_camera(
    mut query: Query<&mut Transform, With<Camera>>,
    mut mouse_motion: EventReader<MouseMotion>,
    keyboard: Res<ButtonInput<MouseButton>>,
    mut mouse_wheel: EventReader<MouseWheel>,
) {
    let mut transform = query.single_mut();
    let rotation_center = Vec3::ZERO;

    // Handle mouse wheel for zoom
    let zoom_speed = 0.2;
    for event in mouse_wheel.read() {
        let dir = (rotation_center - transform.translation).normalize();
        transform.translation += dir * event.y * zoom_speed;
    }

    // Handle rotation
    if keyboard.pressed(MouseButton::Left) {
        for event in mouse_motion.read() {
            let sensitivity = 0.005;
            
            // Apply yaw rotation first (around global Y axis)
            let yaw = Quat::from_axis_angle(Vec3::Y, -event.delta.x * sensitivity);
            
            // Then pitch (around local X axis)
            let right: Vec3 = transform.right().into();  // Convert Direction3d to Vec3
            let pitch = Quat::from_axis_angle(right, -event.delta.y * sensitivity);
            
            // Calculate new position
            let arm = transform.translation - rotation_center;
            let rotated_arm = yaw * pitch * arm;
            
            // Update transform
            transform.translation = rotation_center + rotated_arm;
            transform.look_at(rotation_center, Vec3::Y);
            
            eprintln!("Mouse delta: {:?}, New position: {:?}", event.delta, transform.translation);
        }
    }
}

fn setup_camera(
    mut commands: Commands,
    scene_ready: Query<&SceneReady>,
    camera_query: Query<Entity, (With<Camera3d>, Without<CameraInitialized>)>,
) {
    // Only proceed if the scene is ready
    if scene_ready.is_empty() {
        return;
    }

    // Only run if we find an uninitialized camera
    if let Ok(camera_entity) = camera_query.get_single() {
        commands.entity(camera_entity).insert(CameraInitialized);
    }
}

fn export_to_glb(mesh_data: &MeshData, export_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Create a JSON structure for the GLB
    let mut root = json::Root::default();
    
    // Create buffer and get vertex and index data
    let vertices = mesh_data.get_vertices();
    let indices = mesh_data.get_indices();
    
    // Calculate buffer sizes
    let vertex_buffer_length = vertices.len() * 12; // 3 floats * 4 bytes each
    let index_buffer_length = indices.len() * 4;    // uint32 = 4 bytes
    let buffer_length = vertex_buffer_length + index_buffer_length;
    
    // Create buffer
    let buffer = json::Buffer {
        byte_length: USize64::from(buffer_length),
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        uri: None,
    };
    root.buffers.push(buffer);

    // Create buffer views
    let vertex_view = json::buffer::View {
        buffer: Index::new(0),  // First (and only) buffer
        byte_length: USize64::from(vertex_buffer_length),
        byte_offset: Some(USize64::from(0_usize)),
        byte_stride: Some(json::buffer::Stride(12)),
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        target: Some(Checked::Valid(json::buffer::Target::ArrayBuffer)),
    };
    let vertex_view_index = root.buffer_views.len();
    root.buffer_views.push(vertex_view);

    let index_view = json::buffer::View {
        buffer: Index::new(0),
        byte_length: USize64::from(index_buffer_length),
        byte_offset: Some(USize64::from(0_usize)),
        byte_stride: None,
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        target: Some(Checked::Valid(json::buffer::Target::ElementArrayBuffer)),
    };
    let index_view_index = root.buffer_views.len();
    root.buffer_views.push(index_view);

    // Create accessors
    let vertex_accessor = json::Accessor {
        buffer_view: Some(Index::new(vertex_view_index as u32)),
        byte_offset: Some(USize64::from(0_usize)),
        count: USize64::from(vertices.len()),
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
    let vertex_accessor_index = root.accessors.len();
    root.accessors.push(vertex_accessor);

    let index_accessor = json::Accessor {
        buffer_view: Some(Index::new(index_view_index as u32)),
        byte_offset: Some(USize64::from(0_usize)),
        count: USize64::from(indices.len()),
        component_type: Checked::Valid(json::accessor::GenericComponentType(
            json::accessor::ComponentType::U32,
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
    let index_accessor_index = root.accessors.len();
    root.accessors.push(index_accessor);

    // Create primitive
    let primitive = json::mesh::Primitive {
        attributes: {
            let mut map = BTreeMap::new();
            map.insert(
                Checked::Valid(json::mesh::Semantic::Positions),
                Index::new(vertex_accessor_index as u32),
            );
            map
        },
        extensions: Default::default(),
        extras: Default::default(),
        indices: Some(Index::new(index_accessor_index as u32)),
        material: None,
        mode: Checked::Valid(json::mesh::Mode::Lines),
        targets: None,
    };

    // Create mesh
    let mesh = json::Mesh {
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
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
        mesh: Some(Index::new(mesh_index as u32)),
        name: None,
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
        name: None,
        nodes: vec![Index::new(node_index as u32)],
    };
    root.scenes.push(scene);

    // Create binary buffer
    let mut buffer_data: Vec<u8> = Vec::new();
    
    // Add vertex data
    for vertex in vertices {
        buffer_data.extend_from_slice(bytemuck::cast_slice(&[vertex[0], vertex[1], vertex[2]]));
    }
    
    // Add index data
    buffer_data.extend_from_slice(bytemuck::cast_slice(&indices));

    // Write GLB file
    let writer = std::fs::File::create(export_path)?;
    gltf::json::serialize::to_writer(writer, &root)?;

    Ok(())
}

fn draw_labels(
    mut egui_contexts: EguiContexts,
    windows: Query<&Window>,
    camera: Query<(&Camera, &GlobalTransform)>,
    mesh_data: Res<MeshData>,
    show_labels: Res<ShowLabels>,
) {
    // Early return if labels are disabled
    if !show_labels.0 {
        return;
    }

    let positions = mesh_data.resolve_positions();
    let (camera, camera_transform) = camera.single();
    let window = windows.single();
    let scale = 5.0;  // Same scale as used in setup for the bones
    let label_offset = Vec3::new(0.02, 0.02, 0.0);  // Smaller offset to prevent overlap

    let mut ctx = egui_contexts.ctx_mut();
    
    for (name, position) in positions.iter() {
        // Calculate the midpoint between start and end, then add offset
        let midpoint = (position.start + position.end) * 0.5 + label_offset;
        
        if let Some(screen_pos) = world_to_screen(
            window,
            camera,
            camera_transform,
            midpoint * scale,
        ) {
            // Extract the last part of the bone name (after the last dot)
            let display_name = name.split('.').last().unwrap_or(name);
            
            egui::Area::new(egui::Id::new(name))
                .fixed_pos(egui::pos2(screen_pos.x, screen_pos.y))
                .show(ctx, |ui| {
                    ui.style_mut().text_styles.get_mut(&egui::TextStyle::Body).unwrap().size = 8.0;
                    ui.colored_label(egui::Color32::WHITE, display_name);
                });
        }
    }
}

fn create_mesh(mesh_data: &MeshData) -> Mesh {
    let mut mesh = Mesh::new(
        PrimitiveTopology::LineList,
        RenderAssetUsages::RENDER_WORLD,
    );
    
    let vertices = mesh_data.get_vertices();
    let indices = mesh_data.get_indices();
    
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices);
    mesh.insert_indices(Indices::U32(indices));
    
    mesh
}

fn create_material() -> StandardMaterial {
    StandardMaterial {
        base_color: Color::YELLOW,
        unlit: true,
        ..default()
    }
}

// Add new system for drawing joint spheres
fn draw_joints(
    mesh_data: Res<MeshData>,
    mut gizmos: Gizmos,
) {
    let positions = mesh_data.resolve_positions();
    let scale = 5.0;

    for position in positions.values() {
        gizmos.sphere(
            position.start * scale,
            Quat::IDENTITY,
            0.05,
            Color::RED,
        );
        gizmos.sphere(
            position.end * scale,
            Quat::IDENTITY,
            0.05,
            Color::RED,
        );
    }
}

fn draw_bones(model: &Model, commands: &mut Commands, meshes: &mut ResMut<Assets<Mesh>>, materials: &mut ResMut<Assets<StandardMaterial>>) {
    println!("=== Starting scene setup ===");
    
    // Get vertices and indices in the correct order
    let (vertices, indices) = model.get_bone_chain();
    
    println!("Vertices from get_bone_chain:");
    for (i, v) in vertices.iter().enumerate() {
        println!("  Vertex {}: {:?}", i, v);
    }
    println!("Indices from get_bone_chain: {:?}", indices);

    // Create a single mesh for all bones
    let mut mesh = Mesh::new(PrimitiveTopology::LineList, RenderAssetUsages::default());
    
    // Convert vertices to the format Bevy expects
    let vertices_vec: Vec<[f32; 3]> = vertices.iter().copied().collect();
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices_vec);
    
    // Convert indices to the format Bevy expects
    let indices_vec: Vec<u32> = indices.iter().copied().collect();
    mesh.insert_indices(Indices::U32(indices_vec));

    // Spawn the mesh
    commands.spawn(PbrBundle {
        mesh: meshes.add(mesh),
        material: materials.add(StandardMaterial {
            base_color: Color::WHITE,
            ..default()
        }),
        ..default()
    });

    println!("Scene setup complete");
}

fn world_to_screen(
    window: &Window,
    camera: &Camera,
    camera_transform: &GlobalTransform,
    world_pos: Vec3,
) -> Option<Vec2> {
    camera.world_to_viewport(camera_transform, world_pos)
        .map(|coords| Vec2::new(coords.x, coords.y))
}

fn calculate_positions(groups: &HashMap<String, Group>) -> HashMap<String, BonePosition> {
    let mut positions = HashMap::new();
    let mut pending_bones = Vec::new();
    
    // First pass: collect all bones
    fn collect_bones(
        group: &Group,
        path: &str,
        pending: &mut Vec<(String, String, f32, f32, f32, f32)>
    ) {
        eprintln!("Processing group at path: {}", path);
        
        // Add bones from current group
        for bone in &group.bones {
            pending.push((
                bone.name.clone(),
                bone.parent.clone(),
                bone.orientation,
                bone.slope,
                bone.rotation,
                bone.length
            ));
            if bone.parent.is_empty() {
                eprintln!("Found root bone: {} (parent: \"\")", bone.name);
            }
        }
        
        // Process all subgroups
        for (name, subgroup) in &group.subgroups {
            let new_path = if path.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", path, name)
            };
            collect_bones(subgroup, &new_path, pending);
        }
    }
    
    // Collect all bones
    for (name, group) in groups {
        collect_bones(group, name, &mut pending_bones);
    }
    
    // Process bones until no more can be added
    let mut iteration = 0;
    let mut made_progress = true;
    while made_progress && !pending_bones.is_empty() {
        made_progress = false;
        let mut remaining = Vec::new();
        
        for (name, parent, orientation, slope, rotation, length) in pending_bones.drain(..) {
            let parent_end = if parent.is_empty() {
                eprintln!("Setting root bone {} position to Vec3::ZERO", name);
                Some(Vec3::ZERO)
            } else {
                positions.get(&parent).map(|p: &BonePosition| p.end)
            };
            
            if let Some(start) = parent_end {
                let direction = calculate_direction(orientation, slope);
                let end = start + direction * length;
                positions.insert(name.clone(), BonePosition { start, end });
                
                if parent.is_empty() {
                    eprintln!("Root bone {} position: start={:?}, end={:?}", name, start, end);
                }
                
                made_progress = true;
            } else {
                remaining.push((name, parent, orientation, slope, rotation, length));
            }
        }
        
        pending_bones = remaining;
        iteration += 1;
    }
    
    // Log all positions to verify
    for (name, pos) in &positions {
        if name.contains("body.left_leg.pelvis") {
            eprintln!("Final position of {}: start={:?}, end={:?}", name, pos.start, pos.end);
        }
    }
    
    positions
}

fn create_mesh_data(positions: &HashMap<String, BonePosition>) -> (Vec<[f32; 3]>, Vec<u32>) {
    let mut vertices = Vec::new();
    let mut indices = Vec::new();
    let mut current_index = 0;
    
    for position in positions.values() {
        vertices.push(position.start.into());
        vertices.push(position.end.into());
        indices.push(current_index);
        indices.push(current_index + 1);
        current_index += 2;
    }
    
    (vertices, indices)
}

fn setup_labels(
    mut commands: Commands,
    mesh_data: Res<MeshData>,
) {
    for (name, position) in &mesh_data.positions {
        // Extract the last part of the bone name (after the last dot)
        let display_name = name.split('.').last().unwrap_or(name);
        
        let text_style = TextStyle {
            font_size: 20.0,
            color: Color::WHITE,
            ..default()
        };
        
        let text = Text::from_section(display_name.to_string(), text_style);
        
        commands.spawn((
            Text2dBundle {
                text,
                transform: Transform::from_translation(position.end),
                ..default()
            },
            CleanupMarker,
        ));
    }
}

fn setup_joints(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_data: Res<MeshData>,
) {
    let joint_mesh = meshes.add(Mesh::from(shape::UVSphere {
        radius: 0.005,
        sectors: 8,
        stacks: 8,
    }));

    let joint_material = materials.add(StandardMaterial {
        base_color: Color::rgb(0.8, 0.2, 0.2),
        ..default()
    });

    for position in mesh_data.positions.values() {
        commands.spawn((
            PbrBundle {
                mesh: joint_mesh.clone(),
                material: joint_material.clone(),
                transform: Transform::from_translation(position.start * 5.0),
                ..default()
            },
            CleanupMarker,
        ));

        commands.spawn((
            PbrBundle {
                mesh: joint_mesh.clone(),
                material: joint_material.clone(),
                transform: Transform::from_translation(position.end * 5.0),
                ..default()
            },
            CleanupMarker,
        ));
    }
}

fn main() {
    // Get command line arguments
    let args: Vec<String> = std::env::args().collect();
    
    // Check flags
    let show_axes = args.iter().any(|arg| arg == "--show-axis");
    let show_labels = args.iter().any(|arg| arg == "--show-labels");
    
    // Get non-flag arguments (files)
    let file_args: Vec<_> = args.iter()
        .filter(|arg| !arg.starts_with("--"))
        .collect();
    
    // First non-flag argument after program name is mesh file
    let mesh_file = file_args.get(1)
        .expect("Please provide a mesh file path");
    
    // Second non-flag argument is export path
    let export_path = file_args.get(2)
        .expect("Please provide an export path");

    // Read and parse the TOML file
    let mesh_toml = std::fs::read_to_string(mesh_file)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {}", mesh_file, e));
    
    let model: Model = toml::from_str(&mesh_toml)
        .unwrap_or_else(|e| panic!("Failed to parse TOML from {}: {}", mesh_file, e));
    
    let mesh_data = MeshData::from(model);

    App::new()
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    present_mode: PresentMode::Immediate,
                    ..default()
                }),
                ..default()
            }),
            EguiPlugin,
        ))
        .init_state::<ViewerState>()
        .insert_resource(FileWatcherResource::new(PathBuf::from(mesh_file)))
        .insert_resource(ExportPath(PathBuf::from(export_path)))
        .insert_resource(mesh_data)
        .insert_resource(MeshFile(PathBuf::from(mesh_file)))
        .insert_resource(ShowAxes(show_axes))
        .insert_resource(ShowLabels(show_labels))
        .add_systems(Startup, |mut next_state: ResMut<NextState<ViewerState>>| {
            next_state.set(ViewerState::Loading);
        })
        .add_systems(OnEnter(ViewerState::Loading), setup)
        .add_systems(Update, setup_camera)
        .add_systems(Update, orbit_camera)
        .add_systems(Update, draw_labels)
        .add_systems(Update, draw_joints)
        .add_systems(Update, check_file_changes)
        .run();
}