use bevy::{
    prelude::*,
    render::{mesh::Indices, render_resource::PrimitiveTopology},
    window::PrimaryWindow,
    log::LogPlugin,
    input::mouse::MouseMotion,
    text::Text2dBundle,
};
use gltf::json::{self, Index};
use gltf_json::validation::{Checked, USize64};
use serde::Deserialize;
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

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
}

#[derive(Debug, Resource, Deserialize)]
struct MeshData {
    bones: HashMap<String, Bone>,
}

struct BonePosition {
    start: Vec3,
    end: Vec3,
    orientation: f32,
    slope: f32,
    rotation: f32,
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

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
enum ViewerSystem {
    Camera,
    Labels,
}

#[derive(States, Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
enum ViewerState {
    #[default]
    Loading,
    Ready,
}

impl MeshData {
    fn resolve_value(&self, value: &Value, parent_name: &str) -> f32 {
        match value {
            Value::Absolute(val) => *val,
            Value::Relative { reference, offset } => {
                // Split the reference path into parts
                let parts: Vec<&str> = reference.split('.').collect();
                if parts.len() != 2 {
                    panic!("Invalid reference path: {}", reference);
                }

                // First part should be "connection"
                if parts[0] != "connection" {
                    panic!("Reference must start with 'connection': {}", reference);
                }

                // Get the parent bone
                let parent = self.bones.get(parent_name)
                    .unwrap_or_else(|| panic!("Parent bone not found: {}", parent_name));

                // Get the referenced value based on the second part
                let base_value = match parts[1] {
                    "orientation" => self.resolve_value(&parent.orientation, parent_name),
                    "slope" => self.resolve_value(&parent.slope, parent_name),
                    "rotation" => self.resolve_value(&parent.rotation, parent_name),
                    _ => panic!("Invalid reference property: {}", parts[1]),
                };

                base_value + offset
            }
        }
    }

    fn resolve_positions(&self) -> HashMap<String, BonePosition> {
        let mut positions = HashMap::new();
        let mut last_size = 0;
        
        // First pass: resolve bones without dependencies
        for (name, bone) in &self.bones {
            if bone.connection.is_none() {
                // This is a root bone (like lower_back)
                let orientation = match bone.orientation {
                    Value::Absolute(val) => val,
                    Value::Relative { .. } => panic!("Root bone cannot have relative orientation"),
                };
                let slope = match bone.slope {
                    Value::Absolute(val) => val,
                    Value::Relative { .. } => panic!("Root bone cannot have relative slope"),
                };
                let rotation = match bone.rotation {
                    Value::Absolute(val) => val,
                    Value::Relative { .. } => panic!("Root bone cannot have relative rotation"),
                };

                // Calculate end position based on angles and length
                let direction = calculate_direction(orientation, slope);
                let end = direction * bone.length;

                positions.insert(name.clone(), BonePosition {
                    start: Vec3::ZERO,
                    end,
                    orientation,
                    slope,
                    rotation,
                });
            }
        }

        // Second pass: resolve bones with dependencies
        while positions.len() > last_size {
            last_size = positions.len();
            
            for (name, bone) in &self.bones {
                if !positions.contains_key(name) {
                    if let Some(ref connection) = bone.connection {
                        // Remove the "bones." prefix if it exists
                        let parent_name = connection.strip_prefix("bones.").unwrap_or(connection);
                        
                        if let Some(parent) = positions.get(parent_name) {
                            // Calculate relative values
                            let orientation = self.resolve_value(&bone.orientation, parent_name);
                            let slope = self.resolve_value(&bone.slope, parent_name);
                            let rotation = self.resolve_value(&bone.rotation, parent_name);

                            // Calculate end position
                            let direction = calculate_direction(orientation, slope);
                            let end = parent.end + direction * bone.length;

                            positions.insert(name.clone(), BonePosition {
                                start: parent.end,
                                end,
                                orientation,
                                slope,
                                rotation,
                            });
                        }
                    }
                }
            }
        }

        positions
    }

    fn get_vertices(&self) -> Vec<[f32; 3]> {
        let positions = self.resolve_positions();
        let mut vertices = Vec::new();

        println!("Resolved positions:");
        for (name, position) in positions.iter() {
            println!("Bone {}: start={:?}, end={:?}", name, position.start, position.end);
            vertices.push(position.start.into());
            vertices.push(position.end.into());
        }
        println!("Final vertices: {:?}", vertices);

        vertices
    }

    fn get_indices(&self) -> Vec<u32> {
        let positions = self.resolve_positions();
        let mut indices = Vec::new();
        let mut current_index = 0;

        println!("Creating indices:");
        for _ in positions.values() {
            indices.push(current_index);
            indices.push(current_index + 1);
            println!("Added line: {} -> {}", current_index, current_index + 1);
            current_index += 2;
        }
        println!("Final indices: {:?}", indices);

        indices
    }
}

fn calculate_direction(orientation: f32, slope: f32) -> Vec3 {
    let x = orientation.to_radians().cos() * slope.to_radians().cos();
    let y = slope.to_radians().sin();
    let z = orientation.to_radians().sin() * slope.to_radians().cos();
    Vec3::new(x, y, z)
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_data: Res<MeshData>,
) {
    println!("\n=== Starting scene setup ===");

    // Add cleanup marker to all new entities with scaled transform
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(create_mesh(&mesh_data)),
            material: materials.add(create_material()),
            transform: Transform::from_xyz(0.0, 0.0, 0.0)
                .with_scale(Vec3::splat(5.0)),
            ..default()
        },
        CleanupMarker,
    ));

    // Add 3D camera
    commands.spawn((
        Camera3dBundle {
            transform: Transform::from_xyz(-10.0, 10.0, 20.0).looking_at(Vec3::ZERO, Vec3::Y),
            camera: Camera {
                order: 0,
                ..default()
            },
            ..default()
        },
        CleanupMarker,
    ));

    // Add label quads at bone positions
    let positions = mesh_data.resolve_positions();
    let scale = 5.0;
    
    println!("\nCreating labels:");
    for (name, position) in positions.iter() {
        let end = position.end * scale;
        println!("Label '{}': spawn_position={:?}", name, end);
        
        // Spawn the background quad
        commands.spawn((
            PbrBundle {
                mesh: meshes.add(Mesh::from(shape::Quad::new(Vec2::new(2.0, 0.5)))),
                material: materials.add(StandardMaterial {
                    base_color: Color::rgba(0.0, 0.0, 0.0, 0.5),
                    emissive: Color::rgba(0.0, 0.0, 0.0, 1.0),
                    unlit: true,
                    double_sided: true,
                    alpha_mode: AlphaMode::Blend,
                    ..default()
                }),
                transform: Transform::from_translation(end + Vec3::new(0.0, 0.5, 0.0))
                    .with_rotation(Quat::from_rotation_y(std::f32::consts::FRAC_PI_2)),
                visibility: Visibility::Visible,
                ..default()
            },
            BoneLabel {
                bone_name: name.clone(),
                world_position: end + Vec3::new(0.0, 0.5, 0.0),
            },
            CleanupMarker,
        ));
    }

    // Mark scene as ready
    commands.spawn((SceneReady, CleanupMarker));
    commands.insert_resource(NextState(Some(ViewerState::Ready)));
    println!("Scene setup complete");
}

fn cleanup_scene(
    mut commands: Commands,
    to_cleanup: Query<Entity, With<CleanupMarker>>,
) {
    println!("Cleaning up scene...");
    for entity in to_cleanup.iter() {
        commands.entity(entity).despawn_recursive();
    }
    commands.insert_resource(NextState(Some(ViewerState::Loading)));
    println!("Scene cleanup complete");
}

fn check_file_changes(
    mut commands: Commands,
    mut file_watcher: ResMut<FileWatcherResource>,
    mesh_file: Res<MeshFile>,
    export_path: Res<ExportPath>,
    to_cleanup: Query<Entity, With<CleanupMarker>>,
) {
    if let Ok(metadata) = std::fs::metadata(&file_watcher.path) {
        if let Ok(modified) = metadata.modified() {
            if modified > file_watcher.last_modified {
                println!("File changed, cleaning up scene...");
                cleanup_scene(commands, to_cleanup);
                
                println!("Reloading...");
                file_watcher.last_modified = modified;

                // Read and parse the TOML file
                if let Ok(mesh_toml) = std::fs::read_to_string(&mesh_file.0) {
                    if let Ok(mesh_data) = toml::from_str::<MeshData>(&mesh_toml) {
                        // Export to GLB
                        if let Err(e) = export_to_glb(&mesh_data, &export_path.0) {
                            eprintln!("Failed to export GLB: {}", e);
                        }
                    }
                }
            }
        }
    }
}

fn orbit_camera(
    time: Res<Time>,
    mut query: Query<&mut Transform, (With<Camera3d>, Without<UiCamera>)>,
    window_query: Query<&Window, With<PrimaryWindow>>,
    mouse_button: Res<Input<MouseButton>>,
    mut mouse_motion: EventReader<MouseMotion>,
) {
    let window = window_query.get_single().unwrap();
    
    for mut transform in query.iter_mut() {
        if mouse_button.pressed(MouseButton::Left) {
            // Manual camera control with mouse drag
            for ev in mouse_motion.read() {
                let rotation_speed = 0.5;
                let angle_x = ev.delta.x * rotation_speed * time.delta_seconds();
                let angle_y = ev.delta.y * rotation_speed * time.delta_seconds();

                // Get the camera's current position
                let distance = transform.translation.length();
                
                // Rotate around global Y axis first
                transform.rotate_around(
                    Vec3::ZERO,
                    Quat::from_rotation_y(-angle_x),
                );
                
                // Then rotate around global X axis
                transform.rotate_around(
                    Vec3::ZERO,
                    Quat::from_rotation_x(-angle_y),
                );
                
                // Maintain distance
                let direction = transform.translation.normalize();
                transform.translation = direction * distance;
                
                // Look at center
                transform.look_at(Vec3::ZERO, Vec3::Y);
            }
        } else if !window.cursor.visible {
            // Automatic rotation when cursor is hidden
            let angle = time.elapsed_seconds() * 0.5;
            transform.translation = Vec3::new(
                angle.cos() * 20.0,  // Increased orbit radius
                10.0,                // Increased height
                angle.sin() * 20.0,  // Increased orbit radius
            );
            transform.look_at(Vec3::ZERO, Vec3::Y);
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

fn draw_debug_lines(
    mut gizmos: Gizmos,
    mesh_data: Res<MeshData>,
    labels: Query<(&Transform, &BoneLabel)>,
) {
    let positions = mesh_data.resolve_positions();
    let scale = 5.0;
    
    for (name, position) in positions.iter() {
        let end = position.end * scale;
        
        // Draw text markers at label positions
        let size = 0.1;
        gizmos.line(
            end + Vec3::new(-size, 0.5, 0.0),
            end + Vec3::new(size, 0.5, 0.0),
            Color::WHITE,
        );
        gizmos.line(
            end + Vec3::new(0.0, 0.5 - size, 0.0),
            end + Vec3::new(0.0, 0.5 + size, 0.0),
            Color::WHITE,
        );
        gizmos.line(
            end + Vec3::new(0.0, 0.5, -size),
            end + Vec3::new(0.0, 0.5, size),
            Color::WHITE,
        );
    }
}

fn create_mesh(mesh_data: &MeshData) -> Mesh {
    let mut mesh = Mesh::new(PrimitiveTopology::LineList);
    let vertices = mesh_data.get_vertices();
    let indices = mesh_data.get_indices();
    
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices);
    mesh.set_indices(Some(Indices::U32(indices)));
    mesh
}

fn create_material() -> StandardMaterial {
    StandardMaterial {
        base_color: Color::YELLOW,
        unlit: true,
        double_sided: true,  // Add this to make lines visible from both sides
        ..default()
    }
}

fn update_labels(
    mut labels: Query<(&mut Transform, &BoneLabel)>,
    camera: Query<&GlobalTransform, With<Camera3d>>,
) {
    let Ok(camera_transform) = camera.get_single() else { return };

    for (mut label_transform, _) in labels.iter_mut() {
        let to_camera = camera_transform.translation() - label_transform.translation;
        let angle = f32::atan2(to_camera.x, to_camera.z);
        label_transform.rotation = Quat::from_rotation_y(angle);
    }
}

fn main() {
    // Get command line arguments
    let args: Vec<String> = std::env::args().collect();
    let mesh_file = args.get(1).expect("Please provide a mesh file path");
    let export_path = args.get(2).expect("Please provide an export path");

    // Read and parse the TOML file
    let mesh_toml = std::fs::read_to_string(mesh_file)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {}", mesh_file, e));
    
    let mesh_data: MeshData = toml::from_str(&mesh_toml)
        .unwrap_or_else(|e| panic!("Failed to parse TOML from {}: {}", mesh_file, e));

    App::new()
        .add_plugins(DefaultPlugins.set(LogPlugin {
            filter: "info,wgpu_core=warn,wgpu_hal=warn".into(),
            level: bevy::log::Level::INFO,
        }))
        .add_state::<ViewerState>()
        .configure_sets(Update, (
            ViewerSystem::Camera,
            ViewerSystem::Labels,
        ).chain())
        .add_systems(Startup, setup)
        .add_systems(Update, (
            setup_camera.before(ViewerSystem::Camera),
            orbit_camera.in_set(ViewerSystem::Camera),
            update_labels.after(ViewerSystem::Camera),
            draw_debug_lines,
        ))
        // Only run cleanup when file changes
        .add_systems(Update, (
            check_file_changes,
            bevy::window::close_on_esc,
        ))
        .insert_resource(FileWatcherResource::new(PathBuf::from(mesh_file)))
        .insert_resource(ExportPath(PathBuf::from(export_path)))
        .insert_resource(mesh_data)
        .insert_resource(MeshFile(PathBuf::from(mesh_file)))
        .run();
}