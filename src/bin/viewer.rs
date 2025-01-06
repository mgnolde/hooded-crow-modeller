use bevy::{
    prelude::*,
    render::{mesh::Indices, render_resource::PrimitiveTopology},
    window::PrimaryWindow,
    log::LogPlugin,
    input::mouse::MouseMotion,
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

impl MeshData {
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
                        if let Some(parent) = positions.get(connection) {
                            // Calculate relative values
                            let orientation = self.resolve_value(&bone.orientation, parent.orientation);
                            let slope = self.resolve_value(&bone.slope, parent.slope);
                            let rotation = self.resolve_value(&bone.rotation, parent.rotation);

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

    fn resolve_value(&self, value: &Value, parent_value: f32) -> f32 {
        match value {
            Value::Absolute(val) => *val,
            Value::Relative { offset, .. } => parent_value + offset,
        }
    }

    fn get_vertices(&self) -> Vec<[f32; 3]> {
        let positions = self.resolve_positions();
        let mut vertices = Vec::new();

        for position in positions.values() {
            vertices.push(position.start.into());
            vertices.push(position.end.into());
        }

        vertices
    }

    fn get_indices(&self) -> Vec<u32> {
        let positions = self.resolve_positions();
        let mut indices = Vec::new();
        let mut current_index = 0;

        for _ in positions.values() {
            indices.push(current_index);
            indices.push(current_index + 1);
            current_index += 2;
        }

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
    // Create the mesh
    let mut mesh = Mesh::new(PrimitiveTopology::LineList);

    // Set vertex positions
    let vertices = mesh_data.get_vertices();
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices);

    // Set indices
    let indices = mesh_data.get_indices();
    mesh.set_indices(Some(Indices::U32(indices)));

    // Spawn the mesh
    commands.spawn(PbrBundle {
        mesh: meshes.add(mesh),
        material: materials.add(Color::rgb(0.8, 0.7, 0.6).into()),
        transform: Transform::from_xyz(0.0, 0.0, 0.0),
        ..default()
    });

    // Add a light
    commands.spawn(PointLightBundle {
        point_light: PointLight {
            intensity: 1500.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(4.0, 8.0, 4.0),
        ..default()
    });

    // Add a camera
    commands.spawn(Camera3dBundle {
        transform: Transform::from_xyz(-2.0, 2.5, 5.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    });
}

fn check_file_changes(
    mut file_watcher: ResMut<FileWatcherResource>,
    mesh_file: Res<MeshFile>,
    export_path: Res<ExportPath>,
) {
    if let Ok(metadata) = std::fs::metadata(&file_watcher.path) {
        if let Ok(modified) = metadata.modified() {
            if modified > file_watcher.last_modified {
                println!("File changed, reloading...");
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
    mut query: Query<&mut Transform, With<Camera>>,
    window_query: Query<&Window, With<PrimaryWindow>>,
    mouse_button: Res<Input<MouseButton>>,
    mut mouse_motion: EventReader<MouseMotion>,
) {
    let window = window_query.get_single().unwrap();
    
    for mut transform in query.iter_mut() {
        if mouse_button.pressed(MouseButton::Left) {
            // Manual camera control with mouse drag
            for ev in mouse_motion.iter() {
                let rotation_speed = 0.5;
                let mut angle_x = ev.delta.x * rotation_speed * time.delta_seconds();
                let mut angle_y = ev.delta.y * rotation_speed * time.delta_seconds();
                
                // Clamp vertical rotation to prevent flipping
                angle_y = angle_y.clamp(-1.0, 1.0);
                angle_x = angle_x.clamp(-1.0, 1.0);

                // Rotate around Y axis
                transform.rotate_y(-angle_x);
                
                // Rotate around local X axis
                transform.rotate_local_x(-angle_y);
            }
        } else if !window.cursor.visible {
            // Automatic rotation when cursor is hidden
            let angle = time.elapsed_seconds() * 0.5;
            transform.translation.x = angle.cos() * 5.0;
            transform.translation.z = angle.sin() * 5.0;
            transform.look_at(Vec3::ZERO, Vec3::Y);
        }
    }
}

fn update_vertex_labels(
    mut gizmos: Gizmos,
    mesh_data: Res<MeshData>,
) {
    let positions = mesh_data.resolve_positions();
    
    for (_name, position) in positions.iter() {
        // Draw a small cross at each bone end point
        let size = 0.1;
        gizmos.line(
            position.end + Vec3::new(-size, 0.0, 0.0),
            position.end + Vec3::new(size, 0.0, 0.0),
            Color::WHITE,
        );
        gizmos.line(
            position.end + Vec3::new(0.0, -size, 0.0),
            position.end + Vec3::new(0.0, size, 0.0),
            Color::WHITE,
        );
        gizmos.line(
            position.end + Vec3::new(0.0, 0.0, -size),
            position.end + Vec3::new(0.0, 0.0, size),
            Color::WHITE,
        );
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
            filter: "error".into(),
            level: bevy::log::Level::ERROR,
        }))
        .insert_resource(FileWatcherResource::new(PathBuf::from(mesh_file)))
        .insert_resource(ExportPath(PathBuf::from(export_path)))
        .insert_resource(mesh_data)
        .insert_resource(MeshFile(PathBuf::from(mesh_file)))
        .add_systems(Startup, setup)
        .add_systems(Update, (
            bevy::window::close_on_esc,
            check_file_changes.after(bevy::window::close_on_esc),
            orbit_camera.after(check_file_changes),
            update_vertex_labels.after(orbit_camera),
        ))
        .run();
}
