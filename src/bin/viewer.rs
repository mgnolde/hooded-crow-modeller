use bevy::{
    prelude::*,
    sprite::Sprite,
    text::{Text2dBundle, Text, TextStyle},
    input::mouse::{MouseWheel, MouseMotion},
    log::LogPlugin,
};
use clap::Parser;
use std::{
    fs,
    collections::BTreeMap,
    path::PathBuf,
};
use gltf::json::{self, validation::Checked};
use gltf_json::{
    mesh,
    validation::USize64,
    Index,
};
use toml;
use bevy::render::mesh::{Indices, Mesh};
use bevy::render::render_resource::PrimitiveTopology;
use serde::Deserialize;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input mesh JSON file
    mesh_file: String,

    /// Optional: Export to GLB file
    #[arg(short, long)]
    export: Option<PathBuf>,
}

#[derive(serde::Deserialize)]
struct PointsFile(String);

#[derive(Debug, Deserialize)]
pub struct MeshData {
    vertices: Vec<Vec<f32>>,
    indices: Vec<u32>,
    metadata: Vec<Metadata>,
}

#[derive(Debug, Deserialize)]
pub struct Metadata {
    text: String,
}

#[derive(Component)]
struct FileWatcher {
    last_modified: std::time::SystemTime,
}

#[derive(Component)]
struct OrbitCamera {
    pub focus: Vec3,
    pub radius: f32,
    pub upside_down: bool,
}

#[derive(Resource)]
struct MeshFilePath(String);

#[derive(Resource)]
struct ExportPath(Option<PathBuf>);

#[derive(Component)]
struct MetadataLabel;

#[derive(Component)]
struct Billboard;

#[derive(Component)]
struct VertexLabel {
    vertex_index: usize,
    world_position: Vec3,
}

fn main() {
    // Configure logging to be less verbose
    let mut app = App::new();
    app.add_plugins(DefaultPlugins.set(LogPlugin {
        filter: "error".into(),
        level: bevy::log::Level::ERROR,
    }));

    let args = Args::parse();
    let mesh_file = args.mesh_file.clone();

    app.insert_resource(MeshFilePath(mesh_file))
        .insert_resource(ExportPath(args.export))
        .add_systems(Startup, (setup, spawn_mesh))
        .add_systems(Update, (
            bevy::window::close_on_esc,
            check_file_changes,
            orbit_camera,
            update_vertex_labels,
            check_scene,
        ))
        .run();
}

fn check_file_changes(
    mesh_file: Res<MeshFilePath>,
    export_path: Res<ExportPath>,
    mut commands: Commands,
    meshes: ResMut<Assets<Mesh>>,
    materials: ResMut<Assets<StandardMaterial>>,
    asset_server: Res<AssetServer>,
    query: Query<(Entity, &FileWatcher)>,
) {
    let path = std::path::Path::new(&mesh_file.0);
    if let Ok(metadata) = std::fs::metadata(path) {
        if let Ok(last_modified) = metadata.modified() {
            if let Ok((entity, watcher)) = query.get_single() {
                if last_modified > watcher.last_modified {
                    commands.entity(entity).despawn();
                    spawn_mesh(commands, meshes, materials, mesh_file, export_path, asset_server);
                }
            } else {
                spawn_mesh(commands, meshes, materials, mesh_file, export_path, asset_server);
            }
        }
    }
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    println!("Setting up scene...");

    // Camera with orbit controls
    commands.spawn((
        Camera3dBundle {
            transform: Transform::from_xyz(0.0, 5.0, 10.0).looking_at(Vec3::ZERO, Vec3::Y),
            ..default()
        },
        OrbitCamera {
            radius: 10.0,
            focus: Vec3::ZERO,
            upside_down: false,
        },
    ));

    commands.spawn(DirectionalLightBundle {
        directional_light: DirectionalLight {
            illuminance: 5000.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(4.0, 8.0, 4.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    });

    // Keep the red cube for reference
    let cube = meshes.add(Mesh::from(shape::Cube::new(0.5))); // Made smaller
    let red_material = materials.add(StandardMaterial {
        base_color: Color::RED,
        ..default()
    });

    commands.spawn(PbrBundle {
        mesh: cube,
        material: red_material,
        transform: Transform::from_xyz(2.0, 0.0, 0.0), // Moved to the side
        ..default()
    });
}

// Add diagnostic system
fn check_scene(
    cameras: Query<&Camera3d>,
    lights: Query<&DirectionalLight>,
    meshes: Query<&Handle<Mesh>>,
    materials: Query<&Handle<StandardMaterial>>,
) {
    println!("\nScene Diagnostic:");
    println!("Cameras: {}", cameras.iter().count());
    println!("Lights: {}", lights.iter().count());
    println!("Mesh entities: {}", meshes.iter().count());
    println!("Material entities: {}", materials.iter().count());
}

fn export_to_glb(mesh_data: &MeshData, output_path: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    use std::fs::File;
    use std::io::Write;

    // Create buffer with vertex positions and indices
    let positions_vec: Vec<u8> = mesh_data.vertices
        .iter()
        .flat_map(|v| v.iter().flat_map(|f| f.to_le_bytes()))
        .collect();
    let indices_vec: Vec<u8> = mesh_data.indices
        .iter()
        .flat_map(|i| i.to_le_bytes())
        .collect();

    // Combine buffers
    let buffer_length = positions_vec.len() + indices_vec.len();
    let mut combined_buffer = Vec::with_capacity(buffer_length);
    combined_buffer.extend_from_slice(&positions_vec);
    combined_buffer.extend_from_slice(&indices_vec);

    // Create buffer views
    let position_view = json::buffer::View {
        buffer: json::Index::new(0),
        byte_length: json::validation::USize64::from(positions_vec.len()),
        byte_offset: Some(json::validation::USize64::from(0_usize)),
        byte_stride: None,
        name: Some(String::from("Positions")),
        target: Some(Checked::Valid(json::buffer::Target::ArrayBuffer)),
        extensions: None,
        extras: Default::default(),
    };

    let indices_view = json::buffer::View {
        buffer: json::Index::new(0),
        byte_length: json::validation::USize64::from(indices_vec.len()),
        byte_offset: Some(json::validation::USize64::from(positions_vec.len())),
        byte_stride: None,
        name: Some(String::from("Indices")),
        target: Some(Checked::Valid(json::buffer::Target::ElementArrayBuffer)),
        extensions: None,
        extras: Default::default(),
    };

    // Create accessors
    let position_accessor = json::Accessor {
        buffer_view: Some(json::Index::new(0)),
        byte_offset: Some(json::validation::USize64::from(0_usize)),
        count: json::validation::USize64::from(mesh_data.vertices.len()),
        component_type: Checked::Valid(json::accessor::GenericComponentType(
            json::accessor::ComponentType::F32
        )),
        type_: Checked::Valid(json::accessor::Type::Vec3),
        min: Some(json::Value::from(vec![-1.0, -1.0, -1.0])),
        max: Some(json::Value::from(vec![1.0, 1.0, 1.0])),
        normalized: false,
        sparse: None,
        name: Some(String::from("Positions")),
        extensions: None,
        extras: Default::default(),
    };

    let indices_accessor = json::Accessor {
        buffer_view: Some(json::Index::new(1)),
        byte_offset: Some(json::validation::USize64::from(0_usize)),
        count: json::validation::USize64::from(mesh_data.indices.len()),
        component_type: Checked::Valid(json::accessor::GenericComponentType(
            json::accessor::ComponentType::U32
        )),
        type_: Checked::Valid(json::accessor::Type::Scalar),
        min: None,
        max: None,
        normalized: false,
        sparse: None,
        name: Some(String::from("Indices")),
        extensions: None,
        extras: Default::default(),
    };

    // Create primitive
    let mut attributes = BTreeMap::new();
    attributes.insert(
        Checked::Valid(json::mesh::Semantic::Positions),
        json::Index::new(0),
    );

    let primitive = json::mesh::Primitive {
        attributes,
        indices: Some(json::Index::new(1)),
        material: None,
        mode: Checked::Valid(json::mesh::Mode::Triangles),
        targets: None,
        extensions: None,
        extras: Default::default(),
    };

    // Create mesh
    let mesh = json::Mesh {
        primitives: vec![primitive],
        weights: None,
        name: Some(String::from("Mesh")),
        extensions: None,
        extras: Default::default(),
    };

    // Create node
    let node = json::Node {
        camera: None,
        children: None,
        extensions: None,
        extras: Default::default(),
        matrix: None,
        mesh: Some(json::Index::new(0)),
        name: None,
        rotation: None,
        scale: None,
        translation: None,
        weights: None,
        skin: None,
    };

    // Create scene
    let scene = json::Scene {
        nodes: vec![json::Index::new(0)],
        name: None,
        extensions: None,
        extras: Default::default(),
    };

    // Create root
    let root = json::Root {
        accessors: vec![position_accessor, indices_accessor],
        buffers: vec![json::Buffer {
            byte_length: json::validation::USize64::from(buffer_length),
            uri: None,
            name: None,
            extensions: None,
            extras: Default::default(),
        }],
        buffer_views: vec![position_view, indices_view],
        meshes: vec![mesh],
        nodes: vec![node],
        scenes: vec![scene],
        scene: Some(json::Index::new(0)),
        ..Default::default()
    };

    // Create binary glTF
    let json_string = json::serialize::to_string(&root)?;
    
    // GLB header (magic + version + length)
    let mut glb = vec![
        b'g', b'l', b'T', b'F',  // magic
        2, 0, 0, 0,              // version
        0, 0, 0, 0,              // length (will be filled later)
    ];

    // JSON chunk header
    let json_len = json_string.len();
    let json_pad = (4 - (json_len % 4)) % 4;
    glb.extend_from_slice(&(json_len as u32).to_le_bytes());
    glb.extend_from_slice(b"JSON");
    
    // JSON content
    glb.extend_from_slice(json_string.as_bytes());
    glb.extend_from_slice(&vec![0u8; json_pad]);

    // Binary chunk header
    let bin_len = combined_buffer.len();
    let bin_pad = (4 - (bin_len % 4)) % 4;
    glb.extend_from_slice(&(bin_len as u32).to_le_bytes());
    glb.extend_from_slice(b"BIN\0");
    
    // Binary content
    glb.extend_from_slice(&combined_buffer);
    glb.extend_from_slice(&vec![0u8; bin_pad]);

    // Write total length
    let total_length = glb.len() as u32;
    glb[8..12].copy_from_slice(&total_length.to_le_bytes());

    // Write to file
    let mut file = File::create(output_path)?;
    file.write_all(&glb)?;

    Ok(())
}

fn spawn_mesh(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_file: Res<MeshFilePath>,
    export_path: Res<ExportPath>,
    asset_server: Res<AssetServer>,
) {
    let path = std::path::Path::new(&mesh_file.0);
    let last_modified = std::fs::metadata(path)
        .expect("Failed to read metadata")
        .modified()
        .expect("Failed to get modification time");

    // Read mesh data from TOML file
    let mesh_toml = fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {}", path.display(), e));
    
    let mesh_data: MeshData = toml::from_str(&mesh_toml)
        .unwrap_or_else(|e| panic!("Failed to parse TOML from {}: {}", path.display(), e));

    // Convert to Bevy mesh format
    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);
    let positions: Vec<[f32; 3]> = mesh_data.vertices
        .iter()
        .map(|v| [v[0], v[1], v[2]])
        .collect();
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.set_indices(Some(Indices::U32(mesh_data.indices.clone())));

    // Spawn the mesh with a distinct material
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(mesh),
            material: materials.add(StandardMaterial {
                base_color: Color::rgb(0.0, 0.8, 0.0),  // Green color
                emissive: Color::rgb(0.0, 0.3, 0.0),    // Slight glow
                metallic: 0.0,
                perceptual_roughness: 0.2,
                ..default()
            }),
            transform: Transform::from_xyz(0.0, 0.0, 0.0),
            visibility: Visibility::Visible,
            ..default()
        },
        FileWatcher {
            last_modified,
        },
    ));

    // Spawn vertex labels
    for (i, vertex) in mesh_data.vertices.iter().enumerate() {
        let label_text = if i < mesh_data.metadata.len() {
            mesh_data.metadata[i].text.clone()
        } else {
            format!("Vertex {}", i)
        };

        let world_pos = Vec3::new(vertex[0], vertex[1], vertex[2]);

        commands.spawn((
            NodeBundle {
                style: Style {
                    position_type: PositionType::Absolute,
                    left: Val::Px(0.0),
                    top: Val::Px(0.0),
                    padding: UiRect::all(Val::Px(5.0)),
                    ..default()
                },
                background_color: BackgroundColor(Color::rgba(0.0, 0.0, 0.0, 0.5)),
                visibility: Visibility::Visible,
                ..default()
            },
            VertexLabel {
                vertex_index: i,
                world_position: world_pos,
            },
        ))
        .with_children(|parent| {
            parent.spawn(TextBundle::from_section(
                label_text,
                TextStyle {
                    font_size: 16.0,
                    color: Color::WHITE,
                    ..default()
                },
            ));
        });
    }

    // Handle export if path is provided
    if let Some(export_path) = &export_path.0 {
        if let Err(e) = export_to_glb(&mesh_data, export_path) {
            eprintln!("Failed to export GLB: {}", e);
        } else {
            println!("Successfully exported GLB to {:?}", export_path);
        }
    }
}

fn orbit_camera(
    mut query: Query<(&mut Transform, &mut OrbitCamera)>,
    mut mouse_motion: EventReader<MouseMotion>,
    mut mouse_wheel: EventReader<MouseWheel>,
    mouse_buttons: Res<Input<MouseButton>>,
    time: Res<Time>,
) {
    let orbit_speed = 1.0;
    let zoom_speed = 1.0;
    let pan_speed = 1.0;

    for (mut transform, mut orbit) in query.iter_mut() {
        // Handle mouse orbit
        if mouse_buttons.pressed(MouseButton::Left) {
            for ev in mouse_motion.read() {
                let (mut yaw, mut pitch, _) = transform.rotation.to_euler(EulerRot::YXZ);
                yaw -= ev.delta.x * orbit_speed * time.delta_seconds();
                pitch -= ev.delta.y * orbit_speed * time.delta_seconds();
                pitch = pitch.clamp(-1.5, 1.5);
                
                transform.rotation = Quat::from_euler(EulerRot::YXZ, yaw, pitch, 0.0);
            }
        }

        // Handle mouse zoom
        for ev in mouse_wheel.read() {
            orbit.radius -= ev.y * zoom_speed;
            orbit.radius = orbit.radius.clamp(1.0, 100.0);
        }

        // Handle mouse pan
        if mouse_buttons.pressed(MouseButton::Right) {
            for ev in mouse_motion.read() {
                let right = transform.right();
                let up = transform.up();
                orbit.focus += right * (-ev.delta.x * pan_speed * time.delta_seconds())
                    + up * (-ev.delta.y * pan_speed * time.delta_seconds());
            }
        }

        // Update transform
        let rot_matrix = Mat3::from_quat(transform.rotation);
        transform.translation = orbit.focus + rot_matrix.mul_vec3(Vec3::new(0.0, 0.0, orbit.radius));
    }
}

fn update_vertex_labels(
    mut labels: Query<(&mut Style, &VertexLabel)>,
    camera_3d: Query<(&Camera, &GlobalTransform), (With<Camera3d>, Without<Camera2d>)>,
    windows: Query<&Window>,
) {
    // Get the 3D camera and window
    let Ok((camera, camera_transform)) = camera_3d.get_single() else { return };
    let Ok(window) = windows.get_single() else { return };

    for (mut style, label) in labels.iter_mut() {
        // Convert world position to screen position
        if let Some(screen_pos) = camera.world_to_viewport(
            camera_transform,
            label.world_position
        ) {
            style.left = Val::Px(screen_pos.x);
            style.top = Val::Px(screen_pos.y);
        }
    }
}

fn debug_mesh(
    meshes: Query<&Handle<Mesh>>,
) {
    if meshes.is_empty() {
        println!("No mesh entities found in the scene");
    } else {
        println!("Found {} mesh entities", meshes.iter().count());
    }
}