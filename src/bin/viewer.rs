use bevy::{
    prelude::*,
    render::{
        mesh::Indices,
        render_resource::PrimitiveTopology,
        render_asset::RenderAssetUsages,
    },
    window::PrimaryWindow,
};
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
use std::{
    collections::{BTreeMap, HashMap},
    path::{PathBuf, Path},
    time::SystemTime,
    sync::Once,
};
use serde::Deserialize;
use bevy::input::mouse::MouseMotion;
use hooded_crow_modeller::{Model, BoneGroup};

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

#[derive(Debug, Resource, Deserialize)]
struct MeshData {
    groups: HashMap<String, BoneGroup>,
}

impl From<Model> for MeshData {
    fn from(model: Model) -> Self {
        Self {
            groups: model.groups,
        }
    }
}

#[derive(Clone, Debug)]
struct BonePosition {
    pub start: Vec3,
    pub end: Vec3,
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
    Ready,
}

impl MeshData {
    fn resolve_positions(&self) -> HashMap<String, BonePosition> {
        static mut POSITIONS: Option<HashMap<String, BonePosition>> = None;
        static INIT: Once = Once::new();

        unsafe {
            INIT.call_once(|| {
                let mut positions = HashMap::new();
                let mut last_size = 0;
                
                // Process all bones until no new bones are added
                while {
                    let start_size = positions.len();
                    
                    // Helper function to process bones in a group and its subgroups
                    fn process_group_bones(
                        group: &BoneGroup,
                        positions: &mut HashMap<String, BonePosition>
                    ) {
                        // Process bones in current group
                        for bone in &group.bones {
                            if !positions.contains_key(&bone.name) {
                                let parent_end = if bone.parent.is_empty() {
                                    None
                                } else {
                                    let parent_name = bone.parent.strip_prefix("bones.").unwrap_or(&bone.parent);
                                    positions.get(parent_name).map(|p| p.end)
                                };
                                
                                if parent_end.is_some() || bone.parent.is_empty() {
                                    let direction = calculate_direction(bone.orientation, bone.slope);
                                    let start = parent_end.unwrap_or(Vec3::ZERO);
                                    let end = start + direction * bone.length;
                                    
                                    positions.insert(bone.name.clone(), BonePosition { start, end });
                                }
                            }
                        }

                        // Process bones in subgroups
                        for (_, subgroup) in &group.subgroups {
                            process_group_bones(subgroup, positions);
                        }
                    }

                    // Process all groups
                    for (_, group) in &self.groups {
                        process_group_bones(group, &mut positions);
                    }

                    // Continue if we added any new bones
                    positions.len() > start_size
                } {}

                POSITIONS = Some(positions);
            });

            (*POSITIONS.as_ref().unwrap()).clone()
        }
    }

    fn get_vertices(&self) -> Vec<[f32; 3]> {
        // Use cached positions to generate vertices
        let positions = self.resolve_positions();
        let mut vertices = Vec::new();
        
        for position in positions.values() {
            vertices.push(position.start.into());
            vertices.push(position.end.into());
        }
        
        vertices
    }

    fn get_indices(&self) -> Vec<u32> {
        // Use cached positions to generate indices
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
    let orientation_rad = orientation.to_radians();
    let slope_rad = slope.to_radians();

    Vec3::new(
        slope_rad.sin() * orientation_rad.sin(),
        slope_rad.cos(),
        slope_rad.sin() * orientation_rad.cos()
    )
}

pub fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mesh_data: Res<MeshData>,
    show_axes: Res<ShowAxes>,
    show_labels: Res<ShowLabels>,
) {
    println!("\n=== Starting scene setup ===");

    // Add coordinate axes only if enabled
    if show_axes.0 {
        spawn_coordinate_axes(&mut commands, &mut meshes, &mut materials);
    }

    // Add bones
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

    // Add 3D camera looking straight down Z axis
    commands.spawn((
        Camera3dBundle {
            transform: Transform::from_xyz(0.0, 0.0, 20.0)
                .looking_at(Vec3::ZERO, Vec3::Y),
            ..default()
        },
        CleanupMarker,
    ));

    // Mark scene as ready
    commands.spawn((SceneReady, CleanupMarker));
    commands.insert_resource(NextState(Some(ViewerState::Ready)));
    println!("Scene setup complete");
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
    buttons: Res<ButtonInput<MouseButton>>,
    mut mouse_motion: EventReader<MouseMotion>,
) {
    let window = window_query.get_single().unwrap();
    
    for mut transform in query.iter_mut() {
        if buttons.pressed(MouseButton::Left) {
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
                angle.cos() * 20.0,
                10.0,
                angle.sin() * 20.0,
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

    let mut ctx = egui_contexts.ctx_mut();
    
    for (name, position) in positions.iter() {
        if let Some(screen_pos) = world_to_screen(
            window,
            camera,
            camera_transform,
            position.end * scale,
        ) {
            egui::Area::new(egui::Id::new(name))
                .fixed_pos(egui::pos2(screen_pos.x, screen_pos.y))
                .show(ctx, |ui| {
                    ui.style_mut().text_styles.get_mut(&egui::TextStyle::Body).unwrap().size = 8.0;
                    ui.colored_label(egui::Color32::WHITE, name);  // Changed to white color
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
            0.25,
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
    
    let mesh_data: MeshData = toml::from_str(&mesh_toml)
        .unwrap_or_else(|e| panic!("Failed to parse TOML from {}: {}", mesh_file, e));

    App::new()
        .add_plugins((
            DefaultPlugins,
            EguiPlugin,
        ))
        .init_state::<ViewerState>()
        .insert_resource(FileWatcherResource::new(PathBuf::from(mesh_file)))
        .insert_resource(ExportPath(PathBuf::from(export_path)))
        .insert_resource(mesh_data)
        .insert_resource(MeshFile(PathBuf::from(mesh_file)))
        .insert_resource(ShowAxes(show_axes))
        .insert_resource(ShowLabels(show_labels))
        .add_systems(Startup, setup)
        .add_systems(Update, (
            setup_camera,
            orbit_camera,
            draw_labels,
            draw_joints,
            check_file_changes,
        ))
        .run();
}