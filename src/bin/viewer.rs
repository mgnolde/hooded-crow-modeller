use bevy::{
    prelude::*,
    render::camera::Camera,
    input::{
        mouse::{MouseMotion, MouseWheel, MouseButton},
        keyboard::KeyCode,
    },
    gizmos::gizmos::Gizmos,
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
use hooded_crow_modeller::{Model, Group, Bone};
use bevy_egui::{egui, EguiPlugin, EguiContexts, EguiStartupSet};
use gltf::json::{self, validation::{USize64, Checked}};
use gltf::json::buffer::Stride;
use bytemuck;
use serde_json;

#[derive(Debug, Clone)]
struct BonePosition {
    start: Vec3,
    end: Vec3,
    color: [f32; 3],
}

#[derive(Resource, Clone)]
struct MeshData {
    positions: HashMap<String, BonePosition>,
}

impl Default for MeshData {
    fn default() -> Self {
        Self {
            positions: HashMap::new(),
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
}

#[derive(Resource)]
struct ColorCache {
    colors: BTreeMap<String, [f32; 3]>,
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
struct LastUpdateTime(f64);

fn calculate_transform(orientation: f32, slope: f32, rotation: f32) -> Mat4 {
    // Convert angles to radians
    let orientation_rad = orientation.to_radians();
    let slope_rad = slope.to_radians();
    let rotation_rad = rotation.to_radians();

    // Create rotation matrices
    let orientation_rot = Mat4::from_rotation_y(orientation_rad); // Around Y for orientation
    let slope_rot = Mat4::from_rotation_x(slope_rad); // Around X for slope
    let rotation_rot = Mat4::from_rotation_z(rotation_rad); // Around Z for rotation

    // Combine transformations: first orientation, then slope, then rotation
    orientation_rot * slope_rot * rotation_rot
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
        let start = if bone_path == "body.lower_spine" {
            println!("Root bone, starting at origin");
            Vec3::ZERO // Only the root bone starts at origin
        } else {
            // Get parent's end position from the positions map
            match positions.get(parent_path) {
                Some((start, end)) => {
                    println!("Found parent {} at end: {:?}", parent_path, end);
                    *end
                }
                None => {
                    println!("Parent {} not found, using zero", parent_path);
                    Vec3::ZERO
                }
            }
        };

        // Calculate bone direction based on angles
        let orientation_rad = bone.orientation.to_radians();
        let slope_rad = bone.slope.to_radians();
        
        // Start with forward direction (along Z)
        let mut direction = Vec3::new(0.0, 0.0, 1.0);
        
        // Apply orientation (rotation around Y axis)
        direction = Vec3::new(
            direction.z * orientation_rad.sin(),
            direction.y,
            direction.z * orientation_rad.cos(),
        ).normalize();
        
        // Apply slope (rotation around local X axis)
        direction = Vec3::new(
            direction.x,
            direction.z * slope_rad.sin(),
            direction.z * slope_rad.cos(),
        ).normalize();

        // Calculate end position
        let end = start + direction * bone.length;
        println!("Bone {} from {:?} to {:?}", bone_path, start, end);

        // Store positions
        positions.insert(bone_path.clone(), (start, end));

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
                    parent_transform,
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

fn get_rainbow_color(depth: usize, max_depth: usize) -> [f32; 3] {
    // Map depth to hue (0.0 to 1.0)
    // Reverse the hue so that deeper bones are redder (more attention-grabbing)
    let hue = if max_depth > 1 {
        1.0 - (depth as f32 / (max_depth - 1) as f32)
    } else {
        0.0
    };
    
    // Convert HSV to RGB with full saturation and value
    let (r, g, b) = hsv_to_rgb(hue, 0.8, 0.9);
    [r, g, b]
}

fn setup(
    mut commands: Commands,
    mesh_file: Res<MeshFile>,
    export_path: Option<Res<ExportPath>>,
) {
    // Read and parse TOML file
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

    // Initialize transform and position maps
    let mut transforms = HashMap::new();
    let mut positions = HashMap::new();

    // First process lower_spine as it's the root
    if let Some(lower_spine_group) = model.0.get("body") {
        if let Some(lower_spine) = lower_spine_group.subgroups.get("lower_spine") {
            process_bone_group(
                lower_spine,
                Mat4::IDENTITY,
                Vec3::ZERO,
                "",
                &mut transforms,
                &mut positions,
                "body.lower_spine",
            );
        }
    }

    // Then process pelvis bones
    if let Some(body_group) = model.0.get("body") {
        // Process left and right pelvis, which connect to lower_spine
        for pelvis_name in ["left_pelvis", "right_pelvis"] {
            if let Some(pelvis_group) = body_group.subgroups.get(pelvis_name) {
                process_bone_group(
                    pelvis_group,
                    Mat4::IDENTITY,
                    Vec3::ZERO,
                    "body.lower_spine",
                    &mut transforms,
                    &mut positions,
                    &format!("body.{}", pelvis_name),
                );
            }
        }

        // Process leg bones that connect to pelvis
        if let Some(right_pelvis) = body_group.subgroups.get("right_pelvis") {
            for (subgroup_name, subgroup) in &right_pelvis.subgroups {
                process_bone_group(
                    subgroup,
                    Mat4::IDENTITY,
                    Vec3::ZERO,
                    "body.right_pelvis",
                    &mut transforms,
                    &mut positions,
                    &format!("body.right_pelvis.{}", subgroup_name),
                );
            }
        }

        // Process foot bones that connect to legs
        if let Some(right_pelvis) = body_group.subgroups.get("right_pelvis") {
            if let Some(right_leg) = right_pelvis.subgroups.get("right_leg") {
                for (subgroup_name, subgroup) in &right_leg.subgroups {
                    process_bone_group(
                        subgroup,
                        Mat4::IDENTITY,
                        Vec3::ZERO,
                        "body.right_pelvis.right_leg",
                        &mut transforms,
                        &mut positions,
                        &format!("body.right_pelvis.right_leg.{}", subgroup_name),
                    );
                }
            }
        }
    }

    // Find maximum bone depth
    let max_depth = positions.keys()
        .map(|path| get_bone_depth(path))
        .max()
        .unwrap_or(0);

    // Create mesh data from positions with rainbow colors
    let mut mesh_data = MeshData::default();
    let mut color_map = BTreeMap::new();

    for (bone_path, (start, end)) in positions.iter() {
        let depth = get_bone_depth(bone_path);
        let color = get_rainbow_color(depth, max_depth);
        mesh_data.positions.insert(bone_path.clone(), BonePosition { start: *start, end: *end, color });
        color_map.insert(bone_path.clone(), color);
    }

    // Set up camera
    commands.spawn((
        Camera3dBundle {
            transform: Transform::from_xyz(5.0, 5.0, 5.0).looking_at(Vec3::ZERO, Vec3::Y),
            ..default()
        },
        CameraController::default(),
    ));

    // Insert resources
    commands.insert_resource(mesh_data);
    commands.insert_resource(ShowAxes(true));
    commands.insert_resource(VisualizationSettings {
        visualization_mode: BoneVisualization::Solid,
        line_width: 5.0,
    });
    commands.insert_resource(LastUpdateTime(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs_f64()));
    commands.insert_resource(ColorCache { colors: color_map });

    if let Some(export_path) = export_path {
        commands.insert_resource(ExportState {
            path: export_path.0.clone(),
            exported: false,
        });
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
) {
    if mouse_button_input.just_pressed(MouseButton::Middle) {
        let mut transform = query.single_mut();
        transform.translation = Vec3::new(0.0, 0.0, 5.0);
        transform.look_at(Vec3::ZERO, Vec3::Y);
    }
}

fn draw_bones(
    mut gizmos: Gizmos,
    mesh_data: Res<MeshData>,
    _settings: Res<VisualizationSettings>,
) {
    for bone_position in mesh_data.positions.values() {
        gizmos.line(
            bone_position.start,
            bone_position.end,
            Color::rgb(
                bone_position.color[0],
                bone_position.color[1],
                bone_position.color[2],
            ),
        );
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
) {
    egui::Window::new("Settings").show(contexts.ctx_mut(), |ui| {
        ui.horizontal(|ui| {
            ui.label("Visualization Mode:");
            ui.radio_value(&mut settings.visualization_mode, BoneVisualization::Solid, "Solid");
            ui.radio_value(&mut settings.visualization_mode, BoneVisualization::ByDepth, "By Depth");
            ui.radio_value(&mut settings.visualization_mode, BoneVisualization::ByChain, "By Chain");
        });
        
        ui.add(egui::Slider::new(&mut settings.line_width, 0.1..=5.0)
            .text("Bone Thickness"));
    });
}

fn check_file_changes(
    mesh_file: Res<MeshFile>,
    mut last_update: ResMut<LastUpdateTime>,
    mut commands: Commands,
) {
    if let Ok(metadata) = fs::metadata(&mesh_file.0) {
        if let Ok(modified) = metadata.modified() {
            let modified_time = modified
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs_f64();

            if modified_time > last_update.0 {
                if let Ok(content) = fs::read_to_string(&mesh_file.0) {
                    if let Ok(model) = toml::from_str::<Model>(&content) {
                        let mut mesh_data = MeshData::default();
                        let mut transforms = HashMap::new();
                        let mut positions = HashMap::new();

                        // Process each top-level group
                        for (group_name, group) in &model.0 {
                            process_bone_group(
                                group,
                                Mat4::IDENTITY,
                                Vec3::ZERO,
                                "",
                                &mut transforms,
                                &mut positions,
                                group_name,
                            );
                        }

                        // Create BonePosition structs
                        for (name, (start, end)) in positions {
                            mesh_data.positions.insert(name, BonePosition {
                                start,
                                end,
                                color: [1.0, 1.0, 1.0],
                            });
                        }

                        commands.insert_resource(mesh_data);
                        last_update.0 = modified_time;
                    }
                }
            }
        }
    }
}

fn export_to_glb(mesh_data: &MeshData, export_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Create a JSON structure for the GLB
    let mut root = json::Root::default();
    root.asset.version = "2.0".to_string();
    root.asset.generator = Some("Hooded Crow Modeller".to_string());
    
    // Create buffer and get vertex and index data
    let vertices = mesh_data.positions.values()
        .flat_map(|pos| vec![pos.start, pos.end])
        .flat_map(|v| vec![v.x, v.y, v.z])
        .collect::<Vec<f32>>();
    
    let indices: Vec<u32> = (0..vertices.len() as u32 / 3).collect();
    
    // Calculate buffer sizes
    let vertex_buffer_length = vertices.len() * std::mem::size_of::<f32>();
    let index_buffer_length = indices.len() * std::mem::size_of::<u32>();
    let buffer_length = vertex_buffer_length + index_buffer_length;
    
    // Create buffer
    let buffer = json::Buffer {
        byte_length: USize64(buffer_length as u64),
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        uri: None,
    };
    root.buffers.push(buffer);

    // Create buffer views
    let vertex_view = json::buffer::View {
        buffer: json::Index::new(0),
        byte_length: USize64(vertex_buffer_length as u64),
        byte_offset: Some(USize64(0)),
        byte_stride: Some(Stride(12)),
        extensions: Default::default(),
        extras: Default::default(),
        name: None,
        target: Some(Checked::Valid(json::buffer::Target::ArrayBuffer)),
    };
    let vertex_view_index = root.buffer_views.len();
    root.buffer_views.push(vertex_view);

    let index_view = json::buffer::View {
        buffer: json::Index::new(0),
        byte_length: USize64(index_buffer_length as u64),
        byte_offset: Some(USize64(vertex_buffer_length as u64)),
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
        buffer_view: Some(json::Index::new(vertex_view_index as u32)),
        byte_offset: Some(USize64(0)),
        count: USize64(vertices.len() as u64 / 3),
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
        buffer_view: Some(json::Index::new(index_view_index as u32)),
        byte_offset: Some(USize64(0)),
        count: USize64(indices.len() as u64),
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
                json::Index::new(vertex_accessor_index as u32),
            );
            map
        },
        extensions: Default::default(),
        extras: Default::default(),
        indices: Some(json::Index::new(index_accessor_index as u32)),
        material: None,
        mode: Checked::Valid(json::mesh::Mode::Lines),
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

    // Create binary buffer
    let mut buffer_data = Vec::new();
    
    // Add vertex data
    buffer_data.extend_from_slice(bytemuck::cast_slice(&vertices));
    
    // Add index data
    buffer_data.extend_from_slice(bytemuck::cast_slice(&indices));

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
        .add_systems(Startup, setup)
        .add_systems(Update, (
            keyboard_input,
            mouse_motion,
            mouse_wheel,
            mouse_button_input,
            update_camera,
            draw_bones,
            draw_axes,
            export_system,
            check_file_changes,
            update_ui,
        ))
        .run();
}