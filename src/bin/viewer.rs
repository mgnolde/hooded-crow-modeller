use gltf_json as json;
use gltf_json::validation::Checked;
use std::fs;
use bevy::{
    asset::Assets,
    ecs::system::Commands,
    pbr::{PbrBundle, StandardMaterial},
    render::{
        color::Color,
        mesh::Mesh,
    },
    transform::components::Transform,
    utils::default,
    ecs::component::Component,
    prelude::*,
};

#[derive(serde::Deserialize)]
struct PointsFile(String);

#[derive(serde::Deserialize)]
struct MeshData {
    vertices: Vec<[f32; 3]>,
    indices: Vec<u32>,
}

#[derive(Component)]
struct FileWatcher {
    last_modified: std::time::SystemTime,
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_systems(Startup, spawn_mesh)
        .add_systems(Update, (bevy::window::close_on_esc, check_file_changes))
        .run();
}

fn check_file_changes(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    query: Query<(Entity, &FileWatcher)>,
) {
    let path = std::path::Path::new("mesh.json");
    if let Ok(metadata) = std::fs::metadata(path) {
        if let Ok(last_modified) = metadata.modified() {
            // If we have an existing mesh, check if we need to update it
            if let Ok((entity, watcher)) = query.get_single() {
                if last_modified > watcher.last_modified {
                    // File has changed, despawn old mesh and spawn new one
                    commands.entity(entity).despawn();
                    spawn_mesh(commands, meshes, materials);
                }
            } else {
                // No mesh exists yet, spawn first one
                spawn_mesh(commands, meshes, materials);
            }
        }
    }
}

fn setup(
    mut commands: Commands,
) {
    // Light
    commands.spawn(PointLightBundle {
        point_light: PointLight {
            intensity: 1500.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(4.0, 8.0, 4.0),
        ..default()
    });

    // Camera
    commands.spawn(Camera3dBundle {
        transform: Transform::from_xyz(-2.0, 2.5, 5.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    });
}

fn spawn_mesh(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let path = std::path::Path::new("mesh.json");
    let last_modified = std::fs::metadata(path)
        .expect("Failed to read metadata")
        .modified()
        .expect("Failed to get modification time");

    // Read mesh data from file
    let mesh_json = fs::read_to_string(path)
        .expect("Failed to read mesh file");
    let mesh_data: MeshData = serde_json::from_str(&mesh_json)
        .expect("Failed to parse JSON");

    // Create the mesh
    let mut mesh = Mesh::new(bevy::render::render_resource::PrimitiveTopology::TriangleList);
    
    // Insert mesh attributes
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, mesh_data.vertices.clone());
    
    // Calculate and insert normals
    let mut normals = Vec::with_capacity(mesh_data.vertices.len());
    for _ in 0..mesh_data.vertices.len() {
        normals.push([0.0, 1.0, 0.0]);
    }
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    
    // Insert UV coordinates
    let mut uvs = Vec::with_capacity(mesh_data.vertices.len());
    for _ in 0..mesh_data.vertices.len() {
        uvs.push([0.0, 0.0]);
    }
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    
    // Set indices
    mesh.set_indices(Some(bevy::render::mesh::Indices::U32(mesh_data.indices.clone())));

    // Spawn the mesh entity with the FileWatcher component
    commands.spawn((
        PbrBundle {
            mesh: meshes.add(mesh),
            material: materials.add(StandardMaterial {
                base_color: Color::rgb(0.8, 0.2, 0.2),
                metallic: 0.0,
                perceptual_roughness: 0.8,
                ..default()
            }),
            transform: Transform::from_xyz(0.0, 0.0, 0.0),
            ..default()
        },
        FileWatcher {
            last_modified,
        },
    ));
}
