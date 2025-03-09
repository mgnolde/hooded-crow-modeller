use bevy::math::Vec3;
use bevy::prelude::*;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize, Resource)]
pub struct Model(pub HashMap<String, Group>);

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Group {
    #[serde(default)]
    pub bones: HashMap<String, Bone>,
    #[serde(flatten)]
    pub subgroups: HashMap<String, Group>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Bone {
    pub length: f32,
    #[serde(default)]
    pub orientation: Option<f32>,
    #[serde(default)]
    pub slope: Option<f32>,
    #[serde(default)]
    pub rotation: Option<f32>,
    #[serde(default)]
    pub color: Option<[f32; 4]>,
    #[serde(default)]
    pub skin_verts: Vec<SkinVert>,
    
    // Resolved values after inheritance
    #[serde(skip)]
    pub resolved_orientation: f32,
    #[serde(skip)]
    pub resolved_slope: f32,
    #[serde(skip)]
    pub resolved_rotation: f32,
}

// Triangle definition
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub struct TriangleRef {
    pub name: String,
    pub position: usize,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SkinVert {
    #[serde(default)]
    pub segment_length: f32,  // Position along the bone
    #[serde(default)]
    pub distance: f32,        // Distance from the bone
    #[serde(default)]
    pub rotation: f32,  // Rotation around the bone in degrees
    #[serde(default = "default_weight")]
    pub weight: f32,          // Weight of influence
    #[serde(default)]
    pub id: Option<String>,   // Optional identifier for the skin vertex
    #[serde(default)]
    pub triangles: HashMap<String, usize>, // Triangle references: face_name -> vertex position
    #[serde(default)]
    pub color: Option<[f32; 4]>, // Color for this vertex
}

impl Default for SkinVert {
    fn default() -> Self {
        Self {
            segment_length: 0.5,  // Default to middle of the bone
            distance: 0.0,        // Default to on the bone
            rotation: 0.0,        // Default to 0 degrees
            weight: 1.0,          // Default to full weight
            id: None,             // Default to no id
            triangles: HashMap::new(), // Default to no triangle references
            color: None,          // Default to no color
        }
    }
}

impl SkinVert {
    /// Calculate the position of this skin vertex relative to the bone
    pub fn calculate_position(&self, bone_start: Vec3, bone_end: Vec3, bone_rotation: f32) -> Vec3 {
        // Calculate the direction of the bone
        let bone_direction = (bone_end - bone_start).normalize();
        let bone_length = (bone_end - bone_start).length();
        
        // Find position along the bone based on segment_length
        let segment_position = bone_start + bone_direction * bone_length * self.segment_length;
        
        // If distance is very small, return the position on the bone
        if self.distance < 0.001 {
            return segment_position;
        }
        
        // Create coordinate system for the bone
        // First perpendicular vector - try to use global Y axis
        let perp1 = if bone_direction.dot(Vec3::Y).abs() < 0.99 {
            bone_direction.cross(Vec3::Y).normalize()
        } else {
            bone_direction.cross(Vec3::X).normalize()
        };
        
        // Second perpendicular vector to complete the orthogonal basis
        let perp2 = bone_direction.cross(perp1).normalize();
        
        // Convert rotation to radians and combine with bone rotation
        let combined_angle_rad = (self.rotation + bone_rotation).to_radians();
        
        // Calculate the offset from the bone in the perpendicular plane
        let offset = perp1 * self.distance * combined_angle_rad.cos() +
                     perp2 * self.distance * combined_angle_rad.sin();
        
        // Final position is the base position plus the offset
        segment_position + offset
    }
}

impl Bone {
    pub fn has_parent(&self) -> bool {
        false
    }
    
    pub fn resolve_values(&mut self, parent: Option<&Bone>) {
        // For orientation, add to parent's value if present (relative orientation)
        self.resolved_orientation = match (self.orientation, parent) {
            (Some(val), Some(parent)) => {
                // Convert to radians, add, then convert back to degrees
                (parent.resolved_orientation.to_radians() + val.to_radians()).to_degrees()
            },
            (Some(val), None) => val,                                      // Root bone uses absolute value
            (None, Some(parent)) => parent.resolved_orientation,           // Inherit from parent
            (None, None) => 0.0,                                          // Default value
        };
        
        // Slope and rotation are absolute values
        self.resolved_slope = match (self.slope, parent) {
            (Some(val), _) => val,
            (None, Some(parent)) => parent.resolved_slope,
            (None, None) => 0.0,
        };
        
        self.resolved_rotation = match (self.rotation, parent) {
            (Some(val), _) => val,
            (None, Some(parent)) => parent.resolved_rotation,
            (None, None) => 0.0,
        };
    }
}

fn default_weight() -> f32 {
    1.0
}

#[derive(Debug, Clone)]
pub struct BonePosition {
    pub start: Vec3,
    pub end: Vec3,
    pub color: [f32; 3],
}

impl Default for BonePosition {
    fn default() -> Self {
        Self {
            start: Vec3::ZERO,
            end: Vec3::ZERO,
            color: [1.0, 1.0, 1.0],  // Default white color
        }
    }
}

fn calculate_direction(orientation: f32, slope: f32) -> Vec3 {
    // For vertical slopes (±90°), use the raw slope value
    if (slope - 90.0).abs() < 0.001 {
        // +90° points up
        return Vec3::new(0.0, 1.0, 0.0);
    } else if (slope + 90.0).abs() < 0.001 {
        // -90° points down
        return Vec3::new(0.0, -1.0, 0.0);
    } else {
        // For non-vertical slopes, use spherical coordinates
        let orientation_rad = orientation.to_radians();
        let slope_rad = slope.to_radians();
        Vec3::new(
            slope_rad.cos() * orientation_rad.sin(),
            slope_rad.sin(),
            slope_rad.cos() * orientation_rad.cos()
        ).normalize()
    }
}

impl Model {
    pub fn from_toml(content: &str) -> Result<Self, Box<dyn Error>> {
        let config: toml::Value = content.parse()?;
        
        let mut bones = HashMap::new();

        fn find_parent_bone<'a>(path: &str, bones: &'a HashMap<String, Group>) -> Option<&'a Bone> {
            // Iterate through all groups to find the bone with this path
            for (_group_path, group) in bones {
                if let Some(bone) = group.bones.get(path) {
                    return Some(bone);
                }
            }
            None
        }

        fn parse_skin_verts(table: &toml::value::Table) -> Vec<SkinVert> {
            // Check if skin_verts is present
            if let Some(skin_verts_value) = table.get("skin_verts") {
                // Array format: skin_verts = [{ segment_length = 0.2, ... }]
                if let Some(skin_verts_array) = skin_verts_value.as_array() {
                    let result: Vec<SkinVert> = skin_verts_array.iter().map(|v| {
                        let mut skin_vert = SkinVert::default();
                        if let Some(segment_length) = v.get("segment_length").and_then(|v| v.as_float()) {
                            skin_vert.segment_length = segment_length as f32;
                        }
                        if let Some(distance) = v.get("distance").and_then(|v| v.as_float()) {
                            skin_vert.distance = distance as f32;
                        }
                        if let Some(rotation) = v.get("rotation").and_then(|v| v.as_float()) {
                            skin_vert.rotation = rotation as f32;
                        }
                        if let Some(weight) = v.get("weight").and_then(|v| v.as_float()) {
                            skin_vert.weight = weight as f32;
                        }
                        if let Some(id) = v.get("id").and_then(|v| v.as_str()) {
                            skin_vert.id = Some(id.to_string());
                        }
                        
                        // Parse triangle references
                        if let Some(triangles) = v.get("triangles").and_then(|v| v.as_table()) {
                            for (face_name, position) in triangles {
                                if let Some(pos) = position.as_integer() {
                                    skin_vert.triangles.insert(face_name.clone(), pos as usize);
                                }
                            }
                        }
                        
                        // Parse color
                        if let Some(color) = v.get("color").and_then(|v| v.as_array()) {
                            skin_vert.color = Some([color[0].as_float().unwrap_or(1.0) as f32,
                                                    color[1].as_float().unwrap_or(1.0) as f32,
                                                    color[2].as_float().unwrap_or(1.0) as f32,
                                                    color.get(3).and_then(|c| c.as_float()).unwrap_or(1.0) as f32]);
                        }
                        
                        skin_vert
                    }).collect();
                    return result;
                }
                
                // Table format: skin_verts = { "id1" = { segment_length = 0.2, ... } }
                if let Some(skin_verts_table) = skin_verts_value.as_table() {
                    let result: Vec<SkinVert> = skin_verts_table.iter().map(|(id, props)| {
                        let mut skin_vert = SkinVert::default();
                        skin_vert.id = Some(id.to_string());
                        
                        if let Some(segment_length) = props.get("segment_length").and_then(|v| v.as_float()) {
                            skin_vert.segment_length = segment_length as f32;
                        }
                        if let Some(distance) = props.get("distance").and_then(|v| v.as_float()) {
                            skin_vert.distance = distance as f32;
                        }
                        if let Some(rotation) = props.get("rotation").and_then(|v| v.as_float()) {
                            skin_vert.rotation = rotation as f32;
                        }
                        if let Some(weight) = props.get("weight").and_then(|v| v.as_float()) {
                            skin_vert.weight = weight as f32;
                        }
                        
                        // Parse triangle references
                        if let Some(triangles) = props.get("triangles").and_then(|v| v.as_table()) {
                            for (face_name, position) in triangles {
                                if let Some(pos) = position.as_integer() {
                                    skin_vert.triangles.insert(face_name.clone(), pos as usize);
                                }
                            }
                        }
                        
                        // Parse color
                        if let Some(color) = props.get("color").and_then(|v| v.as_array()) {
                            skin_vert.color = Some([color[0].as_float().unwrap_or(1.0) as f32,
                                                    color[1].as_float().unwrap_or(1.0) as f32,
                                                    color[2].as_float().unwrap_or(1.0) as f32,
                                                    color.get(3).and_then(|c| c.as_float()).unwrap_or(1.0) as f32]);
                        }
                        
                        skin_vert
                    }).collect();
                    return result;
                }
            }
            
            Vec::new()
        }

        fn collect_bones(
            table: &toml::value::Table, 
            current_path: &str, 
            bones: &mut HashMap<String, Group>
        ) -> Group {
            // Create current group
            let mut current_group = Group {
                bones: HashMap::new(),
                subgroups: HashMap::new(),
            };
            
            // If this table has bone properties, add the bone to the current group
            if table.contains_key("length") {
                // Find parent bone to potentially inherit properties
                let parent_bone = if current_path.contains('.') {
                    let parent_path = current_path.rsplit_once('.').unwrap().0;
                    find_parent_bone(parent_path, bones)
                } else {
                    None
                };
                
                // Extract bone properties with inheritance
                let mut bone = Bone {
                    length: table.get("length").and_then(|v| v.as_float()).unwrap_or(1.0) as f32,
                    orientation: table.get("orientation")
                        .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                        .map(|v| v as f32),
                    slope: table.get("slope")
                        .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                        .map(|v| v as f32),
                    rotation: table.get("rotation")
                        .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                        .map(|v| v as f32),
                    color: table.get("color").and_then(|v| v.as_array())
                        .map(|color| [color[0].as_float().unwrap_or(1.0) as f32,
                                      color[1].as_float().unwrap_or(1.0) as f32,
                                      color[2].as_float().unwrap_or(1.0) as f32,
                                      color.get(3).and_then(|c| c.as_float()).unwrap_or(1.0) as f32]),
                    skin_verts: parse_skin_verts(table),
                    resolved_orientation: 0.0,
                    resolved_slope: 0.0,
                    resolved_rotation: 0.0,
                };
                
                // Resolve inherited values
                bone.resolve_values(parent_bone);
                
                // Extract color values from skin vertices
                if let Some(skin_verts_value) = table.get("skin_verts") {
                    if let Some(skin_verts_array) = skin_verts_value.as_array() {
                        for skin_vert in skin_verts_array {
                            if let Some(color) = skin_vert.get("color").and_then(|v| v.as_array()) {
                                let extracted_color = [color[0].as_float().unwrap_or(1.0) as f32,
                                                       color[1].as_float().unwrap_or(1.0) as f32,
                                                       color[2].as_float().unwrap_or(1.0) as f32,
                                                       color.get(3).and_then(|c| c.as_float()).unwrap_or(1.0) as f32];
                                println!("Extracted color for bone {}: {:?}", current_path, extracted_color);
                                bone.color = Some(extracted_color);
                            }
                        }
                    }
                }
                
                current_group.bones.insert(current_path.to_string(), bone);
            }
            
            // Process nested tables as subgroups
            for (key, value) in table {
                if let Some(nested_table) = value.as_table() {
                    // Skip keys that are bone properties
                    if key == "length" || key == "orientation" || key == "slope" || key == "rotation" || key == "skin_verts" || key == "color" {
                        continue;
                    }
                    
                    let new_path = if current_path.is_empty() {
                        key.clone()
                    } else {
                        format!("{}.{}", current_path, key)
                    };
                    
                    // Recursively collect bones from subgroup
                    let subgroup = collect_bones(nested_table, &new_path, bones);
                    
                    // Add subgroup if it has bones or subgroups
                    if !subgroup.bones.is_empty() || !subgroup.subgroups.is_empty() {
                        current_group.subgroups.insert(key.clone(), subgroup);
                    }
                }
            }
            
            // Store the group in the bones map
            if !current_group.bones.is_empty() || !current_group.subgroups.is_empty() {
                bones.insert(current_path.to_string(), current_group.clone());
            }
            
            current_group
        }

        if let Some(body) = config.get("body").and_then(|v| v.as_table()) {
            let root_group = collect_bones(body, "body", &mut bones);
            bones.insert("body".to_string(), root_group);
            
            Ok(Model(bones))
        } else {
            Err("No [body] section found in TOML".into())
        }
    }

    pub fn get_bones(&self) -> HashMap<String, Bone> {
        let mut bones = HashMap::new();
        
        fn collect_bones_from_group(group: &Group, bones: &mut HashMap<String, Bone>) {
            // Add bones from current group
            for (name, bone) in &group.bones {
                bones.insert(name.clone(), bone.clone());
            }
            
            // Recursively collect bones from subgroups
            for (_, subgroup) in &group.subgroups {
                collect_bones_from_group(subgroup, bones);
            }
        }
        
        // Start collection from root groups
        for (_, group) in &self.0 {
            collect_bones_from_group(group, &mut bones);
        }
        
        bones
    }

    pub fn get_bone_chain(&self) -> (Vec<[f32; 3]>, Vec<u32>, Vec<[f32; 3]>) {
        let positions = self.calculate_bone_positions();
        
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        let mut colors = Vec::new();
        
        for (_, start, end, color) in positions.iter() {
            vertices.push([start.x, start.y, start.z]);
            vertices.push([end.x, end.y, end.z]);
            
            // Add colors for both vertices of the bone
            colors.push(*color);
            colors.push(*color);
            
            let idx = vertices.len() - 2;
            indices.push(idx as u32);
            indices.push((idx + 1) as u32);
        }
        
        (vertices, indices, colors)
    }

    fn calculate_bone_depths(&self) -> HashMap<String, u32> {
        let mut depths = HashMap::new();
        let bones = self.get_bones();

        // Helper function to calculate depth recursively
        fn calculate_depth(
            bone_name: &str,
            bones: &HashMap<String, Bone>,
            depths: &mut HashMap<String, u32>,
            visited: &mut HashSet<String>
        ) -> u32 {
            // If we've already calculated this bone's depth, return it
            if let Some(&depth) = depths.get(bone_name) {
                return depth;
            }

            // Prevent cycles
            if !visited.insert(bone_name.to_string()) {
                return 0;
            }

            // Get the bone's parent
            let parent = "";

            // Calculate depth
            let depth = if parent.is_empty() {
                // This is a root bone
                0
            } else {
                // Recursively calculate parent's depth + 1
                calculate_depth(parent, bones, depths, visited) + 1
            };

            depths.insert(bone_name.to_string(), depth);
            depth
        }

        // Calculate depth for each bone
        let mut visited = HashSet::new();
        for bone_name in bones.keys() {
            calculate_depth(bone_name, &bones, &mut depths, &mut visited);
        }

        depths
    }

    pub fn calculate_bone_positions(&self) -> Vec<(String, Vec3, Vec3, [f32; 3])> {
        let mut positions = Vec::new();
        let bones = self.get_bones();
        
        for (name, bone) in bones {
            let level = name.matches('.').count();
            let color = match level {
                0 => [1.0, 0.0, 0.0], // Red
                1 => [0.0, 0.0, 1.0], // Blue
                2 => [1.0, 0.0, 1.0], // Magenta
                _ => [1.0, 1.0, 1.0], // White
            };
            
            let start = Vec3::ZERO;
            
            // Get base direction from orientation and slope
            let direction = calculate_direction(bone.resolved_orientation, bone.resolved_slope);
            
            let end = start + (direction * bone.length);
            
            positions.push((name.clone(), start, end, color));
        }
        
        positions
    }

    pub fn collect_triangles(&self) -> Vec<Triangle> {
        let bone_positions = self.calculate_bone_positions();
        let bone_position_map: HashMap<_, _> = bone_positions
            .into_iter()
            .map(|(name, start, end, _)| (name, (start, end)))
            .collect();
        
        // First, collect all triangle references from all skin vertices
        let mut triangle_refs: HashMap<String, Vec<(usize, String, Vec3)>> = HashMap::new();
        
        for (bone_name, bone) in self.get_bones() {
            if let Some((bone_start, bone_end)) = bone_position_map.get(&bone_name) {
                for skin_vert in &bone.skin_verts {
                    // Calculate the actual position of this skin vertex
                    let position = skin_vert.calculate_position(*bone_start, *bone_end, bone.resolved_rotation);
                    
                    // Print debug info for this vertex
                    println!("Triangle Vertex: bone={}, id={:?}, pos={:?}, triangles={:?}", 
                             bone_name, skin_vert.id, position, skin_vert.triangles);
                    
                    // Add each triangle reference
                    for (face_name, position_in_triangle) in &skin_vert.triangles {
                        triangle_refs
                            .entry(face_name.clone())
                            .or_insert_with(Vec::new)
                            .push((*position_in_triangle, bone_name.clone(), position));
                    }
                }
            }
        }
        
        // Now, build the triangles from the collected references
        let mut triangles = Vec::new();
        
        for (face_name, mut vertices) in triangle_refs {
            // Sort vertices by their position in the triangle
            vertices.sort_by_key(|(pos, _, _)| *pos);
            
            // Print debug info about this triangle
            println!("Triangle '{}' has {} vertices", face_name, vertices.len());
            for (i, (pos_in_tri, bone, pos)) in vertices.iter().enumerate() {
                println!("  Vertex {}: pos_in_tri={}, bone={}, pos={:?}", i, pos_in_tri, bone, pos);
            }
            
            // Check if we have exactly three vertices for a triangle
            if vertices.len() == 3 {
                let positions = [vertices[0].2, vertices[1].2, vertices[2].2];
                let bone_names = [
                    vertices[0].1.clone(), 
                    vertices[1].1.clone(), 
                    vertices[2].1.clone()
                ];
                
                triangles.push(Triangle {
                    name: face_name,
                    vertices: positions,
                    bone_names,
                });
            } else {
                eprintln!("WARNING: Triangle '{}' has {} vertices, expected 3", 
                         face_name, vertices.len());
            }
        }
        
        triangles
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Triangle {
    pub name: String,
    pub vertices: [Vec3; 3],
    pub bone_names: [String; 3],
}

// Helper function to convert HSV to RGB
fn hsv_to_rgb(h: f32, s: f32, v: f32) -> (f32, f32, f32) {
    let h = h % 360.0;
    let c = v * s;
    let x = c * (1.0 - (h / 60.0 % 2.0 - 1.0).abs());
    let m = v - c;

    let (r, g, b) = match h as i32 {
        h if h < 60 => (c, x, 0.0),
        h if h < 120 => (x, c, 0.0),
        h if h < 180 => (0.0, c, x),
        h if h < 240 => (0.0, x, c),
        h if h < 300 => (x, 0.0, c),
        _ => (c, 0.0, x),
    };

    (r + m, g + m, b + m)
}