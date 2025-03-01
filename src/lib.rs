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
    pub skin_verts: Vec<SkinVert>,
    
    // Resolved values after inheritance
    #[serde(skip)]
    pub resolved_orientation: f32,
    #[serde(skip)]
    pub resolved_slope: f32,
    #[serde(skip)]
    pub resolved_rotation: f32,
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
}

impl Default for SkinVert {
    fn default() -> Self {
        Self {
            segment_length: 0.5,  // Default to middle of the bone
            distance: 0.0,        // Default to on the bone
            rotation: 0.0,        // Default to 0 degrees
            weight: 1.0,          // Default to full weight
            id: None,             // Default to no id
        }
    }
}

impl SkinVert {
    /// Calculate the position of this skin vertex relative to the bone
    pub fn calculate_position(&self, bone_start: Vec3, bone_end: Vec3) -> Vec3 {
        // Calculate the direction of the bone
        let bone_direction = (bone_end - bone_start).normalize();
        let bone_length = (bone_end - bone_start).length();
        
        // Find position along the bone based on segment_length
        let segment_position = bone_start + bone_direction * bone_length * self.segment_length;
        
        // If distance is very small, return the position on the bone
        if self.distance < 0.001 {
            return segment_position;
        }
        
        // Calculate perpendicular vectors to create a coordinate system around the bone
        // First perpendicular vector - prefer using the global Y-axis for stability if possible
        let perp1 = if bone_direction.dot(Vec3::Y).abs() < 0.99 {
            // If bone is not nearly parallel to Y, use Y to find perpendicular
            bone_direction.cross(Vec3::Y).normalize()
        } else {
            // If bone is nearly parallel to Y, use X to find perpendicular
            bone_direction.cross(Vec3::X).normalize()
        };
        
        // Second perpendicular vector to complete the orthogonal basis
        let perp2 = bone_direction.cross(perp1).normalize();
        
        // Calculate rotation around the bone (in radians)
        let rotation_rad = self.rotation.to_radians();
        
        // Apply distance and rotation to get the final position
        let offset = perp1 * self.distance * rotation_rad.cos() + 
                     perp2 * self.distance * rotation_rad.sin();
        
        let final_pos = segment_position + offset;
        
        eprintln!("Skin vert calculation: segment={}, distance={}, rotation={}°", 
                  self.segment_length, self.distance, self.rotation);
        eprintln!("  bone_start={:?}, bone_end={:?}, bone_length={}", 
                 bone_start, bone_end, bone_length);
        eprintln!("  segment_position={:?}", segment_position);
        eprintln!("  perp1={:?}, perp2={:?}", perp1, perp2);
        eprintln!("  rotation_rad={}, offset={:?}", rotation_rad, offset);
        eprintln!("  final_position={:?}", final_pos);
        
        final_pos
    }
}

impl Bone {
    pub fn has_parent(&self) -> bool {
        false
    }
    
    pub fn resolve_values(&mut self, parent: Option<&Bone>) {
        // If no parent or value is explicitly specified, use default
        self.resolved_orientation = match (self.orientation, parent) {
            (Some(val), _) => val,                                 // Use explicitly set value
            (None, Some(parent)) => parent.resolved_orientation,   // Inherit from parent
            (None, None) => 0.0,                                  // Default value
        };
        
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
    eprintln!("\nCalculating direction for slope={}, orientation={}", slope, orientation);
    
    // For vertical slopes (±90°), use the raw slope value
    if (slope - 90.0).abs() < 0.001 || (slope + 90.0).abs() < 0.001 {
        // Convert slope to unit direction: +90° -> +1, -90° -> -1
        let y = if slope > 0.0 { 1.0 } else { -1.0 };
        Vec3::new(0.0, y, 0.0)
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
        eprintln!("!!! MODEL CREATION STARTED !!!");
        eprintln!("Content:\n{}", content);
        
        let config: toml::Value = content.parse()?;
        eprintln!("Parsed TOML:\n{:#?}", config);
        
        eprintln!("!!! CHECKING FOR SKIN VERTS IN TOML !!!");
        if let Some(body) = config.get("body").and_then(|v| v.as_table()) {
            if let Some(lower_spine) = body.get("lower_spine").and_then(|v| v.as_table()) {
                eprintln!("Found lower_spine: {:?}", lower_spine.keys().collect::<Vec<_>>());
                if let Some(skin_verts) = lower_spine.get("skin_verts") {
                    eprintln!("Found lower_spine.skin_verts: {:?}", skin_verts);
                } else {
                    eprintln!("No skin_verts in lower_spine");
                }
            }
        }
        
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
            eprintln!("SKINVERT: Parsing skin_verts for bone table: {:?}", table.get("name"));
            
            // Check if skin_verts is present
            if let Some(skin_verts_value) = table.get("skin_verts") {
                // Array format: skin_verts = [{ segment_length = 0.2, ... }]
                if let Some(skin_verts_array) = skin_verts_value.as_array() {
                    eprintln!("SKINVERT: Found {} skin vertices in TOML array format", skin_verts_array.len());
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
                        eprintln!("SKINVERT: Created skin vertex: segment={}, distance={}, rotation={}, weight={}, id={:?}",
                                skin_vert.segment_length, skin_vert.distance, skin_vert.rotation, skin_vert.weight, skin_vert.id);
                        skin_vert
                    }).collect();
                    eprintln!("SKINVERT: Returning {} processed skin vertices from array format", result.len());
                    return result;
                }
                
                // Table format: skin_verts = { "id1" = { segment_length = 0.2, ... } }
                if let Some(skin_verts_table) = skin_verts_value.as_table() {
                    eprintln!("SKINVERT: Found {} skin vertices in TOML table format", skin_verts_table.len());
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
                        
                        eprintln!("SKINVERT: Created skin vertex: segment={}, distance={}, rotation={}, weight={}, id={:?}",
                               skin_vert.segment_length, skin_vert.distance, skin_vert.rotation, skin_vert.weight, skin_vert.id);
                        skin_vert
                    }).collect();
                    eprintln!("SKINVERT: Returning {} processed skin vertices from table format", result.len());
                    return result;
                }
            }
            
            eprintln!("SKINVERT: No valid skin_verts found in table");
            Vec::new()
        }

        fn collect_bones(
            table: &toml::value::Table, 
            current_path: &str, 
            bones: &mut HashMap<String, Group>
        ) -> Group {
            eprintln!("\nProcessing table at path: {}", current_path);
            eprintln!("Table keys: {:?}", table.keys().collect::<Vec<_>>());
            
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
                    skin_verts: parse_skin_verts(table),
                    resolved_orientation: 0.0,
                    resolved_slope: 0.0,
                    resolved_rotation: 0.0,
                };
                
                // Resolve inherited values
                bone.resolve_values(parent_bone);
                
                eprintln!("Created bone: {:?}", bone);
                eprintln!("Bone properties: length={}, raw orientation={:?}, raw slope={:?}, raw rotation={:?}", 
                         bone.length, bone.orientation, bone.slope, bone.rotation);
                eprintln!("Resolved values: orientation={}, slope={}, rotation={}", 
                         bone.resolved_orientation, bone.resolved_slope, bone.resolved_rotation);
                
                current_group.bones.insert(current_path.to_string(), bone);
            }
            
            // Process nested tables as subgroups
            for (key, value) in table {
                if let Some(nested_table) = value.as_table() {
                    // Skip keys that are bone properties
                    if key == "length" || key == "orientation" || key == "slope" || key == "rotation" || key == "skin_verts" {
                        continue;
                    }
                    
                    let new_path = if current_path.is_empty() {
                        key.clone()
                    } else {
                        format!("{}.{}", current_path, key)
                    };
                    
                    eprintln!("Processing subgroup {} at path {}", key, new_path);
                    
                    // Recursively collect bones from subgroup
                    let subgroup = collect_bones(nested_table, &new_path, bones);
                    
                    // Add subgroup if it has bones or subgroups
                    if !subgroup.bones.is_empty() || !subgroup.subgroups.is_empty() {
                        eprintln!("Adding subgroup {} with {} bones and {} subgroups", 
                                key, subgroup.bones.len(), subgroup.subgroups.len());
                        eprintln!("Subgroup bones: {:?}", subgroup.bones);
                        eprintln!("Subgroup subgroups: {:?}", subgroup.subgroups);
                        current_group.subgroups.insert(key.clone(), subgroup);
                    }
                }
            }
            
            // Store the group in the bones map
            if !current_group.bones.is_empty() || !current_group.subgroups.is_empty() {
                eprintln!("Inserting group at path {} with {} bones and {} subgroups", 
                         current_path, current_group.bones.len(), current_group.subgroups.len());
                eprintln!("Group bones: {:?}", current_group.bones);
                eprintln!("Group subgroups: {:?}", current_group.subgroups);
                bones.insert(current_path.to_string(), current_group.clone());
            }
            
            current_group
        }

        if let Some(body) = config.get("body").and_then(|v| v.as_table()) {
            eprintln!("\n!!! BODY SECTION FOUND !!!");
            eprintln!("Body table keys: {:?}", body.keys().collect::<Vec<_>>());
            let root_group = collect_bones(body, "body", &mut bones);
            bones.insert("body".to_string(), root_group);
            eprintln!("\n!!! BONES COLLECTED !!!");
            eprintln!("Created {} bones", bones.len());
            
            // Debug output for all bones
            eprintln!("\n!!! ALL BONES IN MAP !!!");
            for (path, group) in &bones {
                eprintln!("Group at {}: {} bones, {} subgroups", 
                         path, group.bones.len(), group.subgroups.len());
                for (bone_name, bone) in &group.bones {
                    eprintln!("  Bone: {} -> length={}, orientation={:?}, slope={:?}, rotation={:?}", 
                             bone_name, bone.length, bone.orientation, bone.slope, bone.rotation);
                }
                for (subname, subgroup) in &group.subgroups {
                    eprintln!("  Subgroup {}: {} bones, {} subgroups", 
                             subname, subgroup.bones.len(), subgroup.subgroups.len());
                }
            }
            
            Ok(Model(bones))
        } else {
            Err("No [body] section found in TOML".into())
        }
    }

    pub fn get_bones(&self) -> HashMap<String, Bone> {
        let mut bones = HashMap::new();
        
        fn collect_bones_from_group(group: &Group, bones: &mut HashMap<String, Bone>) {
            eprintln!("Collecting bones from group with {} bones and {} subgroups", 
                     group.bones.len(), group.subgroups.len());
            
            // Add bones from current group
            for (name, bone) in &group.bones {
                eprintln!("Adding bone {} -> length={}, orientation={:?}, slope={:?}, rotation={:?}", 
                         name, bone.length, bone.orientation, bone.slope, bone.rotation);
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
        
        eprintln!("Collected {} bones total", bones.len());
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
                eprintln!("Warning: Cycle detected at bone {}", bone_name);
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

            eprintln!("Calculated depth for {}: {}", bone_name, depth);
            depths.insert(bone_name.to_string(), depth);
            depth
        }

        // Calculate depth for each bone
        let mut visited = HashSet::new();
        for bone_name in bones.keys() {
            calculate_depth(bone_name, &bones, &mut depths, &mut visited);
        }

        // Debug output
        eprintln!("\n!!! BONE DEPTHS !!!");
        let mut depth_list: Vec<_> = depths.iter().collect();
        depth_list.sort_by_key(|(_, &depth)| depth);
        for (bone, depth) in depth_list {
            eprintln!("Bone: {} -> Depth: {}", bone, depth);
        }

        depths
    }

    pub fn calculate_bone_positions(&self) -> Vec<(String, Vec3, Vec3, [f32; 3])> {
        let mut positions = Vec::new();
        let bones = self.get_bones();
        
        eprintln!("\n!!! CALCULATING BONE POSITIONS !!!");
        eprintln!("Found {} bones: {:?}", bones.len(), bones.keys().collect::<Vec<_>>());
        
        for (name, bone) in bones {
            let level = name.matches('.').count();
            let color = match level {
                0 => [1.0, 0.0, 0.0], // Red
                1 => [0.0, 0.0, 1.0], // Blue
                2 => [1.0, 0.0, 1.0], // Magenta
                _ => [1.0, 1.0, 1.0], // White
            };
            
            eprintln!("\nProcessing bone: {}", name);
            eprintln!("Level: {}, Color: {:?}", level, color);
            eprintln!("Properties: length={}, raw orientation={:?}, raw slope={:?}, raw rotation={:?}", 
                     bone.length, bone.orientation, bone.slope, bone.rotation);
            eprintln!("Resolved values: orientation={}, slope={}, rotation={}", 
                     bone.resolved_orientation, bone.resolved_slope, bone.resolved_rotation);
            
            let start = Vec3::ZERO;
            
            // Get base direction from orientation and slope
            let direction = calculate_direction(bone.resolved_orientation, bone.resolved_slope);
            eprintln!("Final direction vector: {:?}", direction);
            
            let end = start + (direction * bone.length);
            eprintln!("Start: {:?}, End: {:?}", start, end);
            
            positions.push((name.clone(), start, end, color));
            
            eprintln!("Final position: start={:?}, end={:?}, direction={:?}", start, end, direction);
        }
        
        eprintln!("\n!!! BONE POSITIONS CALCULATED !!!");
        eprintln!("Total positions: {}", positions.len());
        for (name, start, end, color) in &positions {
            eprintln!("  {}: start={:?}, end={:?}, color={:?}", name, start, end, color);
        }
        
        positions
    }
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