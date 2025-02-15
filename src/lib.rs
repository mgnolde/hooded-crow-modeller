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
    pub orientation: f32,
    pub slope: f32,
    pub rotation: f32,
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

impl Bone {
    pub fn has_parent(&self) -> bool {
        false
    }
}

fn calculate_direction(orientation: f32, slope: f32) -> Vec3 {
    let orientation_rad = orientation.to_radians();
    let slope_rad = slope.to_radians();
    
    Vec3::new(
        slope_rad.cos() * orientation_rad.sin(),
        slope_rad.sin(),
        slope_rad.cos() * orientation_rad.cos()
    ).normalize()
}

impl Model {
    pub fn from_toml(content: &str) -> Result<Self, Box<dyn Error>> {
        eprintln!("!!! MODEL CREATION STARTED !!!");
        eprintln!("Content:\n{}", content);
        
        let config: toml::Value = content.parse()?;
        eprintln!("Parsed TOML:\n{:#?}", config);
        
        let mut bones = HashMap::new();

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
                let bone = Bone {
                    length: table.get("length").and_then(|v| v.as_float()).unwrap_or(1.0) as f32,
                    orientation: table.get("orientation")
                        .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                        .unwrap_or(0.0) as f32,
                    slope: table.get("slope")
                        .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                        .unwrap_or(0.0) as f32,
                    rotation: table.get("rotation")
                        .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                        .unwrap_or(0.0) as f32,
                };
                
                eprintln!("Created bone: {:?}", bone);
                eprintln!("Bone properties: length={}, orientation={}, slope={}, rotation={}", 
                         bone.length, bone.orientation, bone.slope, bone.rotation);
                current_group.bones.insert(current_path.to_string(), bone);
            }
            
            // Process nested tables as subgroups
            for (key, value) in table {
                if let Some(nested_table) = value.as_table() {
                    // Skip keys that are bone properties
                    if key == "length" || key == "orientation" || key == "slope" || key == "rotation" {
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
                    eprintln!("  Bone: {} -> length={}, orientation={}, slope={}, rotation={}", 
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
                eprintln!("Adding bone {} -> length={}, orientation={}, slope={}, rotation={}", 
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
        let mut bone_ends = HashMap::new();
        let bones = self.get_bones();
        let depths = self.calculate_bone_depths();
        
        eprintln!("\n!!! CALCULATING BONE POSITIONS !!!");
        eprintln!("Found {} bones: {:?}", bones.len(), bones.keys().collect::<Vec<_>>());
        eprintln!("Bone depths: {:?}", depths);
        
        // Sort bones by depth to ensure we process parents before children
        let mut bone_list: Vec<_> = bones.iter().collect();
        bone_list.sort_by_key(|(name, _)| depths.get(*name).unwrap_or(&0));
        
        eprintln!("\n!!! SORTED BONES BY DEPTH !!!");
        for (name, bone) in &bone_list {
            eprintln!("  {} (depth: {})", name, depths.get(*name).unwrap_or(&0));
        }
        
        for (name, bone) in bone_list {
            let level = name.matches('.').count();
            let color = match level {
                0 => [1.0, 0.0, 0.0], // Red
                1 => [0.0, 0.0, 1.0], // Blue
                2 => [1.0, 0.0, 1.0], // Magenta
                _ => [1.0, 1.0, 1.0], // White
            };
            
            eprintln!("\nProcessing bone: {}", name);
            eprintln!("Level: {}, Color: {:?}", level, color);
            eprintln!("Properties: length={}, orientation={}, slope={}, rotation={}", 
                     bone.length, bone.orientation, bone.slope, bone.rotation);
            
            let start = Vec3::ZERO;
            
            // Get base direction from orientation and slope
            let direction = calculate_direction(bone.orientation, bone.slope);
            eprintln!("Base direction from orientation and slope: {:?}", direction);
            
            // Apply rotation around the parent bone's direction if this is not the root bone
            let final_direction = direction;
            
            let end = start + (final_direction * bone.length);
            
            bone_ends.insert(name.clone(), end);
            positions.push((name.clone(), start, end, color));
            
            eprintln!("Final position: start={:?}, end={:?}, direction={:?}", start, end, final_direction);
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