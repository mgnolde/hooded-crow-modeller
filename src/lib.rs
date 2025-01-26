use bevy::math::Vec3;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Deserialize, Serialize)]
pub struct Model {
    #[serde(flatten)]
    pub groups: HashMap<String, BoneGroup>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct BoneGroup {
    #[serde(default)]  // Makes bones optional with default of empty Vec
    pub bones: Vec<Bone>,
    #[serde(default)]  // Makes subgroups optional with default of empty HashMap
    pub subgroups: HashMap<String, BoneGroup>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Bone {
    pub name: String,
    pub parent: String,
    pub orientation: f32,
    pub slope: f32,
    pub rotation: f32,
    pub length: f32,
}

#[derive(Clone, Debug)]
pub struct BonePosition {
    pub start: Vec3,
    pub end: Vec3,
}

impl Default for BonePosition {
    fn default() -> Self {
        Self {
            start: Vec3::ZERO,
            end: Vec3::ZERO,
        }
    }
}

impl Bone {
    pub fn has_parent(&self) -> bool {
        !self.parent.is_empty()
    }
}

fn calculate_direction(orientation: f32, slope: f32) -> Vec3 {
    // Convert angles from degrees to radians
    let orientation_rad = orientation.to_radians();
    let slope_rad = slope.to_radians();

    // Calculate direction vector
    Vec3::new(
        slope_rad.sin() * orientation_rad.sin(),  // X component
        slope_rad.cos(),                          // Y component
        slope_rad.sin() * orientation_rad.cos()   // Z component
    )
}

impl Model {
    pub fn get_bones(&self) -> HashMap<String, Bone> {
        let mut bones = HashMap::new();
        
        fn add_bones_from_group(
            bones: &mut HashMap<String, Bone>,
            group: &BoneGroup,
            path: &str
        ) {
            // Add bones from current group (if any)
            for bone in &group.bones {
                eprintln!("Adding bone '{}' from path '{}'", bone.name, path);
                bones.insert(bone.name.clone(), bone.clone());
            }
            
            // Recursively add bones from all nested subgroups
            for (name, subgroup) in &group.subgroups {
                let new_path = if path.is_empty() {
                    name.clone()
                } else {
                    format!("{}.{}", path, name)
                };
                eprintln!("Processing subgroup: {}", new_path);
                
                // Process the subgroup's bones
                add_bones_from_group(bones, subgroup, &new_path);
                
                // Also process any nested subgroups within this subgroup
                for (sub_name, sub_subgroup) in &subgroup.subgroups {
                    let sub_path = format!("{}.{}", new_path, sub_name);
                    eprintln!("Processing nested subgroup: {}", sub_path);
                    add_bones_from_group(bones, sub_subgroup, &sub_path);
                }
            }
        }
        
        // Process all groups
        eprintln!("\n=== Processing Bone Groups ===");
        for (name, group) in &self.groups {
            eprintln!("Processing top-level group: {}", name);
            add_bones_from_group(&mut bones, group, name);
        }
        
        eprintln!("\n=== Bone Collection Summary ===");
        eprintln!("Total bones collected: {}", bones.len());
        for name in bones.keys() {
            eprintln!("Collected bone: {}", name);
        }
        
        bones
    }

    pub fn get_bone_chain(&self) -> (Vec<[f32; 3]>, Vec<u32>) {
        let positions = self.calculate_bone_positions();
        
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        for (_, start, end) in positions.iter() {
            vertices.push([start.x, start.y, start.z]);
            vertices.push([end.x, end.y, end.z]);
            
            let idx = vertices.len() - 2;
            indices.push(idx as u32);
            indices.push((idx + 1) as u32);
        }
        
        (vertices, indices)
    }

    fn calculate_bone_positions(&self) -> Vec<(String, Vec3, Vec3)> {
        let mut positions = Vec::new();
        let mut processed = std::collections::HashSet::new();
        let bones = self.get_bones();  // Get all bones once
        
        fn process_bone(
            bone_name: &str,
            bones: &HashMap<String, Bone>,
            positions: &mut Vec<(String, Vec3, Vec3)>,
            processed: &mut std::collections::HashSet<String>
        ) -> (Vec3, Vec3) {
            if processed.contains(bone_name) {
                let (_, start, end) = positions.iter()
                    .find(|(name, _, _)| name == bone_name)
                    .expect("Bone was marked as processed but not found");
                return (*start, *end);
            }

            let bone = bones.get(bone_name)
                .expect("Bone not found");

            let (start_point, end_point) = if bone.parent.is_empty() {
                let direction = calculate_direction(bone.orientation, bone.slope);
                let end = direction * bone.length;
                (Vec3::ZERO, end)
            } else {
                let parent_name = bone.parent.strip_prefix("bones.").unwrap_or(&bone.parent);
                let (_, parent_end) = process_bone(parent_name, bones, positions, processed);
                
                let direction = calculate_direction(bone.orientation, bone.slope);
                let relative_end = direction * bone.length;
                
                (parent_end, parent_end + relative_end)
            };
            
            positions.push((bone_name.to_string(), start_point, end_point));
            processed.insert(bone_name.to_string());
            
            (start_point, end_point)
        }
        
        // Process all bones
        let all_bones: Vec<String> = bones.keys().cloned().collect();

        for name in all_bones {
            if !processed.contains(&name) {
                process_bone(&name, &bones, &mut positions, &mut processed);
            }
        }
        
        positions
    }
}