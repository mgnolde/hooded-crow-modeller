use std::collections::HashMap;
use bevy::math::Vec3;
use serde::Deserialize;
use serde_json::Value;
use bevy::prelude::*;

#[derive(Debug, Deserialize)]
pub struct MeshData {
    pub bones: HashMap<String, Bone>,
}

#[derive(Debug, Deserialize)]
pub struct Bone {
    #[serde(default)]
    pub parent: String,  // Empty string for root bone
    #[serde(default)]
    pub child: Value,    // Can be either String or Vec<String>, empty string for end bones
    pub orientation: f32,
    pub slope: f32,
    pub rotation: f32,
    pub length: f32,
}

impl Bone {
    pub fn get_children(&self) -> Vec<String> {
        match &self.child {
            Value::String(s) => {
                if s.is_empty() {
                    vec![]
                } else {
                    vec![s.clone()]
                }
            }
            Value::Array(arr) => arr.iter()
                .filter_map(|v| v.as_str())
                .map(String::from)
                .collect(),
            _ => vec![]
        }
    }

    pub fn has_parent(&self) -> bool {
        !self.parent.is_empty()
    }

    // Calculate the end point of this bone relative to its start point
    pub fn get_end_point(&self) -> Vec3 {
        // Convert angles to radians
        let orientation_rad = self.orientation.to_radians();
        let slope_rad = self.slope.to_radians();
        
        // For a bone of length L:
        // x = L * cos(slope) * cos(orientation)
        // y = L * sin(slope)
        // z = L * cos(slope) * sin(orientation)
        let x = self.length * slope_rad.cos() * orientation_rad.cos();
        let y = self.length * slope_rad.sin();
        let z = self.length * slope_rad.cos() * orientation_rad.sin();
        
        let end = Vec3::new(x, y, z);
        println!("  End point calculation for bone:");
        println!("    orientation: {}° ({}rad)", self.orientation, orientation_rad);
        println!("    slope: {}° ({}rad)", self.slope, slope_rad);
        println!("    length: {}", self.length);
        println!("    end point vector: ({}, {}, {})", x, y, z);
        
        end
    }
}

impl MeshData {
    pub fn resolve_positions(&self) -> HashMap<String, BonePosition> {
        println!("\n=== Bone Hierarchy Resolution ===");
        let mut cache = HashMap::new();
        let mut positions = HashMap::new();
        
        // First, print the bone hierarchy
        println!("Bone hierarchy:");
        for (name, bone) in &self.bones {
            if !bone.has_parent() {
                self.print_bone_hierarchy(name, &mut vec![], 0);
            }
        }
        
        println!("\nCalculating positions:");
        for (name, _) in &self.bones {
            println!("\n[{}]", name);
            self.resolve_bone_position(name, &mut cache, &mut positions);
        }
        
        positions
    }

    fn print_bone_hierarchy(&self, bone_name: &str, visited: &mut Vec<String>, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{}└─ {}", indent, bone_name);
        
        if visited.contains(&bone_name.to_string()) {
            println!("{}   (circular reference detected)", indent);
            return;
        }
        
        visited.push(bone_name.to_string());
        
        if let Some(bone) = self.bones.get(bone_name) {
            for child in bone.get_children() {
                self.print_bone_hierarchy(&child, visited, depth + 1);
            }
        }
        
        visited.pop();
    }

    fn resolve_bone_position(
        &self,
        bone_name: &str,
        cache: &mut HashMap<String, BonePosition>,
        positions: &mut HashMap<String, BonePosition>,
    ) -> BonePosition {
        if let Some(pos) = cache.get(bone_name) {
            println!("  ├─ Using cached position");
            return pos.clone();
        }

        let bone = &self.bones[bone_name];
        let position = if bone.has_parent() {
            let parent_name = &bone.parent;
            println!("  ├─ Needs parent '{}' position first", parent_name);
            let parent_pos = self.resolve_bone_position(parent_name, cache, positions);
            
            let end = calculate_bone_end(bone, &parent_pos);
            println!("  ├─ Position calculated relative to parent");
            println!("  └─ Final: start={:?}, end={:?}", parent_pos.end, parent_pos.end + end);
            BonePosition {
                start: parent_pos.end,
                end: parent_pos.end + end,
            }
        } else {
            let end = calculate_bone_end(bone, &BonePosition::default());
            println!("  ├─ Root bone, starting at origin");
            println!("  └─ Final: start={:?}, end={:?}", Vec3::ZERO, end);
            BonePosition {
                start: Vec3::ZERO,
                end,
            }
        };

        cache.insert(bone_name.to_string(), position.clone());
        positions.insert(bone_name.to_string(), position.clone());
        
        position
    }
}

fn calculate_bone_end(bone: &Bone, parent_pos: &BonePosition) -> Vec3 {
    let orientation_rad = bone.orientation.to_radians();
    let slope_rad = bone.slope.to_radians();
    let rotation_rad = bone.rotation.to_radians();

    let direction = Vec3::new(
        orientation_rad.sin() * slope_rad.cos(),
        slope_rad.sin(),
        orientation_rad.cos() * slope_rad.cos(),
    ).normalize() * bone.length;

    direction
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

#[derive(Debug, Deserialize)]
pub struct Model {
    pub bones: HashMap<String, Bone>,
}

impl Model {
    fn calculate_bone_positions(&self) -> Vec<(String, Vec3, Vec3)> {
        let mut positions = Vec::new();
        let mut processed = std::collections::HashSet::new();
        println!("\n=== Bone Position Calculation Paths ===");
        
        // Helper function to process a bone and its dependencies
        fn process_bone(
            bone_name: &str,
            path: &[String],
            model: &Model,
            positions: &mut Vec<(String, Vec3, Vec3)>,
            processed: &mut std::collections::HashSet<String>
        ) -> (Vec3, Vec3) {
            // Log the current path and bone being processed
            let path_str = if path.is_empty() {
                format!("{}", bone_name)
            } else {
                format!("{} -> {}", path.join(" -> "), bone_name)
            };
            println!("Processing: {}", path_str);
            
            if processed.contains(bone_name) {
                let pos = positions.iter()
                    .find(|(name, _, _)| name == bone_name)
                    .map(|(_, start, end)| (*start, *end))
                    .unwrap();
                println!("  └─ Already processed, using cached position");
                return pos;
            }
            
            let bone = &model.bones[bone_name];
            let (start_point, end_point) = if bone.parent.is_empty() {
                println!("  └─ Root bone, starting at origin");
                let end = bone.get_end_point();
                (Vec3::ZERO, end)
            } else {
                let parent_name = bone.parent.replace("bones.", "");
                
                // Add current bone to path and process parent
                let mut new_path = path.to_vec();
                new_path.push(bone_name.to_string());
                let (_, parent_end) = process_bone(&parent_name, &new_path, model, positions, processed);
                
                let relative_end = bone.get_end_point();
                println!("  └─ Calculated position relative to parent '{}'", parent_name);
                (parent_end, parent_end + relative_end)
            };
            
            positions.push((bone_name.to_string(), start_point, end_point));
            processed.insert(bone_name.to_string());
            
            (start_point, end_point)
        }
        
        // Process all bones with path tracking
        for name in self.bones.keys() {
            if !processed.contains(name.as_str()) {
                process_bone(name, &[], self, &mut positions, &mut processed);
            }
        }
        
        positions
    }

    pub fn get_bone_chain(&self) -> (Vec<[f32; 3]>, Vec<u32>) {
        let positions = self.calculate_bone_positions();
        
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        println!("\n=== Building bone chain ===");
        println!("Final bone positions:");
        for (name, start, end) in positions.iter() {
            println!("Bone {}: start={:?}, end={:?}", name, start, end);
            vertices.push([start.x, start.y, start.z]);
            vertices.push([end.x, end.y, end.z]);
            
            let idx = vertices.len() - 2;
            println!("Adding line: {} -> {}", idx, idx + 1);
            indices.push(idx as u32);
            indices.push((idx + 1) as u32);
        }
        
        println!("Final vertices: {:?}", vertices);
        println!("Final indices: {:?}", indices);
        
        (vertices, indices)
    }
} 