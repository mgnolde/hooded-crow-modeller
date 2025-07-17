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
    #[serde(default = "default_bone_length", alias = "len")]
    pub length: f32,
    #[serde(default, alias = "orient")]
    pub orientation: Option<f32>,
    #[serde(default)]
    pub slope: Option<f32>,
    #[serde(default, alias = "rot")]
    pub rotation: Option<f32>,
    #[serde(default)]
    pub skin_verts: Vec<SkinVert>,
    #[serde(default)]
    pub triangle_defs: HashMap<String, TriangleDefinition>,
    #[serde(default, alias = "col")]
    pub color: Option<[f32; 4]>,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TriangleDefinition {
    pub verts: Vec<String>,  // Vertex IDs
    pub color: Option<[f32; 4]>,  // Optional color for the triangle
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SkinVert {
    #[serde(default, alias = "len")]
    pub frac: f32,  // Fractional position along the bone (0.0 = start, 1.0 = end)
    #[serde(default, alias = "dist")]
    pub distance: f32,        // Distance from the bone
    #[serde(default, alias = "rot")]
    pub rotation: f32,  // Rotation around the bone in degrees
    #[serde(default = "default_weight")]
    pub weight: f32,          // Weight of influence
    #[serde(default)]
    pub id: Option<String>,   // Optional identifier for the skin vertex
    #[serde(default, alias = "tri")]
    pub triangles: HashMap<String, usize>, // Triangle references: face_name -> vertex position
}

impl Default for SkinVert {
    fn default() -> Self {
        eprintln!("[SKINVERT DEBUG] SkinVert::default() called. Creating default SkinVert: frac=0.5, dist=0.0, rot=0.0");
        Self {
            frac: 0.5,  // Default to middle of the bone
            distance: 0.0,        // Default to on the bone
            rotation: 0.0,        // Default to 0 degrees
            weight: 1.0,          // Default to full weight
            id: None,             // Default to no id
            triangles: HashMap::new(), // Default to no triangle references
        }
    }
}

impl SkinVert {
    /// Calculate the position of this skin vertex relative to the bone
    pub fn calculate_position(&self, bone_start: Vec3, bone_end: Vec3, bone_rotation: f32) -> Vec3 {
        eprintln!("[SKINVERT DEBUG] CALC_POS for SV id: {:?}, frac: {}, dist: {}, sv_rot: {}", self.id, self.frac, self.distance, self.rotation);
        eprintln!("[SKINVERT DEBUG]   CALC_POS inputs: bone_start: {:?}, bone_end: {:?}, bone_rotation (deg): {}", bone_start, bone_end, bone_rotation);
        // Calculate the direction of the bone
        let bone_direction = (bone_end - bone_start).normalize();
        let bone_length = (bone_end - bone_start).length();
        
        // Find position along the bone based on frac
        let segment_position = bone_start + bone_direction * bone_length * self.frac;
        
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
        eprintln!("[SKINVERT DEBUG]   CALC_POS intermediates: bone_direction: {:?}, bone_length: {}", bone_direction, bone_length);
        eprintln!("[SKINVERT DEBUG]   CALC_POS segment_position (on bone): {:?}", segment_position);
        eprintln!("[SKINVERT DEBUG]   CALC_POS perp1: {:?}, perp2: {:?}", perp1, perp2);
        eprintln!("[SKINVERT DEBUG]   CALC_POS combined_angle_rad: {:.4} (sv_rot {} + bone_rot {}) -> total_deg: {}", combined_angle_rad, self.rotation, bone_rotation, self.rotation + bone_rotation);
        eprintln!("[SKINVERT DEBUG]   CALC_POS offset_vector: {:?}", offset);
        let final_position = segment_position + offset;
        eprintln!("[SKINVERT DEBUG]   CALC_POS final_position: {:?}", final_position);
        // Final position is the base position plus the offset
        final_position
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

fn default_bone_length() -> f32 {
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
    // Convert angles to radians
    let orientation_rad = orientation.to_radians();
    let slope_rad = (-slope).to_radians(); // Invert sign so positive slopes point up
    
    // For vertical slopes (±90°), now respect orientation
    if (slope - 90.0).abs() < 0.001 {
        // +90° points up, but use orientation to determine direction in XZ plane
        return Vec3::new(
            0.2 * orientation_rad.sin(), // Small X component based on orientation
            0.98, // Mostly Y (up)
            0.2 * orientation_rad.cos()  // Small Z component based on orientation
        ).normalize();
    } else if (slope + 90.0).abs() < 0.001 {
        // -90° points down, but use orientation to determine direction in XZ plane
        return Vec3::new(
            0.2 * orientation_rad.sin(), // Small X component based on orientation
            -0.98, // Mostly Y (down)
            0.2 * orientation_rad.cos() // Small Z component based on orientation
        ).normalize();
    } else {
        // For non-vertical slopes, use spherical coordinates
        // First apply orientation in XZ plane, then apply slope
        // This matches the reordered calculate_transform function behavior
        Vec3::new(
            slope_rad.cos() * orientation_rad.sin(),
            slope_rad.sin(),
            slope_rad.cos() * orientation_rad.cos()
        ).normalize()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Triangle {
    pub name: String,
    pub vertices: [Vec3; 3],
    pub bone_names: [String; 3],
    pub color: [f32; 4], // RGBA color for the triangle
}

impl Model {
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
    

    // Helper function to create an empty TOML table
    fn empty_table() -> &'static toml::value::Table {
        static EMPTY_TABLE: std::sync::OnceLock<toml::value::Table> = std::sync::OnceLock::new();
        EMPTY_TABLE.get_or_init(|| toml::value::Table::new())
    }
    
    pub fn from_toml(input: &str) -> Result<Self, Box<dyn Error>> {
        let toml_data = input.parse::<toml::Value>()?;
        let root = toml_data.as_table().unwrap_or_else(|| Self::empty_table());

        println!("[MODEL] Parsing TOML data into model");
        println!("[MODEL] Root keys: {:?}", root.keys().collect::<Vec<_>>());

        // Recursive function to find triangles in nested structures
        fn find_triangle_tables(prefix: &str, table: &toml::value::Table) {
            // First check if this table has a triangles key
            if table.contains_key("triangles") {
                println!("[MODEL] Found triangles key in {}", prefix);
                if let Some(triangles_table) = table.get("triangles").and_then(|t| t.as_table()) {
                    println!("[MODEL] Triangle table in {} contains {} entries: {:?}", 
                            prefix, triangles_table.len(), triangles_table.keys().collect::<Vec<_>>());
                    
                    // Examine each triangle definition
                    for (triangle_name, triangle_data) in triangles_table {
                        if let Some(tri_table) = triangle_data.as_table() {
                            if let Some(verts_value) = tri_table.get("verts") {
                                if let Some(verts_array) = verts_value.as_array() {
                                    println!("[MODEL] Triangle '{}' has vertices: {:?}", 
                                        triangle_name, verts_array.iter().map(|v| v.as_str().unwrap_or("")).collect::<Vec<_>>());
                                }
                            }
                        }
                    }
                }
            }
            
            // Then recursively check all nested tables
            for (key, value) in table {
                if let Some(nested_table) = value.as_table() {
                    let new_prefix = if prefix.is_empty() {
                        key.clone()
                    } else {
                        format!("{}.{}", prefix, key)
                    };
                    find_triangle_tables(&new_prefix, nested_table);
                }
            }
        }
        
        // Search for triangles in the entire TOML structure
        find_triangle_tables("", root);

        let mut model = Model(HashMap::new());


        fn find_parent_bone<'a>(path: &str, bones: &'a HashMap<String, Group>) -> Option<&'a Bone> {
            // Iterate through all groups to find the bone with this path
            for (_group_path, group) in bones {
                if let Some(bone) = group.bones.get(path) {
                    return Some(bone);
                }
            }
            None
        }

        fn parse_triangles(table: &toml::value::Table) -> HashMap<String, TriangleDefinition> {
            println!("[TRIANGLE PARSE] Searching for triangles in table with keys: {:?}", table.keys().collect::<Vec<_>>());
            if let Some(triangles_value) = table.get("triangles") {
                println!("[TRIANGLE PARSE] Found 'triangles' key in table with type: {:?}", triangles_value.type_str());
                if let Some(triangles_table) = triangles_value.as_table() {
                    println!("[TRIANGLE PARSE] 'triangles' is a table with {} entries: {:?}", 
                       triangles_table.len(), triangles_table.keys().collect::<Vec<_>>());
                    let mut triangle_defs = HashMap::new();

                    // Process each triangle definition
                    for (triangle_name, tri_value) in triangles_table {
                        println!("[TRIANGLE PARSE] Processing triangle '{}' with type: {:?}", triangle_name, tri_value.type_str());
                        
                        // Triangle should have verts key
                        if let Some(verts_value) = tri_value.get("verts") {
                            println!("[TRIANGLE PARSE] Found 'verts' key for triangle '{}' with type: {:?}", 
                                triangle_name, verts_value.type_str());
                            
                            let mut verts = Vec::new();
                            if let Some(verts_array) = verts_value.as_array() {
                                // Verts as array: verts = ["v1", "v2", "v3"]
                                println!("[TRIANGLE PARSE] 'verts' is an array with {} elements", verts_array.len());
                                for (i, vert) in verts_array.iter().enumerate() {
                                    if let Some(id) = vert.as_str() {
                                        println!("[TRIANGLE PARSE] Adding vertex ID from array[{}]: {}", i, id);
                                        verts.push(id.to_string());
                                    } else {
                                        println!("[TRIANGLE PARSE] Warning: vertex at index {} is not a string: {:?}", i, vert);
                                    }
                                }
                            } else if let Some(verts_table) = verts_value.as_table() {
                                // Verts as table: verts = { "0" = "v1", "1" = "v2", "2" = "v3" }
                                println!("[TRIANGLE PARSE] 'verts' is a table with {} entries", verts_table.len());
                                for (idx, vert_id) in verts_table {
                                    if let Some(id) = vert_id.as_str() {
                                        println!("[TRIANGLE PARSE] Adding vertex ID from table[{}]: {}", idx, id);
                                        verts.push(id.to_string());
                                    } else {
                                        println!("[TRIANGLE PARSE] Warning: vertex with index {} is not a string: {:?}", idx, vert_id);
                                    }
                                }
                            } else {
                                println!("[TRIANGLE PARSE] Warning: 'verts' is neither an array nor a table, type is: {:?}", verts_value.type_str());
                            }
                            
                            println!("[TRIANGLE PARSE] Triangle '{}' has {} vertices: {:?}", 
                                triangle_name, verts.len(), verts);
                            
                            // Get optional color
                            let mut color = None;
                            let color_value_long = tri_value.get("color");
                            let color_value_short = tri_value.get("col");
                            println!("[TRIANGLE PARSE] Triangle '{}' color keys: long={:?}, short={:?}", 
                                triangle_name, color_value_long.is_some(), color_value_short.is_some());
                            
                            if let Some(color_value) = color_value_long.or(color_value_short) {
                                println!("[TRIANGLE PARSE] Found color for triangle '{}' with type: {:?}", 
                                    triangle_name, color_value.type_str());
                                if let Some(color_array) = color_value.as_array() {
                                    color = Some([
                                        color_array[0].as_float().unwrap_or(0.7) as f32,
                                        color_array[1].as_float().unwrap_or(0.7) as f32,
                                        color_array[2].as_float().unwrap_or(0.7) as f32,
                                        color_array.get(3).and_then(|c| c.as_float()).unwrap_or(0.5) as f32
                                    ]);
                                    println!("[TRIANGLE PARSE] Triangle '{}' has color: RGBA({:.1},{:.1},{:.1},{:.1})", 
                                triangle_name, color.unwrap()[0], color.unwrap()[1], color.unwrap()[2], color.unwrap()[3]);
                                }
                            } else {
                                println!("[TRIANGLE PARSE] Triangle '{}' has default color (semi-transparent gray)", triangle_name);
                            }
                            
                            println!("[TRIANGLE PARSE] Adding triangle '{}' with vertices: {:?}", triangle_name, verts);
                            triangle_defs.insert(triangle_name.clone(), TriangleDefinition { verts, color });
                        }
                    }
                    println!("[TRIANGLE PARSE] Total triangles parsed: {}", triangle_defs.len());
                    if !triangle_defs.is_empty() {
                        println!("[TRIANGLE PARSE] Triangle definitions: {:?}", 
                            triangle_defs.iter().map(|(k, v)| (k.clone(), v.verts.clone())).collect::<HashMap<_,_>>());
                    }
                    return triangle_defs;
                }
            }

            // If no triangles section found, return empty HashMap
            HashMap::new()
        }

        // Function to parse skin vertices from array format
        fn parse_skin_verts_from_array(skin_verts_array: &Vec<toml::Value>) -> Vec<SkinVert> {
            let result: Vec<SkinVert> = skin_verts_array.iter().map(|v| {
                let mut skin_vert = SkinVert::default();
                // Check for both long and short field names
                if let Some(segment_length) = v.get("frac")
                    .or_else(|| v.get("segment_length"))
                    .or_else(|| v.get("len"))
                    .and_then(|v| v.as_float()) {
                    skin_vert.frac = segment_length as f32;
                }
                if let Some(distance) = v.get("distance")
                    .or_else(|| v.get("dist"))
                    .and_then(|v| v.as_float()) {
                    skin_vert.distance = distance as f32;
                }
                if let Some(rotation) = v.get("rotation")
                    .or_else(|| v.get("rot"))
                    .and_then(|v| v.as_float()) {
                    skin_vert.rotation = rotation as f32;
                }
                if let Some(weight) = v.get("weight")
                    .and_then(|v| v.as_float()) {
                    skin_vert.weight = weight as f32;
                }
                
                // Parse ID
                if let Some(id) = v.get("id")
                    .and_then(|v| v.as_str()) {
                    skin_vert.id = Some(id.to_string());
                }
                
                // Parse triangle references (check both triangles and tri)
                if let Some(triangles) = v.get("triangles")
                    .or_else(|| v.get("tri"))
                    .and_then(|v| v.as_table()) {
                    for (face_name, position) in triangles {
                        if let Some(pos) = position.as_integer() {
                            skin_vert.triangles.insert(face_name.clone(), pos as usize);
                        }
                    }
                }
                
                
                skin_vert
            }).collect();
            return result;
        }
        
        fn parse_skin_verts(skin_verts_value: &toml::Value) -> Vec<SkinVert> {
            // Array format: skin_verts = [{segment_length = 0.2, ...}, {...}]
            if let Some(skin_verts_array) = skin_verts_value.as_array() {
                return parse_skin_verts_from_array(skin_verts_array);
            }
            
            // Table format: skin_verts = { "id1" = { segment_length = 0.2, ... } }
            if let Some(skin_verts_table) = skin_verts_value.as_table() {
                let result: Vec<SkinVert> = skin_verts_table.iter().map(|(id, props)| {
                    let mut skin_vert = SkinVert::default();
                    skin_vert.id = Some(id.to_string());
                    
                    // Check for both long and short field names
                    if let Some(segment_length) = props.get("frac")
                        .or_else(|| props.get("segment_length"))
                        .or_else(|| props.get("len"))
                        .and_then(|v| v.as_float()) {
                        skin_vert.frac = segment_length as f32;
                    }
                    if let Some(distance) = props.get("distance")
                        .or_else(|| props.get("dist"))
                        .and_then(|v| v.as_float()) {
                        skin_vert.distance = distance as f32;
                    }
                    // Debugging rotation parsing
                    let rot_val_long = props.get("rotation");
                    let rot_val_short = props.get("rot");
                    eprintln!("[SKINVERT DEBUG] PARSE_SKIN_VERTS (Table Format) - id={:?}: props.get(\"rotation\") is {:?}, props.get(\"rot\") is {:?}", skin_vert.id, rot_val_long, rot_val_short);

                    if let Some(val) = rot_val_long.or(rot_val_short) {
                        eprintln!("[SKINVERT DEBUG] PARSE_SKIN_VERTS (Table Format) - id={:?}: Found TOML value for rotation: {:?}", skin_vert.id, val);
                        if let Some(rotation_float) = val.as_float() {
                            eprintln!("[SKINVERT DEBUG] PARSE_SKIN_VERTS (Table Format) - id={:?}: Successfully parsed as float: {}", skin_vert.id, rotation_float);
                            skin_vert.rotation = rotation_float as f32;
                        } else if let Some(rotation_int) = val.as_integer() {
                            eprintln!("[SKINVERT DEBUG] PARSE_SKIN_VERTS (Table Format) - id={:?}: Parsed as integer: {}, converting to f32", skin_vert.id, rotation_int);
                            skin_vert.rotation = rotation_int as f32;
                        } else {
                            eprintln!("[SKINVERT DEBUG] PARSE_SKIN_VERTS (Table Format) - id={:?}: FAILED to parse TOML value {:?} as float or integer", skin_vert.id, val);
                        }
                    } else {
                        eprintln!("[SKINVERT DEBUG] PARSE_SKIN_VERTS (Table Format) - id={:?}: NEITHER \"rotation\" NOR \"rot\" key found.", skin_vert.id);
                    }
                    if let Some(weight) = props.get("weight")
                        .and_then(|v| v.as_float()) {
                        skin_vert.weight = weight as f32;
                    }
                    
                    // Parse triangle references (check both triangles and tri)
                    if let Some(triangles) = props.get("triangles")
                        .or_else(|| props.get("tri"))
                        .and_then(|v| v.as_table()) {
                        for (face_name, position) in triangles {
                            if let Some(pos) = position.as_integer() {
                                skin_vert.triangles.insert(face_name.clone(), pos as usize);
                            }
                        }
                    }
                    
                    eprintln!("[SKINVERT DEBUG] PARSE_SKIN_VERTS (Table Format) - Parsed SkinVert: id={:?}, frac={}, dist={}, rot={}", 
                               skin_vert.id, skin_vert.frac, skin_vert.distance, skin_vert.rotation);
                    
                    skin_vert
                }).collect();
                return result;
            }
            
            // Return empty collection if no valid format found
            Vec::new()
        }
        
        fn collect_bones(
            table: &toml::value::Table, 
            current_path: &str, 
            bones: &mut HashMap<String, Group>
        ) -> Group {
            println!("[COLLECT BONES] Processing path: {}, table keys: {:?}", current_path, table.keys().collect::<Vec<_>>());
            
            // Check if this table has a triangles key directly
            if table.contains_key("triangles") {
                println!("[COLLECT BONES] Found 'triangles' key at path: {}", current_path);
                println!("[COLLECT BONES] Triangles value type: {:?}", table.get("triangles").unwrap().type_str());
                
                if let Some(triangles_table) = table.get("triangles").and_then(|t| t.as_table()) {
                    println!("[COLLECT BONES] Triangles is a table with {} entries", triangles_table.len());
                    for (tri_name, tri_value) in triangles_table {
                        println!("[COLLECT BONES] Triangle '{}' has type: {:?}", tri_name, tri_value.type_str());
                        
                        if let Some(tri_table) = tri_value.as_table() {
                            println!("[COLLECT BONES] Triangle '{}' has keys: {:?}", 
                                tri_name, tri_table.keys().collect::<Vec<_>>());
                        }
                    }
                } else {
                    println!("[COLLECT BONES] Warning: 'triangles' is not a table at path: {}", current_path);
                }
            }
            
            // Create current group
            let mut current_group = Group {
                bones: HashMap::new(),
                subgroups: HashMap::new(),
            };
            
            // Check for both long and short field names
            let has_length = table.contains_key("length") || table.contains_key("len");
            
            // If this table has bone properties, add the bone to the current group
            if has_length {
                // Find parent bone to potentially inherit properties
                let parent_bone = if current_path.contains('.') {
                    let parent_path = current_path.rsplit_once('.').unwrap().0;
                    find_parent_bone(parent_path, bones)
                } else {
                    None
                };
                
                // Get length (required field)
                let length = table.get("length")
                    .or_else(|| table.get("len"))
                    .and_then(|v| v.as_float())
                    .unwrap_or(1.0) as f32;
                
                // Get optional fields, checking both long and short names
                let orientation = table.get("orientation")
                    .or_else(|| table.get("orient"))
                    .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                    .map(|v| v as f32);
                
                let slope = table.get("slope")
                    .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                    .map(|v| v as f32);
                
                let rotation = table.get("rotation")
                    .or_else(|| table.get("rot"))
                    .and_then(|v| v.as_float().or_else(|| v.as_integer().map(|i| i as f64)))
                    .map(|v| v as f32);
                
                // Check for triangles key first
                println!("[COLLECT BONES] Checking for triangles in bone '{}'", current_path);
                let has_triangles = table.contains_key("triangles");
                if has_triangles {
                    println!("[COLLECT BONES] Found triangles key in bone '{}'", current_path);
                }
                
                // Parse triangle definitions if available
                let parsed_triangles = parse_triangles(table);
                println!("[BONE CREATE] For bone '{}', triangle_defs count: {}", 
                    current_path, parsed_triangles.len());
                if !parsed_triangles.is_empty() {
                    println!("[BONE CREATE] Triangle names in '{}': {:?}", 
                        current_path, parsed_triangles.keys().collect::<Vec<_>>());
                }
                
                let mut bone = Bone {
                    length,
                    orientation,
                    slope,
                    rotation,
                    // Parse skin verts if available
                    skin_verts: if let Some(skin_verts_value) = table.get("skin_verts") {
                        parse_skin_verts(skin_verts_value)
                    } else {
                        Vec::new()
                    },
                    // Assign parsed triangle definitions
                    triangle_defs: parsed_triangles,
                    color: None,
                    resolved_orientation: 0.0,
                    resolved_slope: 0.0,
                    resolved_rotation: 0.0,
                };
                
                println!("Created bone at {}: length={}, orientation={:?}, rotation={:?}", 
                         current_path, length, orientation, rotation);
                
                // Resolve inherited values
                bone.resolve_values(parent_bone);
                
                // Extract color values from skin vertices
                if let Some(skin_verts_value) = table.get("skin_verts") {
                    if let Some(skin_verts_array) = skin_verts_value.as_array() {
                        for skin_vert in skin_verts_array {
                            if let Some(color) = skin_vert.get("color")
                                .or_else(|| skin_vert.get("col"))
                                .and_then(|v| v.as_array()) {
                                let extracted_color = [color[0].as_float().unwrap_or(1.0) as f32,
                                                       color[1].as_float().unwrap_or(1.0) as f32,
                                                       color[2].as_float().unwrap_or(1.0) as f32,
                                                       color.get(3).and_then(|c| c.as_float()).unwrap_or(1.0) as f32];
                                bone.color = Some(extracted_color);
                            }
                        }
                    }
                }
                
                // The triangle_defs were already set when creating the bone above
                println!("[COLLECT_BONES] Bone '{}' has {} triangle definitions", 
                         current_path, bone.triangle_defs.len());
                
                current_group.bones.insert(current_path.to_string(), bone);
            }
            
            // Process nested tables as subgroups
            for (key, value) in table {
                if let Some(nested_table) = value.as_table() {
                    // Skip keys that are bone properties (both long and short forms)
                    if key == "length" || key == "len" || 
                       key == "orientation" || key == "orient" || 
                       key == "slope" || 
                       key == "rotation" || key == "rot" || 
                       key == "skin_verts" || 
                       key == "triangles" || 
                       key == "color" || key == "col" {
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

        if let Some(body) = root.get("body").and_then(|v| v.as_table()) {
            let root_group = collect_bones(body, "body", &mut model.0);
            model.0.insert("body".to_string(), root_group);
            
            Ok(model)
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


    pub fn collect_triangles(&self) -> Vec<Triangle> {
        println!("[MODEL] Collecting triangles from model with {} bones", self.get_bones().len());
        let mut triangles: Vec<Triangle> = Vec::new();
        let mut vertex_map = HashMap::new();
        
        // Get bone positions
        let bone_positions = self.calculate_bone_positions();
        let bone_position_map: HashMap<_, _> = bone_positions
            .into_iter()
            .map(|(name, start, end, _)| (name, (start, end)))
            .collect();
            
        // Build vertex_map from skin verts with IDs
        let mut total_verts = 0;
        for (bone_name, bone) in self.get_bones() {
            if let Some((start, end)) = bone_position_map.get(&bone_name) {
                for sv in &bone.skin_verts {
                    if let Some(ref id) = sv.id {
                        let pos = sv.calculate_position(*start, *end, bone.resolved_rotation);
                        total_verts += 1;
                        println!("[TRIANGLE COLLECT] Storing vertex '{}' position {:?} from bone {}", id, pos, bone_name);
                        vertex_map.insert(id.clone(), (pos, bone_name.clone()));
                    } else {
                        println!("[TRIANGLE COLLECT] WARNING: Skin vertex in bone '{}' has no ID and cannot be referenced by triangles", bone_name);
                    }
                }
            }
        }
        
        println!("[TRIANGLE COLLECT] Built vertex map with {} vertices (total processed: {})", vertex_map.len(), total_verts);
        
        // Debug print all vertex IDs in the map for reference
        println!("[TRIANGLE COLLECT] Vertex IDs available: {:?}", vertex_map.keys().collect::<Vec<_>>());

        println!("[COLLECT_TRIANGLES] Starting triangle collection...");
        
        // Debug print for bone counting
        let bone_count = self.get_bones().len();
        println!("[COLLECT_TRIANGLES] Processing {} bones", bone_count);
        
        // Debug print all bones to see what's available
        println!("[COLLECT_TRIANGLES] Available bones: {:?}", 
                 self.get_bones().keys().collect::<Vec<_>>());
        
        // Iterate through all bones to collect their triangle definitions
        for (bone_name, bone) in self.get_bones() {
            println!("[COLLECT_TRIANGLES] Processing bone '{}' with {} triangle definitions", 
                     bone_name, bone.triangle_defs.len());
            
            // Debug: Print all fields of the bone to see what data it has
            println!("[COLLECT_TRIANGLES] Bone '{}' data: triangle_defs.keys={:?}", 
                     bone_name, 
                     bone.triangle_defs.keys().collect::<Vec<_>>());
            
            // Process triangle_defs in the current bone
            for (triangle_name, triangle_def) in &bone.triangle_defs {
                println!("[COLLECT_TRIANGLES] Processing triangle '{}' with {} vertex references: {:?}", 
                         triangle_name, triangle_def.verts.len(), triangle_def.verts);
                
                // Skip triangles that don't have exactly 3 vertices
                if triangle_def.verts.len() != 3 {
                    println!("[COLLECT_TRIANGLES] Skipping triangle '{}' with {} vertices (not exactly 3)", 
                             triangle_name, triangle_def.verts.len());
                    continue;
                }
                
                // Try to find all the referenced vertices in skin_verts
                let mut vertex_positions: Vec<Vec3> = Vec::with_capacity(3);
                let mut all_vertices_found = true;
                
                for vertex_id in &triangle_def.verts {
                    // Find the vertex with the given ID
                    let found_vertex = vertex_map.get(vertex_id);
                    
                    if let Some((position, bone_name)) = found_vertex {
                        vertex_positions.push(*position);
                        println!("[COLLECT_TRIANGLES] Found vertex '{}' at position: {:?} from bone {}", vertex_id, position, bone_name);
                    } else {
                        println!("[COLLECT_TRIANGLES] ERROR: Vertex '{}' not found in vertex_map", vertex_id);
                        all_vertices_found = false;
                        break;
                    }
                }
                
                // If all vertices were found, create and add the triangle
                if all_vertices_found && vertex_positions.len() == 3 {
                    triangles.push(Triangle {
                        name: format!("{}.{}", bone_name, triangle_name),
                        vertices: [vertex_positions[0], vertex_positions[1], vertex_positions[2]],
                        bone_names: [bone_name.clone(), bone_name.clone(), bone_name.clone()],
                        color: triangle_def.color.unwrap_or([0.7, 0.7, 0.7, 0.5]),
                    });
                    println!("[COLLECT_TRIANGLES] Added triangle '{}' with vertices: {:?}, {:?}, {:?}", 
                             format!("{}.{}", bone_name, triangle_name), vertex_positions[0], vertex_positions[1], vertex_positions[2]);
                } else {
                    println!("[COLLECT_TRIANGLES] Failed to add triangle '{}' due to missing vertices", triangle_name);
                }
            }
        }
        
        // Now also collect triangles from skin vertices (legacy triangle references)
        println!("[COLLECT_TRIANGLES] Processing legacy triangles from {} skin vertices", self.get_bones().values().map(|b| b.skin_verts.len()).sum::<usize>());
        
        // Debug: print a few skin vertices to see what data they have
        struct LegacyVertInfo {
            pos_index: usize,
            position: Vec3,
            bone_name: String,
        }

        let mut legacy_faces: HashMap<String, Vec<LegacyVertInfo>> = HashMap::new();
        for (bone_name, bone) in self.get_bones() {
            if let Some((start, end)) = bone_position_map.get(&bone_name) {
                for sv in &bone.skin_verts {
                    if sv.triangles.is_empty() {
                        continue;
                    }
                    let pos_calc = sv.calculate_position(*start, *end, bone.resolved_rotation);
                    for (face_name, &pos_idx) in &sv.triangles {
                        if pos_idx >= 3 {
                            eprintln!(
                                "[TRIANGLE COLLECT] Vertex id {:?} references invalid position {} in face '{}'",
                                sv.id, pos_idx, face_name
                            );
                            continue;
                        }
                        legacy_faces
                            .entry(face_name.clone())
                            .or_default()
                            .push(LegacyVertInfo {
                                pos_index: pos_idx,
                                position: pos_calc,
                                bone_name: bone_name.clone(),
                            });
                    }
                }
            }
        }
        
        for (face_name, mut verts) in legacy_faces {
            if verts.len() != 3 {
                eprintln!(
                    "[TRIANGLE COLLECT] Legacy face '{}' has {} verts, expected 3 – skipping",
                    face_name,
                    verts.len()
                );
                continue;
            }
            // order vertices by their specified position index 0,1,2
            verts.sort_by_key(|v| v.pos_index);
            let mut positions = [Vec3::ZERO; 3];
            // Fix String array initialization (String doesn't implement Copy)
            let mut bones_arr = [String::new(), String::new(), String::new()];
            for (i, v) in verts.into_iter().enumerate() {
                positions[i] = v.position;
                bones_arr[i] = v.bone_name;
            }
            triangles.push(Triangle {
                name: face_name,
                vertices: positions,
                bone_names: bones_arr,
                color: [0.6, 0.6, 0.6, 0.5], // default color for legacy triangles
            });
        }

        eprintln!("[TRIANGLE COLLECT] Total triangles created: {}", triangles.len());
        triangles
    }



// Triangle struct moved to before impl Model block

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

// End of impl Model
}