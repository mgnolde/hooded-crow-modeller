use bevy::math::Vec3;
use bevy::prelude::*;
use std::collections::HashMap;
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

#[derive(Debug, Clone, Serialize)]
pub struct CameraSettings {
    pub frac: f32,           // Position along bone (0.0 = start, 1.0 = end)
    pub dist: CameraDistance, // Distance from bone (can use templates like "{{waist}}")
    pub rot: f32,            // Rotation around bone in degrees
}

#[derive(Debug, Clone, Serialize)]
pub enum CameraDistance {
    Template(String),
    Resolved(f32),
}

impl<'de> Deserialize<'de> for CameraDistance {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self, Visitor};
        
        struct CameraDistanceVisitor;
        
        impl<'de> Visitor<'de> for CameraDistanceVisitor {
            type Value = CameraDistance;
            
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a string template or numeric value")
            }
            
            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(CameraDistance::Template(value.to_string()))
            }
            
            fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(CameraDistance::Resolved(value as f32))
            }
            
            fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(CameraDistance::Resolved(value as f32))
            }
        }
        
        deserializer.deserialize_any(CameraDistanceVisitor)
    }
}

impl<'de> Deserialize<'de> for CameraSettings {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self, MapAccess, Visitor};
        
        struct CameraSettingsVisitor;
        
        impl<'de> Visitor<'de> for CameraSettingsVisitor {
            type Value = CameraSettings;
            
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("camera settings")
            }
            
            fn visit_map<V>(self, mut map: V) -> Result<CameraSettings, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut frac = None;
                let mut dist = None;
                let mut rot = None;
                
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "frac" => {
                            if frac.is_some() {
                                return Err(de::Error::duplicate_field("frac"));
                            }
                            frac = Some(map.next_value()?);
                        }
                        "dist" => {
                            if dist.is_some() {
                                return Err(de::Error::duplicate_field("dist"));
                            }
                            dist = Some(map.next_value()?);
                        }
                        "rot" => {
                            if rot.is_some() {
                                return Err(de::Error::duplicate_field("rot"));
                            }
                            rot = Some(map.next_value()?);
                        }
                        _ => {
                            // Ignore unknown fields
                            let _ = map.next_value::<serde::de::IgnoredAny>()?;
                        }
                    }
                }
                
                let frac = frac.ok_or_else(|| de::Error::missing_field("frac"))?;
                let dist = dist.ok_or_else(|| de::Error::missing_field("dist"))?;
                let rot = rot.ok_or_else(|| de::Error::missing_field("rot"))?;
                
                Ok(CameraSettings { frac, dist, rot })
            }
        }
        
        deserializer.deserialize_map(CameraSettingsVisitor)
    }
}

impl CameraSettings {
    /// Calculate camera position and look target based on bone coordinates
    pub fn calculate_camera_transform(&self, bone_start: Vec3, bone_end: Vec3, bone_rotation: f32, distance: f32) -> (Vec3, Vec3) {
        // Calculate the direction of the bone
        let bone_direction = (bone_end - bone_start).normalize();
        let bone_length = (bone_end - bone_start).length();
        
        // Find position along the bone based on frac (this is what the camera looks at)
        let look_target = bone_start + bone_direction * bone_length * self.frac;
        
        // Calculate camera position by moving away from the bone at specified distance and rotation
        // Use the same logic as SkinVert but with the distance parameter
        let perpendicular = if bone_direction.y.abs() < 0.9 {
            Vec3::Y.cross(bone_direction).normalize()
        } else {
            Vec3::X.cross(bone_direction).normalize()
        };
        
        // Apply rotation around the bone
        let rotation_radians = (self.rot + bone_rotation).to_radians();
        let cos_rot = rotation_radians.cos();
        let sin_rot = rotation_radians.sin();
        
        let up = bone_direction.cross(perpendicular).normalize();
        let offset = perpendicular * cos_rot + up * sin_rot;
        
        // Position camera at distance from the look target
        let camera_position = look_target + offset * distance;
        
        (camera_position, look_target)
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct BoneSettings {
    pub cam: Option<CameraSettings>,
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
    #[serde(default)]
    pub settings: Option<BoneSettings>,
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

#[derive(Debug, Clone, Serialize)]
pub struct SkinVert {
    pub frac: f32,  // Fractional position along the bone (0.0 = start, 1.0 = end)
    pub distance: f32,        // Distance from the bone
    pub rotation: f32,  // Rotation around the bone in degrees
    pub weight: f32,          // Weight of influence
    pub id: Option<String>,   // Optional identifier for the skin vertex
    pub triangles: HashMap<String, usize>, // Triangle references: face_name -> vertex position
    pub ruler: bool,          // Show visual aids for positioning
}

impl<'de> Deserialize<'de> for SkinVert {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self, MapAccess, Visitor};
        
        struct SkinVertVisitor;
        
        impl<'de> Visitor<'de> for SkinVertVisitor {
            type Value = SkinVert;
            
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a SkinVert")
            }
            
            fn visit_map<V>(self, mut map: V) -> Result<SkinVert, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut frac = None;
                let mut distance = None;
                let mut rotation = None;
                let mut weight = None;
                let mut id = None;
                let mut triangles = None;
                let mut ruler = None;
                
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "frac" | "len" => {
                            if frac.is_some() {
                                return Err(de::Error::duplicate_field("frac"));
                            }
                            frac = Some(map.next_value()?);
                        }
                        "distance" | "dist" => {
                            if distance.is_some() {
                                return Err(de::Error::duplicate_field("distance"));
                            }
                            distance = Some(map.next_value()?);
                        }
                        "rotation" | "rot" => {
                            if rotation.is_some() {
                                return Err(de::Error::duplicate_field("rotation"));
                            }
                            rotation = Some(map.next_value()?);
                        }
                        "weight" => {
                            if weight.is_some() {
                                return Err(de::Error::duplicate_field("weight"));
                            }
                            weight = Some(map.next_value()?);
                        }
                        "id" => {
                            if id.is_some() {
                                return Err(de::Error::duplicate_field("id"));
                            }
                            id = Some(map.next_value()?);
                        }
                        "triangles" | "tri" => {
                            if triangles.is_some() {
                                return Err(de::Error::duplicate_field("triangles"));
                            }
                            triangles = Some(map.next_value()?);
                        }
                        "ruler" => {
                            if ruler.is_some() {
                                return Err(de::Error::duplicate_field("ruler"));
                            }
                            let ruler_value: bool = map.next_value()?;
                            println!("PARSE DEBUG: Found ruler field with value: {}", ruler_value);
                            ruler = Some(ruler_value);
                        }
                        _ => {
                            // Ignore unknown fields
                            let _ = map.next_value::<serde::de::IgnoredAny>()?;
                        }
                    }
                }
                
                let frac = frac.unwrap_or(0.5);
                let distance = distance.unwrap_or(0.0);
                let rotation = rotation.unwrap_or(0.0);
                let weight = weight.unwrap_or(1.0);
                let triangles = triangles.unwrap_or_else(HashMap::new);
                let ruler = ruler.unwrap_or(false);
                
                let result = SkinVert { frac, distance, rotation, weight, id, triangles, ruler };
                
                if ruler {
                    println!("PARSE DEBUG: Created SkinVert with ruler=true: {:?}", result);
                }
                
                Ok(result)
            }
        }
        
        deserializer.deserialize_map(SkinVertVisitor)
    }
}

impl Default for SkinVert {
    fn default() -> Self {
        Self {
            frac: 0.5,  // Default to middle of the bone
            distance: 0.0,        // Default to on the bone
            rotation: 0.0,        // Default to 0 degrees
            weight: 1.0,          // Default to full weight
            id: None,             // Default to no id
            triangles: HashMap::new(), // Default to no triangle references
            ruler: false,         // Default to no ruler visualization
        }
    }
}

impl SkinVert {
    /// Calculate the position of this skin vertex relative to the bone
    pub fn calculate_position(&self, bone_start: Vec3, bone_end: Vec3, bone_rotation: f32) -> Vec3 {
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
        let final_position = segment_position + offset;
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
        // Parse TOML to extract variables and process templates
        let mut toml_data = input.parse::<toml::Value>()?;
        
        // Extract and resolve all variables from [body] section (including derived ones)
        let variables = Self::extract_and_resolve_variables(&mut toml_data)?;
        
        // Process templates in the entire TOML structure
        Self::process_templates(&mut toml_data, &variables)?;
        let root = toml_data.as_table().unwrap_or_else(|| Self::empty_table());


        // Recursive function to find triangles in nested structures
        fn find_triangle_tables(prefix: &str, table: &toml::value::Table) {
            // First check if this table has a triangles key
            if table.contains_key("triangles") {
                if let Some(triangles_table) = table.get("triangles").and_then(|t| t.as_table()) {
                    
                    // Examine each triangle definition
                    for (_triangle_name, triangle_data) in triangles_table {
                        if let Some(tri_table) = triangle_data.as_table() {
                            if let Some(verts_value) = tri_table.get("verts") {
                                if let Some(_verts_array) = verts_value.as_array() {
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
            if let Some(triangles_value) = table.get("triangles") {
                if let Some(triangles_table) = triangles_value.as_table() {
                    let mut triangle_defs = HashMap::new();

                    // Process each triangle definition
                    for (triangle_name, tri_value) in triangles_table {
                        
                        // Triangle should have verts key
                        if let Some(verts_value) = tri_value.get("verts") {
                            
                            let mut verts = Vec::new();
                            if let Some(verts_array) = verts_value.as_array() {
                                // Verts as array: verts = ["v1", "v2", "v3"]
                                for (_i, vert) in verts_array.iter().enumerate() {
                                    if let Some(id) = vert.as_str() {
                                        verts.push(id.to_string());
                                    } else {
                                    }
                                }
                            } else if let Some(verts_table) = verts_value.as_table() {
                                // Verts as table: verts = { "0" = "v1", "1" = "v2", "2" = "v3" }
                                for (_idx, vert_id) in verts_table {
                                    if let Some(id) = vert_id.as_str() {
                                        verts.push(id.to_string());
                                    } else {
                                    }
                                }
                            } else {
                            }
                            
                            
                            // Get optional color
                            let mut color = None;
                            let color_value_long = tri_value.get("color");
                            let color_value_short = tri_value.get("col");
                            
                            if let Some(color_value) = color_value_long.or(color_value_short) {
                                if let Some(color_array) = color_value.as_array() {
                                    color = Some([
                                        color_array[0].as_float().unwrap_or(0.7) as f32,
                                        color_array[1].as_float().unwrap_or(0.7) as f32,
                                        color_array[2].as_float().unwrap_or(0.7) as f32,
                                        color_array.get(3).and_then(|c| c.as_float()).unwrap_or(0.5) as f32
                                    ]);
                                }
                            } else {
                            }
                            
                            triangle_defs.insert(triangle_name.clone(), TriangleDefinition { verts, color });
                        }
                    }
                    if !triangle_defs.is_empty() {
                        // Triangle definitions parsed successfully
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
                
                // Parse ruler field
                if let Some(ruler) = v.get("ruler").and_then(|v| v.as_bool()) {
                    skin_vert.ruler = ruler;
                    if ruler {
                        println!("PARSE DEBUG: Setting ruler=true for array vertex");
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

                    if let Some(val) = rot_val_long.or(rot_val_short) {
                        if let Some(rotation_float) = val.as_float() {
                            skin_vert.rotation = rotation_float as f32;
                        } else if let Some(rotation_int) = val.as_integer() {
                            skin_vert.rotation = rotation_int as f32;
                        } else {
                        }
                    } else {
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
                    
                    // Parse ruler field
                    if let Some(ruler) = props.get("ruler").and_then(|v| v.as_bool()) {
                        skin_vert.ruler = ruler;
                        if ruler {
                            println!("PARSE DEBUG: Setting ruler=true for vertex '{}'", id);
                        }
                    }
                    
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
            
            // Check if this table has a triangles key directly
            if table.contains_key("triangles") {
                
                if let Some(triangles_table) = table.get("triangles").and_then(|t| t.as_table()) {
                    for (_tri_name, tri_value) in triangles_table {
                        
                        if let Some(_tri_table) = tri_value.as_table() {
                            // Triangle table found
                        }
                    }
                } else {
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
                println!("Parsing bone at path: {}, table keys: {:?}", current_path, table.keys().collect::<Vec<_>>());
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
                let has_triangles = table.contains_key("triangles");
                if has_triangles {
                }
                
                // Parse triangle definitions if available
                let parsed_triangles = parse_triangles(table);
                if !parsed_triangles.is_empty() {
                }
                
                let mut bone = Bone {
                    length,
                    orientation,
                    slope,
                    rotation,
                    // Parse skin verts if available
                    skin_verts: if let Some(skin_verts_value) = table.get("verts").or_else(|| table.get("skin_verts")) {
                        parse_skin_verts(skin_verts_value)
                    } else {
                        Vec::new()
                    },
                    // Assign parsed triangle definitions
                    triangle_defs: parsed_triangles,
                    color: None,
                    settings: if let Some(cam_value) = table.get("cam") {
                        println!("Found 'cam' in TOML for bone, attempting to parse: {:?}", cam_value);
                        // Parse camera settings directly from bone
                        let cam_settings: Result<CameraSettings, _> = cam_value.clone().try_into();
                        match cam_settings {
                            Ok(cam) => {
                                println!("Successfully parsed camera settings: frac={}, dist={:?}, rot={}", cam.frac, cam.dist, cam.rot);
                                Some(BoneSettings { cam: Some(cam) })
                            },
                            Err(e) => {
                                println!("Failed to parse camera settings: {:?}", e);
                                None
                            },
                        }
                    } else if let Some(settings_value) = table.get("settings") {
                        // Parse settings section
                        settings_value.clone().try_into().ok()
                    } else {
                        None
                    },
                    resolved_orientation: 0.0,
                    resolved_slope: 0.0,
                    resolved_rotation: 0.0,
                };
                
                
                // Resolve inherited values
                bone.resolve_values(parent_bone);
                
                // Extract color values from skin vertices
                if let Some(skin_verts_value) = table.get("verts").or_else(|| table.get("skin_verts")) {
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
                       key == "skin_verts" || key == "verts" || 
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



    pub fn collect_triangles(&self) -> Vec<Triangle> {
        let mut triangles: Vec<Triangle> = Vec::new();
        let mut vertex_map = HashMap::new();
        
        // Get bone positions
        let bone_positions = self.calculate_bone_positions();
        let bone_position_map: HashMap<_, _> = bone_positions
            .into_iter()
            .map(|(name, start, end, _)| (name, (start, end)))
            .collect();
            
        // Build vertex_map from skin verts with IDs
        let mut _total_verts = 0;
        for (bone_name, bone) in self.get_bones() {
            if let Some((start, end)) = bone_position_map.get(&bone_name) {
                for sv in &bone.skin_verts {
                    if let Some(ref id) = sv.id {
                        let pos = sv.calculate_position(*start, *end, bone.resolved_rotation);
                        _total_verts += 1;
                        vertex_map.insert(id.clone(), (pos, bone_name.clone()));
                    } else {
                    }
                }
            }
        }
        
        
        // Debug print all vertex IDs in the map for reference

        
        // Debug print for bone counting
        let _bone_count = self.get_bones().len();
        
        // Debug print all bones to see what's available
        
        // Iterate through all bones to collect their triangle definitions
        for (bone_name, bone) in self.get_bones() {
            
            // Process triangle_defs in the current bone
            for (triangle_name, triangle_def) in &bone.triangle_defs {
                
                // Skip triangles that don't have exactly 3 vertices
                if triangle_def.verts.len() != 3 {
                    continue;
                }
                
                // Try to find all the referenced vertices in skin_verts
                let mut vertex_positions: Vec<Vec3> = Vec::with_capacity(3);
                let mut all_vertices_found = true;
                
                for vertex_id in &triangle_def.verts {
                    // Find the vertex with the given ID
                    let found_vertex = vertex_map.get(vertex_id);
                    
                    if let Some((position, _bone_name)) = found_vertex {
                        vertex_positions.push(*position);
                    } else {
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
                } else {
                }
            }
        }
        
        // Now also collect triangles from skin vertices (legacy triangle references)
        
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
                            // Invalid position index
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
                // Legacy face doesn't have exactly 3 vertices, skip
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

        triangles
    }



// Triangle struct moved to before impl Model block

    fn extract_and_resolve_variables(toml_data: &mut toml::Value) -> Result<HashMap<String, f64>, Box<dyn Error>> {
        let mut variables = HashMap::new();
        let mut pending_variables = HashMap::new();
        
        // Get the [body] section
        if let Some(body_table) = toml_data.get_mut("body").and_then(|b| b.as_table_mut()) {
            // First pass: collect direct numeric values and template strings
            for (key, value) in body_table.iter() {
                match value {
                    toml::Value::Float(f) => {
                        variables.insert(key.clone(), *f);
                    }
                    toml::Value::Integer(i) => {
                        variables.insert(key.clone(), *i as f64);
                    }
                    toml::Value::String(s) if s.starts_with("{{") && s.ends_with("}}") => {
                        pending_variables.insert(key.clone(), s.clone());
                    }
                    _ => {} // Skip other types
                }
            }
            
            // Multi-pass resolution of template variables
            let max_passes = 10; // Prevent infinite loops
            for _pass in 0..max_passes {
                let mut resolved_this_pass = false;
                let mut still_pending = HashMap::new();
                
                for (var_name, template_str) in pending_variables {
                    let template_content = &template_str[2..template_str.len()-2].trim();
                    
                    if let Ok(result) = Self::evaluate_expression(template_content, &variables) {
                        variables.insert(var_name.clone(), result);
                        
                        // Update the TOML data with the resolved value
                        body_table.insert(var_name, toml::Value::Float(result));
                        resolved_this_pass = true;
                    } else {
                        still_pending.insert(var_name, template_str);
                    }
                }
                
                pending_variables = still_pending;
                
                // If no variables were resolved this pass, we're done
                if !resolved_this_pass || pending_variables.is_empty() {
                    break;
                }
            }
            
            // Report any unresolved variables
            if !pending_variables.is_empty() {
            }
        }
        
        Ok(variables)
    }

    pub fn extract_and_resolve_variables_as_strings(toml_data: &mut toml::Value) -> Result<HashMap<String, String>, Box<dyn Error>> {
        let variables = Self::extract_and_resolve_variables(toml_data)?;
        let string_variables = variables.into_iter()
            .map(|(k, v)| (k, v.to_string()))
            .collect();
        Ok(string_variables)
    }

    fn process_templates(value: &mut toml::Value, variables: &HashMap<String, f64>) -> Result<(), Box<dyn Error>> {
        // First pass: collect all objects from the entire TOML structure
        let mut verts_map = HashMap::new();
        let mut triangles_map = HashMap::new();
        Self::collect_all_objects(value, &mut verts_map, &mut triangles_map);
        
        // Second pass: process templates with collected objects
        Self::process_templates_with_context(value, variables, &mut verts_map, &mut triangles_map)
    }
    
    fn process_templates_with_context(
        value: &mut toml::Value, 
        variables: &HashMap<String, f64>,
        verts_map: &mut HashMap<String, toml::Value>,
        triangles_map: &mut HashMap<String, toml::Value>
    ) -> Result<(), Box<dyn Error>> {
        use toml::Value;
        
        match value {
            Value::Table(table) => {
                // Collect vertices and triangles first
                Self::collect_objects_from_table(table, verts_map, triangles_map);
                
                // Process all values
                for (_, v) in table.iter_mut() {
                    Self::process_templates_with_context(v, variables, verts_map, triangles_map)?;
                }
            }
            Value::Array(array) => {
                for v in array.iter_mut() {
                    Self::process_templates_with_context(v, variables, verts_map, triangles_map)?;
                }
            }
            Value::String(s) => {
                // Check if this is a template string
                if s.starts_with("{{") && s.ends_with("}}") {
                    let template_content = &s[2..s.len()-2].trim();
                    
                    // Check for mirroring syntax first
                    if let Some(result) = Self::process_mirror_expression(template_content, verts_map, triangles_map)? {
                        *value = result;
                    } else if let Ok(result) = Self::evaluate_expression(template_content, variables) {
                        // Fallback to numeric expression evaluation
                        *value = Value::Float(result);
                    }
                }
            }
            _ => {} // Numbers, booleans, etc. don't need processing
        }
        Ok(())
    }
    
    fn evaluate_expression(expr: &str, variables: &HashMap<String, f64>) -> Result<f64, Box<dyn Error>> {
        let expr = expr.trim();
        
        // Simple variable lookup
        if let Some(&value) = variables.get(expr) {
            return Ok(value);
        }
        
        // Simple addition expression: "waist + 0.05"
        if let Some(plus_pos) = expr.find(" + ") {
            let left = expr[..plus_pos].trim();
            let right = expr[plus_pos + 3..].trim();
            
            let left_val = if let Some(&val) = variables.get(left) {
                val
            } else {
                left.parse::<f64>()?
            };
            
            let right_val = right.parse::<f64>()?;
            return Ok(left_val + right_val);
        }
        
        // Try to parse as direct number
        expr.parse::<f64>().map_err(|e| e.into())
    }
    
    fn collect_all_objects(
        value: &toml::Value,
        verts_map: &mut HashMap<String, toml::Value>,
        triangles_map: &mut HashMap<String, toml::Value>
    ) {
        use toml::Value;
        
        match value {
            Value::Table(table) => {
                Self::collect_objects_from_table(table, verts_map, triangles_map);
            }
            Value::Array(array) => {
                for v in array {
                    Self::collect_all_objects(v, verts_map, triangles_map);
                }
            }
            _ => {}
        }
    }
    
    fn collect_objects_from_table(
        table: &toml::value::Table,
        verts_map: &mut HashMap<String, toml::Value>,
        triangles_map: &mut HashMap<String, toml::Value>
    ) {
        // Look for 'verts' sections
        if let Some(verts_table) = table.get("verts").and_then(|v| v.as_table()) {
            for (name, value) in verts_table {
                verts_map.insert(name.clone(), value.clone());
            }
        }
        
        // Look for 'triangles' sections
        if let Some(triangles_table) = table.get("triangles").and_then(|v| v.as_table()) {
            for (name, value) in triangles_table {
                triangles_map.insert(name.clone(), value.clone());
            }
        }
        
        // Recursively search in nested tables
        for (_, value) in table {
            if let Some(nested_table) = value.as_table() {
                Self::collect_objects_from_table(nested_table, verts_map, triangles_map);
            }
        }
    }
    
    fn process_mirror_expression(
        expr: &str,
        verts_map: &HashMap<String, toml::Value>,
        triangles_map: &HashMap<String, toml::Value>
    ) -> Result<Option<toml::Value>, Box<dyn Error>> {
        // Check if expression contains mirroring syntax: "object_name, axis=true"
        if let Some(comma_pos) = expr.find(',') {
            let object_name = expr[..comma_pos].trim();
            let mirror_spec = expr[comma_pos + 1..].trim();
            
            // Parse mirror specification (e.g., "y=true", "x=true")
            if let Some(eq_pos) = mirror_spec.find('=') {
                let axis = mirror_spec[..eq_pos].trim();
                let enabled = mirror_spec[eq_pos + 1..].trim();
                
                if enabled == "true" {
                    // Try to mirror a vertex
                    if let Some(vert_value) = verts_map.get(object_name) {
                        return Ok(Some(Self::mirror_vertex(vert_value, axis)?));
                    }
                    
                    // Try to mirror a triangle
                    if let Some(triangle_value) = triangles_map.get(object_name) {
                        return Ok(Some(Self::mirror_triangle(triangle_value, axis)?));
                    }
                }
            }
        }
        
        Ok(None)
    }
    
    fn mirror_vertex(vert_value: &toml::Value, axis: &str) -> Result<toml::Value, Box<dyn Error>> {
        let mut mirrored = vert_value.clone();
        
        if let Some(table) = mirrored.as_table_mut() {
            match axis {
                "y" => {
                    // Mirror rotation for y-axis (flip horizontally)
                    if let Some(rot_value) = table.get_mut("rot") {
                        if let Some(rot) = rot_value.as_float() {
                            // Mirror rotation: 180 - original_rotation
                            let mirrored_rot = 180.0 - rot;
                            *rot_value = toml::Value::Float(mirrored_rot);
                        } else if let Some(rot) = rot_value.as_integer() {
                            let mirrored_rot = 180 - rot;
                            *rot_value = toml::Value::Integer(mirrored_rot);
                        }
                    }
                }
                "x" => {
                    // Mirror rotation for x-axis (flip vertically)
                    if let Some(rot_value) = table.get_mut("rot") {
                        if let Some(rot) = rot_value.as_float() {
                            let mirrored_rot = -rot;
                            *rot_value = toml::Value::Float(mirrored_rot);
                        } else if let Some(rot) = rot_value.as_integer() {
                            let mirrored_rot = -rot;
                            *rot_value = toml::Value::Integer(mirrored_rot);
                        }
                    }
                }
                _ => return Err(format!("Unsupported mirror axis: {}", axis).into()),
            }
        }
        
        Ok(mirrored)
    }
    
    fn mirror_triangle(triangle_value: &toml::Value, _axis: &str) -> Result<toml::Value, Box<dyn Error>> {
        let mut mirrored = triangle_value.clone();
        
        if let Some(table) = mirrored.as_table_mut() {
            // For triangles, we need to reverse the winding order to maintain correct normals
            if let Some(verts_value) = table.get_mut("verts") {
                if let Some(verts_array) = verts_value.as_array_mut() {
                    // Reverse the vertex order for proper winding after mirroring
                    verts_array.reverse();
                    
                    // Append mirror suffix to vertex names
                    for vert in verts_array.iter_mut() {
                        if let Some(vert_name) = vert.as_str() {
                            let mirrored_name = format!("{}_mirror", vert_name);
                            *vert = toml::Value::String(mirrored_name);
                        }
                    }
                }
            }
        }
        
        Ok(mirrored)
    }

    /// Find camera settings in the model and calculate camera transform
    pub fn find_camera_settings(&self, variables: &HashMap<String, String>) -> Option<(Vec3, Vec3)> {
        println!("Looking for camera settings in model...");
        // Iterate through all bones to find camera settings
        for (group_name, group) in &self.0 {
            println!("Checking group: {}", group_name);
            if let Some((camera_pos, look_target)) = self.find_camera_in_group(group_name, group, variables) {
                return Some((camera_pos, look_target));
            }
        }
        println!("No camera settings found");
        None
    }

    fn find_camera_in_group(&self, group_path: &str, group: &Group, variables: &HashMap<String, String>) -> Option<(Vec3, Vec3)> {
        // Check bones in this group
        for (bone_name, bone) in &group.bones {
            println!("Checking bone: {}.{}, has settings: {}", group_path, bone_name, bone.settings.is_some());
            if let Some(settings) = &bone.settings {
                println!("Found settings for bone {}.{}, has cam: {}", group_path, bone_name, settings.cam.is_some());
                if let Some(cam) = &settings.cam {
                    let full_bone_path = format!("{}.{}", group_path, bone_name);
                    
                    // Get bone positions
                    let all_bones = self.get_bones();
                    if let Some((bone_start, bone_end)) = self.get_bone_positions(&full_bone_path) {
                        // Resolve distance template
                        if let Ok(distance) = self.resolve_distance_template(&cam.dist, variables) {
                            let bone_rotation = all_bones.get(&full_bone_path)
                                .map(|b| b.resolved_rotation)
                                .unwrap_or(0.0);
                            
                            let (camera_pos, look_target) = cam.calculate_camera_transform(
                                bone_start, bone_end, bone_rotation, distance
                            );
                            return Some((camera_pos, look_target));
                        }
                    }
                }
            }
        }
        
        // Check subgroups
        for (subgroup_name, subgroup) in &group.subgroups {
            let subgroup_path = format!("{}.{}", group_path, subgroup_name);
            if let Some(result) = self.find_camera_in_group(&subgroup_path, subgroup, variables) {
                return Some(result);
            }
        }
        
        None
    }

    fn get_bone_positions(&self, bone_path: &str) -> Option<(Vec3, Vec3)> {
        // Process all bones to get their positions (similar to process_bones_in_order)
        let mut transforms = HashMap::new();
        let mut positions = HashMap::new();
        
        self.process_bones_for_positions(&mut transforms, &mut positions);
        
        // Return the specific bone's position
        positions.get(bone_path).copied()
    }

    fn process_bones_for_positions(&self, transforms: &mut HashMap<String, (Mat4, Vec3)>, positions: &mut HashMap<String, (Vec3, Vec3)>) {
        use std::collections::BTreeMap;
        
        // First find all bones and their depths
        let mut bones_by_depth: BTreeMap<usize, Vec<String>> = BTreeMap::new();
        
        fn collect_bones(
            group: &Group,
            path: &str,
            bones_by_depth: &mut BTreeMap<usize, Vec<String>>,
        ) {
            let depth = path.matches('.').count();
            
            // Add this group if it has bones
            if !group.bones.is_empty() {
                bones_by_depth
                    .entry(depth)
                    .or_default()
                    .push(path.to_string());
            }
            
            // Recursively process subgroups
            for (name, subgroup) in &group.subgroups {
                let subpath = if path.is_empty() {
                    name.clone()
                } else {
                    format!("{}.{}", path, name)
                };
                collect_bones(subgroup, &subpath, bones_by_depth);
            }
        }

        // Collect all bones by their depth in the hierarchy
        if let Some(body) = self.0.get("body") {
            collect_bones(body, "body", &mut bones_by_depth);
        }

        // Process bones level by level, starting from the root (lowest depth)
        for (_depth, paths) in bones_by_depth {
            for path in paths {
                self.process_group_bones(&path, transforms, positions);
            }
        }
    }

    fn process_group_bones(&self, group_path: &str, _transforms: &mut HashMap<String, (Mat4, Vec3)>, positions: &mut HashMap<String, (Vec3, Vec3)>) -> Option<()> {
        // Navigate to the group
        let parts: Vec<&str> = group_path.split('.').collect();
        let mut current_group = self.0.get(parts[0])?;
        
        for &part in parts.iter().skip(1) {
            current_group = current_group.subgroups.get(part)?;
        }

        // Process each bone in this group
        for (bone_name, bone) in &current_group.bones {
            let full_bone_path = format!("{}.{}", group_path, bone_name);
            
            // Find parent path
            let parent_path = if let Some(last_dot) = group_path.rfind('.') {
                &group_path[..last_dot]
            } else {
                ""
            };

            // Calculate bone position
            let bone_start = if parent_path.is_empty() {
                Vec3::ZERO // Root bone starts at origin
            } else {
                // Get parent's end position
                positions.get(parent_path)
                    .map(|(_, end)| *end)
                    .unwrap_or(Vec3::ZERO)
            };

            // Calculate bone direction and end position
            let orientation_rad = bone.resolved_orientation.to_radians();
            let slope_rad = bone.resolved_slope.to_radians();
            
            let direction = Vec3::new(
                orientation_rad.cos() * slope_rad.cos(),
                slope_rad.sin(),
                orientation_rad.sin() * slope_rad.cos(),
            );
            
            let bone_end = bone_start + direction * bone.length;
            
            // Store positions
            positions.insert(full_bone_path, (bone_start, bone_end));
        }
        
        Some(())
    }

    fn resolve_distance_template(&self, dist: &CameraDistance, variables: &HashMap<String, String>) -> Result<f32, Box<dyn Error>> {
        match dist {
            CameraDistance::Resolved(value) => Ok(*value),
            CameraDistance::Template(template) => {
                // If it's a simple number, parse it directly
                if let Ok(value) = template.parse::<f32>() {
                    return Ok(value);
                }
                
                // If it contains template syntax, resolve it
                if template.contains("{{") && template.contains("}}") {
                    // Extract the variable name from {{variable_name}}
                    let start = template.find("{{").ok_or("Invalid template syntax")? + 2;
                    let end = template.find("}}").ok_or("Invalid template syntax")?;
                    let var_name = &template[start..end].trim();
                    
                    if let Some(value_str) = variables.get(*var_name) {
                        return value_str.parse::<f32>().map_err(|e| e.into());
                    } else {
                        return Err(format!("Variable '{}' not found", var_name).into());
                    }
                }
                
                Err("Invalid distance template".into())
            }
        }
    }


// End of impl Model
}
