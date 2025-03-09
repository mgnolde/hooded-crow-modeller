use bevy::prelude::*;

fn main() {
    println!("Testing right angle calculation");
    
    // Test both functions that could cause issues
    test_calculate_direction();
    test_calculate_transform();
}

fn test_calculate_direction() {
    println!("\n=== Testing calculate_direction function ===");
    
    // Test vertical angles (±90°)
    print_direction(0.0, 90.0);  // Should be (0, 1, 0)
    print_direction(45.0, 90.0); // Should still be (0, 1, 0) regardless of orientation
    print_direction(0.0, -90.0); // Should be (0, -1, 0)
    
    // Test near-vertical angles
    print_direction(0.0, 89.0);  // Almost vertical
    print_direction(0.0, -89.0); // Almost negative vertical
    
    // Test horizontal angles for comparison
    print_direction(0.0, 0.0);   // Horizontal, facing +Z
    print_direction(90.0, 0.0);  // Horizontal, facing +X
}

fn print_direction(orientation: f32, slope: f32) {
    let direction = calculate_direction(orientation, slope);
    println!("Orientation: {}°, Slope: {}° -> Direction: {:?}", 
             orientation, slope, direction);
}

fn test_calculate_transform() {
    println!("\n=== Testing calculate_transform function ===");
    
    // Test vertical angles (±90°)
    print_transform(0.0, 90.0, 0.0);  // Should point up
    print_transform(45.0, 90.0, 0.0); // Should still point up
    print_transform(0.0, -90.0, 0.0); // Should point down
    
    // Test near-vertical angles
    print_transform(0.0, 89.0, 0.0);  // Almost vertical
    print_transform(0.0, -89.0, 0.0); // Almost negative vertical
    
    // Test horizontal angles for comparison
    print_transform(0.0, 0.0, 0.0);   // Horizontal, facing +Z
    print_transform(90.0, 0.0, 0.0);  // Horizontal, facing +X
}

fn print_transform(orientation: f32, slope: f32, rotation: f32) {
    let transform = calculate_transform(orientation, slope, rotation);
    let forward = transform.transform_vector3(Vec3::Z).normalize();
    println!("Orientation: {}°, Slope: {}°, Rotation: {}° -> Forward: {:?}", 
             orientation, slope, rotation, forward);
}

// Copy of the calculate_direction function to test
fn calculate_direction(orientation: f32, slope: f32) -> Vec3 {
    // For vertical slopes (±90°), use special handling
    if (slope - 90.0).abs() < 0.001 {
        return Vec3::new(0.0, 1.0, 0.0); // Exactly up
    } 
    if (slope + 90.0).abs() < 0.001 {
        return Vec3::new(0.0, -1.0, 0.0); // Exactly down
    }
    
    // For non-vertical slopes, use spherical coordinates
    let orientation_rad = orientation.to_radians();
    let slope_rad = slope.to_radians();
    
    let x = slope_rad.cos() * orientation_rad.sin();
    let y = slope_rad.sin();
    let z = slope_rad.cos() * orientation_rad.cos();
    
    Vec3::new(x, y, z).normalize()
}

// Copy of the calculate_transform function to test
fn calculate_transform(orientation: f32, slope: f32, rotation: f32) -> Mat4 {
    // Convert angles to radians
    let orientation_rad = orientation.to_radians();
    let slope_rad = slope.to_radians();
    let rotation_rad = rotation.to_radians();

    // Special case for exactly vertical slopes (±90°)
    if (slope - 90.0).abs() < 0.001 {
        // For exactly 90°, create a transform pointing straight up
        let up_vector = Vec3::Y; // (0, 1, 0)
        let matrix = Mat4::from_rotation_y(orientation_rad) * Mat4::from_rotation_z(rotation_rad);
        return matrix * Mat4::from_translation(up_vector);
    } 
    else if (slope + 90.0).abs() < 0.001 {
        // For exactly -90°, create a transform pointing straight down
        let down_vector = -Vec3::Y; // (0, -1, 0)
        let matrix = Mat4::from_rotation_y(orientation_rad) * Mat4::from_rotation_z(rotation_rad);
        return matrix * Mat4::from_translation(down_vector);
    }

    // For non-vertical cases, use the standard spherical coordinate approach
    let slope_rot = Mat4::from_rotation_x(slope_rad);
    let orientation_rot = Mat4::from_rotation_y(orientation_rad);
    let rotation_rot = Mat4::from_rotation_z(rotation_rad);
    
    orientation_rot * slope_rot * rotation_rot * Mat4::from_translation(Vec3::Z)
}
