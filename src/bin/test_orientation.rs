use hooded_crow_modeller::{Model, Vec3};

fn main() {
    println!("Running orientation test...");
    
    // Create a minimal model for testing the bone orientation logic
    let toml_content = r#"
    [bones.body]
    length = 0.5
    orientation = 0
    
    [bones.body.lower_spine]
    length = 0.5
    orientation = 0
    slope = 90
    
    [bones.body.horizontal]
    length = 0.5
    orientation = 0
    slope = 0
    
    [bones.body.diagonal]
    length = 0.5
    orientation = 0
    slope = 45
    
    [bones.body.down]
    length = 0.5
    orientation = 0
    slope = -90
    "#;
    
    // Load the test model
    let model = Model::from_toml_str(toml_content).expect("Failed to load test model");
    println!("\n=== Test Model Loaded ===");
    
    // Resolve and calculate bone positions
    let bone_positions = model.calculate_bone_positions();
    
    // Print the results for each bone
    println!("\n=== Bone Positions ===");
    for (name, start, end, _) in bone_positions {
        println!("Bone: {}", name);
        println!("  Start: {:?}", start);
        println!("  End: {:?}", end);
        println!("  Direction: {:?}", (end - start).normalize());
        println!();
    }
    
    // Specifically test the lower_spine which should point straight up (0, 1, 0)
    if let Some((_, start, end, _)) = bone_positions.iter().find(|(name, _, _, _)| name == "body.lower_spine") {
        let direction = (*end - *start).normalize();
        println!("LOWER SPINE TEST:");
        println!("  Direction: {:?}", direction);
        println!("  Expected: Vec3(0, 1, 0)");
        
        // Check if it's pointing almost exactly up
        let up_dot = direction.dot(Vec3::new(0.0, 1.0, 0.0));
        println!("  Alignment with UP vector: {:.6}", up_dot);
        if (up_dot - 1.0).abs() < 0.001 {
            println!("  ✓ SUCCESS: lower_spine is correctly pointing UP");
        } else {
            println!("  ❌ FAILED: lower_spine is NOT correctly pointing UP");
        }
    }
}
