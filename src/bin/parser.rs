use std::error::Error;
use std::fs;

fn main() -> Result<(), Box<dyn Error>> {
    // Read the file content
    let file_content = fs::read_to_string("mesh.toml")?;
    
    // Parse the TOML content
    let value: toml::Value = toml::from_str(&file_content)?;
    
    // Print the parsed content
    //println!("Parsed TOML: {:#?}", value);

    Ok(())
}