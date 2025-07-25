use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct TestVert {
    frac: f32,
    dist: f32,
    rot: f32,
    #[serde(default)]
    ruler: bool,
}

fn main() {
    let toml_str = r#"
        test_vertex = { frac = 0.6, dist = 0.1, rot = 45, ruler = true }
    "#;
    
    let parsed: std::collections::HashMap<String, TestVert> = toml::from_str(toml_str).unwrap();
    
    for (name, vert) in parsed {
        println!("Vertex '{}': frac={}, dist={}, rot={}, ruler={}", 
                 name, vert.frac, vert.dist, vert.rot, vert.ruler);
    }
}