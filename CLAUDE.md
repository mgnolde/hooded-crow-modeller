# Hooded Crow Modeller - Project Memory

## Project Overview
- 3D model viewer and editor using Bevy 0.14
- TOML-based model definitions with bone structures
- Real-time visualization with wireframes and triangles

## Architecture
- `src/lib.rs`: Core model parsing and data structures
- `src/bin/viewer.rs`: Bevy-based 3D viewer application
- `example.toml`: Reference model configuration

## Model Configuration
- TOML files define bone hierarchies, vertices, and triangles
- Template variables (e.g., `{{waist}}`, `{{lower_waist}}`) for parametric modeling
- Support for mirroring vertices and triangles across axes

## Camera System
- Camera positioning via TOML: `cam = { frac = 0.5, dist = "{{waist}}", rot = 180 }`
- Can be defined inline with bones or in separate `[bone.settings]` sections
- Camera looks directly at the specified bone target point
- Supports both template variables and direct numeric values

## Rendering Preferences
- **Template colors**: Grayscale depth-based coloring (black=lowest z, white=highest z)
- **Bone line width**: 2x or 3x default thickness for better visibility
- **Wireframe mode**: Use depth-based coloring
- **Triangles**: Support colored fills with transparency

## Data Structures
- `CameraSettings`: Uses `CameraDistance` enum for flexible distance specification
- `Model`: Main container with groups, bones, and rendering data
- `SkinVert`: Vertex positioning with bone relationships

## Development Practices
- Always run `cargo check` before testing changes
- Use debug output for complex parsing and positioning logic
- Template variable resolution happens before camera parsing
- Prefer editing existing files over creating new ones

## Common Commands
- Build and run viewer: `cargo run --bin viewer example.toml`
- Check compilation: `cargo check`
- Export to GLB: `cargo run --bin viewer example.toml --export output.glb`