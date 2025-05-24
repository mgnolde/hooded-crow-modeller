# Hooded Crow Modeller

A command line 3D modeller, with a synchronized GUI component to visualize changes on the fly.

The modelling is done by defining a skeleton (consisting of bones and the joints between them). Basically, it's a directed graph with edge attributes, encoded in a TOML file. Attributes are inherited from the parent bone. Bones have skin vertices, defining the actual mesh. No animation is supported yet, altough the implementation is designed with animation in mind. Everything is relative, no coordinates needed / supported.



![image info](./mat/modeller.png)

Syntax example

```toml
[body]

[body.lower_spine]
len = 0.20
orient = 0
slope = 90
rot = 0

[body.lower_spine.middle_spine]
len = 0.18

[body.lower_spine.middle_spine.upper_spine]
len = 0.15

[body.lower_spine.middle_spine.upper_spine.left_upper_arm]
len = 0.3
orient = 100
slope = 20
rot = 0
```

run via:
````Bash
cargo run --bin viewer example.toml
```