# Generate a beveled 3D polygon

This function generates a beveled 3D polygon from a straight skeleton.

## Usage

``` r
generate_beveled_polygon(
  skeleton,
  bevel_offsets = generate_bevel(),
  bevel_heights = NULL,
  set_max_height = FALSE,
  max_height = NA,
  vertical_offset = 0,
  base = TRUE,
  base_height = 0,
  raw_offsets = FALSE,
  raw_heights = FALSE,
  swap_yz = TRUE,
  progress = TRUE,
  double_sided = FALSE,
  sides = FALSE,
  return_skeleton_polygons = FALSE,
  scale_all_max = FALSE,
  material = material_list(),
  bevel_material = NA,
  verbose = FALSE
)
```

## Arguments

- skeleton:

  Default \`NULL\`. A straight skeleton generated from the
  \`skeletonize\` function.

- bevel_offsets:

  Default \`NULL\`. The offset(s) of the bevel.

- bevel_heights:

  Default is set to \`bevel_offsets\`. Numeric vector specifying the
  heights of the bevels. Must be of the same length as
  \`bevel_offsets\`.

- set_max_height:

  Default \`FALSE\`. A logical flag that controls whether to set the max
  height of the roof based on the \`max_height\` argument.

- max_height:

  Default \`1\`. The maximum height of the polygon.

- vertical_offset:

  Default \`0\`. The vertical offset of the polygon.

- base:

  Default \`TRUE\`. A logical flag that controls whether to generate the
  bottom of the polygon.

- base_height:

  Default \`NA\`. Height of the base, defaulting to
  \`min(bevel_heights) + vertical_offset\` .

- raw_offsets:

  Default \`FALSE\`. A logical flag indicating whether the
  \`bevel_offsets\` are already in raw format and do not need to be
  multiplied by the maximum time of the skeleton.

- raw_heights:

  Default \`FALSE\`. A logical flag indicating whether the
  \`bevel_heights\` are already in raw format and do not need to be
  multiplied by the maximum time of the skeleton.

- swap_yz:

  Default \`TRUE\`. A logical flag that controls whether to swap the y
  and z coordinates in the resulting mesh. If \`TRUE\`, the y and z
  coordinates will be swapped.

- progress:

  Default \`TRUE\`. A logical flag to control whether a progress bar is
  displayed during roof generation.

- double_sided:

  Default \`FALSE\`. A logical flag that controls whether the polygon
  should be double-sided.

- sides:

  Default \`FALSE\`. A logical flag on whether to draw the sides. This
  will automatically be set to \`TRUE\` if \`base = TRUE\` and the
  \`base_height\` is less than \`vertical_offset\`.

- return_skeleton_polygons:

  Default \`FALSE\`. A logical flag that controls whether to return the
  skeleton polygons along with the 3D mesh.

- scale_all_max:

  Default \`FALSE\`. If passing in a list of multiple skeletons with
  polygons, whether to scale each polygon to the overall max height, or
  whether to scale each max height to the maximum internal distance in
  the polygon.

- material:

  Default \`material_list()\`. Interface to set the
  color/appearance/material options for the resulting \`ray_mesh\` mesh.

- bevel_material:

  Default \`NA\`, uses the material specified in \`material\`. Interface
  to set the color/appearance/material options for the resulting
  \`ray_mesh\` bevel mesh.

- verbose:

  Default \`FALSE\`. A logical flag to control whether additional timing
  information should be displayed.

## Value

A 3D mesh of the beveled polygon model.

## Examples

``` r
#Generate vertices and holes
vertices = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)-3.5
hole_1 = matrix(c(1,1, 2,1, 2,2, 1,2, 1,1), ncol = 2, byrow = TRUE)[5:1,]-3.5
hole_2 = matrix(c(5,5, 6,5, 6,6, 5,6, 5,5), ncol = 2, byrow = TRUE)[5:1,]-3.5
skeleton = skeletonize(vertices, holes = list(hole_1, hole_2))
if(run_docs_raybevel()) {
plot_skeleton(skeleton)
}


#Generate a roof model and specify the material
if(run_docs_raybevel()) {
  library(rayrender)
  library(rayvertex)

  scene_base = xz_rect(xwidth=100,zwidth=100,
                       material=diffuse(color="grey20", checkercolor="white")) |>
    add_object(sphere(y=8,z=10,x=-3,material=light(intensity=100))) |>
    add_object(sphere(y=800,z=10,x=-3,radius=100,material=light(intensity=5))) |>
    add_object(sphere(x=-10,z=-10,y=5,material=light(color="red", intensity=40))) |>
    add_object(sphere(x=10,z=-10,y=5,material=light(color="orange", intensity=40)))

  bevel = generate_bevel("angled", bevel_start = 0, bevel_end = 0.2, max_height=0.25)
  roof_model = generate_beveled_polygon(skeleton,
                                        bevel_offsets = bevel,
                                        material = material_list(diffuse="purple"))
  #Visualize with rayvertex
  roof_model |>
    add_shape(xz_rect_mesh(scale=c(20,1,20)) ) |>
    rasterize_scene(lookfrom=c(10,10,10),fov=40,
                    light_info = directional_light(c(-0.5,0.7,0.8)))

  #Visualize with rayrender
  raymesh_model(roof_model, override_material = FALSE) |>
    add_object(scene_base) |>
    render_scene(lookfrom=c(10,30,20),samples=16,
                 width=800,height=800,fov=0,ortho_dimensions=c(10,10), verbose=TRUE)
}
#> Setting `lookat` to: c(0.00, 0.31, 0.00)

#> Pre-processing scene       : 0.0 secs (Total: 0.0 secs)
#> Pre-processed scene        : 0.0 secs (Total: 0.0 secs)
#> Generated Camera           : 0.0 secs (Total: 0.0 secs)
#> Built Scene BVH            : 0.0 secs (Total: 0.0 secs)
#> Loaded background          : 0.0 secs (Total: 0.0 secs)
#> Allocating sampler         : 0.0 secs (Total: 0.0 secs)
#> Finished rendering         : 13.8 secs (Total: 13.9 secs)

#> Post-processed image       : 0.8 secs (Total: 14.7 secs)

# Change the bevel to be circular
if(run_docs_raybevel()) {
  bevel = generate_bevel("circular", bevel_start = 0, bevel_end = 0.2, max_height=0.25)
  roof_model = generate_beveled_polygon(skeleton,
                                        bevel_offsets = bevel,
                                        material = material_list(diffuse="purple"))

  raymesh_model(roof_model, override_material = FALSE) |>
    add_object(scene_base) |>
    render_scene(lookfrom=c(10,30,20), samples=16,
                 width=800,height=800,fov=0,ortho_dimensions=c(10,10))
}


# Change the bevel to type "bump", change the max height, and raise it off the surface
if(run_docs_raybevel()) {
  bevel = generate_bevel("bump", bevel_start = 0, bevel_end = 0.4, max_height=0.25)
  roof_model = generate_beveled_polygon(skeleton, base_height=1,
                                        bevel_offsets = bevel,
                                        material = material_list(diffuse="purple"))

  raymesh_model(roof_model, override_material = FALSE) |>
    add_object(scene_base) |>
    render_scene(lookfrom=c(10,30,20), samples=16,
                 width=800,height=800,fov=0,ortho_dimensions=c(10,10))
}


# Generate a complex bevel and use the exact specified heights
if(run_docs_raybevel()) {
  bevel = generate_complex_bevel(c("bump", "exp", "circular","step"),
                                 bevel_start = c(0,0.3,0.7,0.95),
                                 bevel_end = c(0.1,0.6,0.95,1),
                                 reverse = c(F,F,T,F),
                                 segment_height = c(0.25,0.5,0.5,4),
                                 plot_bevel = TRUE)

  roof_model = generate_beveled_polygon(skeleton, vertical_offset=0.1,
                                        bevel_offsets = bevel,
                                        raw_heights = TRUE,
                                        material = material_list(diffuse="purple"))

  raymesh_model(roof_model, override_material = FALSE) |>
    add_object(scene_base) |>
    render_scene(lookfrom=c(10,30,20), samples=16,
                 width=800,height=800,fov=0,ortho_dimensions=c(10,10))
}



# Turn the polygon into a ziggurat, using the step bevel type
if(run_docs_raybevel()) {
  offs = seq(0, 1, by = 0.05)
  bevel = generate_complex_bevel("step",
                                 bevel_start = offs[-length(offs)],
                                 bevel_end = offs[-1],
                                 segment_height = 0.2)

  roof_model = generate_beveled_polygon(skeleton, vertical_offset=0.2,
                                        bevel_offsets = bevel,
                                        raw_heights = TRUE,
                                        material = material_list(diffuse = "purple"))

  raymesh_model(roof_model, override_material = FALSE) |>
    add_object(scene_base) |>
    render_scene(lookfrom = c(10,30,20), samples=16,
                 width = 800, height = 800, fov = 0, ortho_dimensions = c(10,10))
}


# Turn the polygon into a smooth wavy slide, taking advantage of vector recycling to flip/reverse
if(run_docs_raybevel()) {
  offs = seq(0, 1, by = 0.1)
  bevel = generate_complex_bevel("exp",
                                 bevel_start = offs[-length(offs)],
                                 bevel_end = offs[-1],
                                 reverse = c(TRUE, FALSE),
                                 flip = c(TRUE, FALSE),
                                 segment_height = 0.25)

  roof_model = generate_beveled_polygon(skeleton, vertical_offset=0.2,
                                        bevel_offsets = bevel,
                                        raw_heights = TRUE,
                                        material = material_list(diffuse = "purple"))

  raymesh_model(roof_model, override_material = FALSE) |>
    add_object(scene_base) |>
    render_scene(lookfrom = c(10,30,20), samples=16,
                 width = 800, height = 800, fov = 0, ortho_dimensions = c(10,10))
}


# Skeletonize and turn an {sf} object into a beveled polygon
if(run_docs_raybevel()) {
  us_states = spData::us_states
  texas = us_states[us_states$NAME == "Texas",]
  texas_skeleton = skeletonize(texas)
  plot_skeleton(texas_skeleton)

  bevel = generate_bevel("angled" , bevel_end=0.3, max_height = 0.3)
  roof_model_texas = generate_beveled_polygon(texas_skeleton,
                                        bevel_offsets = bevel,
                                        material = material_list(diffuse = "purple")) |>
    center_mesh() |>
    translate_mesh(c(0,0.3,0))

  raymesh_model(roof_model_texas, material = diffuse(color="purple")) |>
    add_object(scene_base) |>
    add_object(sphere(x=-10,z=-10,y=5,material=light(color="red", intensity=40))) |>
    add_object(sphere(x=10,z=-10,y=5,material=light(color="orange", intensity=40))) |>
    render_scene(lookfrom=c(0,10,0),camera_up=c(0,0,1), samples=16,
                 width=800,height=800,fov=0, ortho_dimensions=c(15,15))
}


# Generate a smooth bevel
if(run_docs_raybevel()) {
  bevel = generate_bevel("exp", bevel_start = 0, bevel_end=0.5, max_height=2)
  roof_model_texas = generate_beveled_polygon(texas_skeleton,
                                        bevel_offsets = bevel,
                                        material = material_list(diffuse = "purple")) |>
    center_mesh() |>
    translate_mesh(c(0,0.5,0))

  raymesh_model(roof_model_texas, material = diffuse(color="purple")) |>
    add_object(scene_base) |>
    add_object(sphere(x=-10,z=-10,y=5,material=light(color="red", intensity=40))) |>
    add_object(sphere(x=10,z=-10,y=5,material=light(color="orange", intensity=40))) |>
    render_scene(lookfrom=c(0,10,0),camera_up=c(0,0,1), samples=16,
                 width=800,height=800,fov=0, ortho_dimensions=c(15,15))
}
```
