#' Generate a 3D roof model
#'
#' This function generates a 3D roof model from a straight skeleton.
#'
#' @param skeleton Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param angle Default `45`. Angle of the roof.
#' @param max_height Default `NA`. The maximum height of the roof.
#' @param vertical_offset Default `0`. The vertical offset of the roof.
#' @param base Default `TRUE`. A logical flag that controls whether to generate the bottom of the roof.
#' @param base_height Default `vertical_offset`. Height of the base.
#' @param sides Default `FALSE`. A logical flag on whether to draw the sides. This will automatically be set to `TRUE`
#' if `base = TRUE` and the `base_height` is less than `vertical_offset`.
#' @param double_sided Default `FALSE`. A logical flag that controls whether the polygon should be double-sided.
#' @param swap_yz Default `TRUE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh.
#' If `TRUE`, the y and z coordinates will be swapped.
#' @param progress Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#' @param verbose Default `FALSE`. A logical flag to control whether additional timing information should be displayed.
#' @param scale_all_max Default `FALSE`. If passing in a list of multiple skeletons with polygons, whether to scale each polygon to the overall
#' max height, or whether to scale each max height to the maximum internal distance in the polygon.
#' @param material Default `material_list()`. Interface to set the color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param roof_material Default `NA`, uses the material specified in `material`. Interface to set the color/appearance/material options for the resulting `ray_mesh` rooftop mesh.
#'
#' @return A 3D mesh of the roof model.
#'
#' @import rayvertex
#'
#' @export
#' @examples
#' #Generate vertices and holes
#' vertices = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)-3.5
#' hole_1 = matrix(c(1,1, 2,1, 2,2, 1,2, 1,1), ncol = 2, byrow = TRUE)[5:1,]-3.5
#' hole_2 = matrix(c(5,5, 6,5, 6,6, 5,6, 5,5), ncol = 2, byrow = TRUE)[5:1,]-3.5
#' skeleton = skeletonize(vertices, holes = list(hole_1, hole_2))
#' if(run_docs_raybevel()) {
#' plot_skeleton(skeleton)
#' }
#'
#' #Generate a roof model and specify the material
#' if(run_docs_raybevel()) {
#'   library(rayrender)
#'   library(rayvertex)
#'   roof_model = generate_roof(skeleton, material = material_list(diffuse="purple"))
#'   scene_base = xz_rect(xwidth=100,zwidth=100,
#'                        material=diffuse(color="grey20", checkercolor="white")) |>
#'     add_object(sphere(y=8,z=10,x=-3,material=light(intensity=100))) |>
#'     add_object(sphere(y=800,z=10,x=-3,radius=100,material=light(intensity=5)))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Change the maximum height of the roof
#' if(run_docs_raybevel()) {
#'   roof_model = generate_roof(skeleton, max_height=5)
#'   raymesh_model(roof_model, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' #Add a vertical_offset to the roof, without a base
#' if(run_docs_raybevel()) {
#'   roof_model = generate_roof(skeleton, vertical_offset = 2, base = FALSE)
#'   raymesh_model(roof_model, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,10,20), lookat=c(0,2,0), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Add a base
#' if(run_docs_raybevel()) {
#'   roof_model = generate_roof(skeleton, vertical_offset = 2, base = TRUE)
#'   raymesh_model(roof_model, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,10,20), lookat=c(0,2,0), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Change the base height (note that the vertical_offset is measured from the base, not from zero)
#' if(run_docs_raybevel()) {
#'   roof_model = generate_roof(skeleton, vertical_offset = 2, base = TRUE, base_height=1)
#'   raymesh_model(roof_model, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,10,20), lookat=c(0,2,0), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#'
#' # Skeletonize and turn an {sf} object into a roof
#' if(run_docs_raybevel()) {
#'   us_states = spData::us_states
#'   cali = us_states[us_states$NAME == "California",]
#'   cali_skeleton = skeletonize(cali)
#'   plot_skeleton(cali_skeleton)
#'   roof_model_cali = generate_roof(cali_skeleton, max_height = 2) |>
#'     center_mesh() |>
#'     translate_mesh(c(0,1,0))
#'
#'   raymesh_model(roof_model_cali, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     add_object(sphere(x=-10,z=-10,y=4,material=light(color="red", intensity=40))) |>
#'     add_object(sphere(x=10,z=-10,y=4,material=light(color="orange", intensity=40))) |>
#'     render_scene(lookfrom=c(0,10,-1), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0, ortho_dimensions=c(12,12))
#' }
generate_roof = function(skeleton, max_height = NA, vertical_offset = 0,
                         base = FALSE, base_height = 0, angle = 45,
                         sides = FALSE, double_sided = FALSE, scale_all_max = FALSE,
                         swap_yz = TRUE, progress = TRUE,
                         material = material_list(),
                         roof_material = NA,
                         verbose = FALSE) {
  if(run_docs_raybevel() || !interactive()) {
    progress = FALSE
  }
  if(!is.list(roof_material)) {
    roof_material = material
  }
  if(inherits(skeleton, "rayskeleton_list")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating roof [:bar] eta: :eta",
      total = length(skeleton), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    vec_default = function(length_skeleton, value) {
      if(length(value) == 1) {
        return(rep(value,length(skeleton)))
      } else {
        stopifnot(length(value) == length_skeleton)
        return(value)
      }
    }
    len_s = length(skeleton)
    vertical_offset = vec_default(len_s, vertical_offset)
    base_height = vec_default(len_s, base_height)
    angle = vec_default(len_s, angle)
    if(!is.na(max_height)) {
      if(length(max_height) == 1) {
        if(scale_all_max) {
          max_height = rep(max_height,length(skeleton))
        } else {
          #Calculate proportional maximum heights
          single_max_heights = unlist(lapply(skeleton, \(x) max(x$nodes$time)))
          max_height = single_max_heights/max(single_max_heights) * max_height
        }
      } else {
        stopifnot(length(max_height) == length(skeleton))
      }
    }
    for(j in seq_len(length(skeleton))) {
      if(progress) {
        pb$tick()
      }
      meshlist[[counter]] = generate_roof(skeleton[[j]],
                                          base = base,
                                          max_height = max_height[j],
                                          sides = sides,
                                          double_sided = double_sided,
                                          base_height = base_height[j],
                                          vertical_offset = vertical_offset[j],
                                          angle = angle[j],
                                          swap_yz = swap_yz,
                                          material = material,
                                          roof_material = roof_material,
                                          progress = FALSE)
      counter = counter + 1
    }
    return(scene_from_list(meshlist))
  }
  if(!inherits(skeleton, "rayskeleton")) {
    stop("`skeleton` must be of class `rayskeleton`")
  }
  angle_slope = tanpi(angle/180)
  if(is.na(max_height)) {
    max_height = max(skeleton$nodes$time) * angle_slope
  }

  polygon_ind = convert_ss_to_polygons(skeleton, progress = progress)
  nodes = skeleton$nodes
  index_list = list()
  for(i in seq_len(length(polygon_ind))) {
    tmp_ind = polygon_ind[[i]]
    tmp_poly = nodes[tmp_ind,2:3]
    index_list[[i]] = matrix(tmp_ind[decido::earcut(tmp_poly)],byrow=TRUE,ncol=3)
  }
  indices_all = do.call("rbind",index_list)
  xyz = nodes[,2:4]
  colnames(xyz) = c("x","y","z")
  if(!is.na(max_height)) {
    stopifnot(max_height >= 0)
    xyz[,3] = xyz[,3]/max(xyz[,3]) * max_height
  }
  original_verts = attr(skeleton,"original_vertices")
  original_holes = attr(skeleton,"original_holes")
  side_top = vertical_offset

  mesh = construct_mesh(vertices = as.matrix(xyz),
                                   indices = as.matrix(indices_all)-1) |>
    translate_mesh(c(0,0,vertical_offset)) |>
    set_material(roof_material)

  if(double_sided) {
    xyzflip = xyz
    xyzflip[,3] = -xyzflip[,3]
    indices_flip = indices_all[,3:1,drop=FALSE]
    double_mesh = construct_mesh(vertices = as.matrix(xyzflip),
                                 indices = as.matrix(indices_flip)-1) |>
      set_material(roof_material)
  }
  if(base && !double_sided) {
    if(length(original_holes) > 0) {
      holes = c(0,utils::head(cumsum(unlist(lapply(original_holes,nrow))),-1)) + nrow(original_verts) + 1
      hole_mat = do.call("rbind", original_holes)
      original_verts_holes = rbind(original_verts, hole_mat)
    } else {
      holes = 0
      original_verts_holes = original_verts
    }
    base_indices = matrix(decido::earcut(original_verts_holes, holes = holes), byrow = TRUE, ncol=3)
    base_indices = base_indices[,3:1,drop = FALSE]
    original_verts_base = cbind(original_verts_holes, rep(0, nrow(original_verts_holes)))
    base_mesh = construct_mesh(vertices = as.matrix(original_verts_base),
                               indices = as.matrix(base_indices)-1) |>
      set_material(material)

    mesh = mesh |>
      add_shape(base_mesh)
  }
  if(double_sided) {
    mesh = add_shape(mesh, double_mesh)
  }
  if((sides || base || double_sided) &&
     ((!double_sided && side_top > 0) || (double_sided && side_top > 0))) {
    if(!double_sided) {
      side_mesh = extrude_sides(original_verts, original_holes, bottom = 0, top = side_top)
    } else {
      side_mesh = extrude_sides(original_verts, original_holes, bottom = -side_top, top = side_top)
    }
    side_mesh = side_mesh |>
      set_material(material)
    mesh = add_shape(mesh, side_mesh)
  }
  if(!is.na(base_height)) {
    mesh = translate_mesh(mesh, c(0,0,base_height))
  }
  if(swap_yz) {
    mesh = swap_yz(mesh)
  }
  print_time(verbose, "Generated mesh")
  return(mesh)
}

#' Generate a beveled 3D polygon
#'
#' This function generates a beveled 3D polygon from a straight skeleton.
#'
#' @param skeleton Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param bevel_offsets Default `NULL`. The offset(s) of the bevel.
#' @param max_height Default `1`. The maximum height of the polygon.
#' @param bevel_heights Default is set to `bevel_offsets`. Numeric vector specifying the heights of the bevels. Must be of the same length as `bevel_offsets`.
#' @param set_max_height Default `FALSE`. A logical flag that controls whether to set the max height of the roof based on the `max_height` argument.
#' @param swap_yz Default `TRUE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh. If `TRUE`, the y and z coordinates will be swapped.
#' @param progress Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#' @param vertical_offset Default `0`. The vertical offset of the polygon.
#' @param base Default `TRUE`. A logical flag that controls whether to generate the bottom of the polygon.
#' @param base_height Default `NA`. Height of the base, defaulting to `min(bevel_heights) + vertical_offset` .
#' @param sides Default `FALSE`. A logical flag on whether to draw the sides. This will automatically be set to `TRUE`
#' if `base = TRUE` and the `base_height` is less than `vertical_offset`.
#' @param scale_all_max Default `FALSE`. If passing in a list of multiple skeletons with polygons, whether to scale each polygon to the overall
#' max height, or whether to scale each max height to the maximum internal distance in the polygon.
#' @param verbose Default `FALSE`. A logical flag to control whether additional timing information should be displayed.
#' @param raw_offsets Default `FALSE`. A logical flag indicating whether the `bevel_offsets` are already in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' @param raw_heights Default `FALSE`. A logical flag indicating whether the `bevel_heights` are already in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' @param double_sided Default `FALSE`. A logical flag that controls whether the polygon should be double-sided.
#' @param return_skeleton_polygons Default `FALSE`. A logical flag that controls whether to return the skeleton polygons along with the 3D mesh.
#' @param material Default `material_list()`. Interface to set the color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param bevel_material Default `NA`, uses the material specified in `material`. Interface to set the color/appearance/material options for the resulting `ray_mesh` bevel mesh.
#'
#' @return A 3D mesh of the beveled polygon model.
#'
#' @import rayvertex
#'
#' @export
#' @examples
#' #Generate vertices and holes
#' vertices = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)-3.5
#' hole_1 = matrix(c(1,1, 2,1, 2,2, 1,2, 1,1), ncol = 2, byrow = TRUE)[5:1,]-3.5
#' hole_2 = matrix(c(5,5, 6,5, 6,6, 5,6, 5,5), ncol = 2, byrow = TRUE)[5:1,]-3.5
#' skeleton = skeletonize(vertices, holes = list(hole_1, hole_2))
#' plot_skeleton(skeleton)
#'
#' #Generate a roof model and specify the material
#' if(run_docs_raybevel()) {
#'   library(rayrender)
#'   library(rayvertex)
#'   scene_base = xz_rect(xwidth=100,zwidth=100,
#'                        material=diffuse(color="grey20", checkercolor="white")) |>
#'     add_object(sphere(y=8,z=10,x=-3,material=light(intensity=100))) |>
#'     add_object(sphere(y=800,z=10,x=-3,radius=100,material=light(intensity=5))) |>
#'     add_object(sphere(x=-10,z=-10,y=5,material=light(color="red", intensity=40))) |>
#'     add_object(sphere(x=10,z=-10,y=5,material=light(color="orange", intensity=40)))
#'
#'   bevel = generate_bevel("angled", bevel_start = 0, bevel_end = 0.2, max_height=0.25)
#'   roof_model = generate_beveled_polygon(skeleton,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse="purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Change the bevel to be circular
#' if(run_docs_raybevel()) {
#'   bevel = generate_bevel("circular", bevel_start = 0, bevel_end = 0.2, max_height=0.25)
#'   roof_model = generate_beveled_polygon(skeleton,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse="purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Change the bevel to type "bump", change the max height, and raise it off the surface
#' if(run_docs_raybevel()) {
#'   bevel = generate_bevel("bump", bevel_start = 0, bevel_end = 0.4, max_height=0.25)
#'   roof_model = generate_beveled_polygon(skeleton, base_height=1,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse="purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Generate a complex bevel and use the exact specified heights
#' if(run_docs_raybevel()) {
#'   bevel = generate_complex_bevel(c("bump", "exp", "circular","step"),
#'                                  bevel_start = c(0,0.3,0.7,0.95),
#'                                  bevel_end = c(0.1,0.6,0.95,1),
#'                                  reverse = c(F,F,T,F),
#'                                  segment_height = c(0.25,0.5,0.5,4),
#'                                  plot_bevel = TRUE)
#'
#'   roof_model = generate_beveled_polygon(skeleton, vertical_offset=0.1,
#'                                         bevel_offsets = bevel,
#'                                         raw_heights = TRUE,
#'                                         material = material_list(diffuse="purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Turn the polygon into a ziggurat, using the step bevel type
#' if(run_docs_raybevel()) {
#'   offs = seq(0, 1, by = 0.05)
#'   bevel = generate_complex_bevel("step",
#'                                  bevel_start = offs[-length(offs)],
#'                                  bevel_end = offs[-1],
#'                                  segment_height = 0.2)
#'
#'   roof_model = generate_beveled_polygon(skeleton, vertical_offset=0.2,
#'                                         bevel_offsets = bevel,
#'                                         raw_heights = TRUE,
#'                                         material = material_list(diffuse = "purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom = c(10,30,20), sample_method = "sobol_blue",
#'                  width = 800, height = 800, fov = 0, ortho_dimensions = c(10,10))
#' }
#'
#' # Turn the polygon into a smooth wavy slide, taking advantage of vector recycling to flip/reverse
#' if(run_docs_raybevel()) {
#'   offs = seq(0, 1, by = 0.1)
#'   bevel = generate_complex_bevel("exp",
#'                                  bevel_start = offs[-length(offs)],
#'                                  bevel_end = offs[-1],
#'                                  reverse = c(TRUE, FALSE),
#'                                  flip = c(TRUE, FALSE),
#'                                  segment_height = 0.25)
#'
#'   roof_model = generate_beveled_polygon(skeleton, vertical_offset=0.2,
#'                                         bevel_offsets = bevel,
#'                                         raw_heights = TRUE,
#'                                         material = material_list(diffuse = "purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom = c(10,30,20), sample_method = "sobol_blue",
#'                  width = 800, height = 800, fov = 0, ortho_dimensions = c(10,10))
#' }
#'
#' # Skeletonize and turn an {sf} object into a beveled polygon
#' if(run_docs_raybevel()) {
#'   us_states = spData::us_states
#'   texas = us_states[us_states$NAME == "Texas",]
#'   texas_skeleton = skeletonize(texas)
#'   plot_skeleton(texas_skeleton)
#'
#'   bevel = generate_bevel("angled" , bevel_end=0.3, max_height = 0.3)
#'   roof_model_texas = generate_beveled_polygon(texas_skeleton,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse = "purple")) |>
#'     center_mesh() |>
#'     translate_mesh(c(0,0.3,0))
#'
#'   raymesh_model(roof_model_texas, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     add_object(sphere(x=-10,z=-10,y=5,material=light(color="red", intensity=40))) |>
#'     add_object(sphere(x=10,z=-10,y=5,material=light(color="orange", intensity=40))) |>
#'     render_scene(lookfrom=c(0,10,0),camera_up=c(0,0,1), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0, ortho_dimensions=c(15,15))
#' }
#'
#' # Generate a smooth bevel
#' if(run_docs_raybevel()) {
#'   bevel = generate_bevel("exp", bevel_start = 0, bevel_end=0.5, max_height=2)
#'   roof_model_texas = generate_beveled_polygon(texas_skeleton,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse = "purple")) |>
#'     center_mesh() |>
#'     translate_mesh(c(0,0.5,0))
#'
#'   raymesh_model(roof_model_texas, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     add_object(sphere(x=-10,z=-10,y=5,material=light(color="red", intensity=40))) |>
#'     add_object(sphere(x=10,z=-10,y=5,material=light(color="orange", intensity=40))) |>
#'     render_scene(lookfrom=c(0,10,0),camera_up=c(0,0,1), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0, ortho_dimensions=c(15,15))
#' }
generate_beveled_polygon = function(skeleton,
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
                                    verbose = FALSE) {
  if(run_docs_raybevel() || !interactive()) {
    progress = FALSE
  }
  if(!is.list(bevel_material)) {
    bevel_material = material
  }
  init_time()
  if(inherits(skeleton, "rayskeleton_list")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating bevel [:bar] eta: :eta",
      total = length(skeleton), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    vec_default = function(length_skeleton, value) {
      if(length(value) == 1) {
        return(rep(value,length(skeleton)))
      } else {
        stopifnot(length(value) == length_skeleton)
        return(value)
      }
    }
    len_s = length(skeleton)
    vertical_offset = vec_default(len_s, vertical_offset)
    base_height = vec_default(len_s, base_height)
    if(length(max_height) == 1) {
      if(scale_all_max) {
        max_height = rep(max_height,length(skeleton))
      } else {
        #Calculate proportional maximum heights
        single_max_heights = unlist(lapply(skeleton, \(x) max(x$nodes$time)))
        max_height = single_max_heights/max(single_max_heights) * max_height
      }
    }
    for(j in seq_len(length(skeleton))) {
      if(progress) {
        pb$tick()
      }
      meshlist[[counter]] = generate_beveled_polygon(skeleton[[j]],
                                                     bevel_offsets = bevel_offsets,
                                                     bevel_heights = bevel_heights,
                                                     bevel_material = bevel_material,
                                                     base = base,
                                                     vertical_offset = vertical_offset[j],
                                                     max_height = max_height[j],
                                                     raw_offsets = raw_offsets,
                                                     raw_heights = raw_heights,
                                                     sides = sides,
                                                     double_sided = double_sided,
                                                     base_height = base_height[j],
                                                     set_max_height = set_max_height,
                                                     return_skeleton_polygons = return_skeleton_polygons,
                                                     swap_yz = swap_yz,
                                                     progress = progress,
                                                     material = material)
      counter = counter + 1
    }
    if(return_skeleton_polygons) {
      class(meshlist) = c("rayskeleton_list_polygons", "list")
      return(meshlist)
    }
    return(scene_from_list(meshlist))
  }
  if(!inherits(skeleton, "rayskeleton")) {
    stop("`skeleton` must be of class `rayskeleton`")
  }
  max_time = max(skeleton$links$destination_time)
  if(is.list(bevel_offsets) &&
     !is.null(bevel_offsets$x) &&
     !is.null(bevel_offsets$y)) {
    bevel_heights = bevel_offsets$y
    bevel_offsets = bevel_offsets$x
  }

  if(!raw_offsets) {
    if(any(bevel_offsets > 1 | bevel_offsets < 0)) {
      stop("If using percentage offsets, all `bevel_offsets` must be between 0 and 1.")
    }
    bevel_offsets = bevel_offsets * max_time
  }
  non_zero_offsets = bevel_offsets[bevel_offsets > 0]
  if(all(non_zero_offsets >= max_time)) {
    message(sprintf("All `bevel_offset` greater than max offset in polygon of %f, calculating full polygon model",
                    max(skeleton$nodes[,4])))
    interpolated_data = stats::approx(x = c(bevel_offsets),
                                      y = c(bevel_heights),
                                      xout =  max_time)
    max_height = max(interpolated_data$y)
    return(generate_roof(skeleton,
                         max_height = max_height,
                         base_height = base_height,
                         vertical_offset = vertical_offset,
                         swap_yz = swap_yz,
                         base = base,
                         sides = sides,
                         double_sided = double_sided,
                         progress = progress,
                         roof_material = bevel_material,
                         material = material))
  }
  if(!raw_heights) {
    bevel_heights = bevel_heights * max_time
  }

  bevel_heights = bevel_heights[order(bevel_offsets)]
  bevel_offsets = bevel_offsets[order(bevel_offsets)]

  height_range = range(bevel_heights)
  height_range = height_range[2]-height_range[1]
  if(set_max_height && !is.na(max_height)) {
    stopifnot(max_height > 0)
    min_height = min(bevel_heights)
    offset_bevel_heights = bevel_heights - min_height
    offset_bevel_heights = max_height * offset_bevel_heights / height_range + min_height
    height_range = max_height
    bevel_heights = offset_bevel_heights
  }
  stopifnot(length(vertical_offset) == 1)
  stopifnot(vertical_offset >= 0)
  bevel_heights = bevel_heights + vertical_offset

  valid_bevels = bevel_offsets <= max_time & bevel_offsets > 0
  zero_height_val = bevel_heights[1]
  bevel_offsets_polys = bevel_offsets[valid_bevels]
  bevel_heights_polys = bevel_heights[valid_bevels]

  stopifnot(length(bevel_offsets_polys) == length(bevel_heights_polys))

  #Remove extremely close offsets (usually arising from floating point error)
  for(i in rev(seq_len(length(bevel_offsets_polys)))) {
    all_other_offsets = bevel_offsets_polys[-i]
    if(any(abs(bevel_offsets_polys[i] - all_other_offsets) < 1e-14, na.rm = TRUE)) {
      bevel_offsets_polys[i] = NA
    }
  }
  remove_close_vals = !is.na(bevel_offsets_polys)
  bevel_offsets_polys = bevel_offsets_polys[remove_close_vals]
  bevel_heights_polys = bevel_heights_polys[remove_close_vals]

  #Generate new nodes
  beveled_ss = generate_offset_links_nodes(skeleton, bevel_offsets_polys, verbose = verbose, progress = progress)
  print_time(verbose, "Generated offset links")

  reordered_new_ss = recalculate_ordered_ids(beveled_ss)
  print_time(verbose, "Recalculated ordered IDs")

  polygon_ind = convert_ss_to_polygons(reordered_new_ss, progress = progress)
  print_time(verbose, "Converted to polygons")

  nodes = reordered_new_ss$nodes
  index_list = list()
  for(i in seq_len(length(polygon_ind))) {
    tmp_ind = polygon_ind[[i]]
    tmp_poly = nodes[tmp_ind,2:3]
    index_list[[i]] = matrix(tmp_ind[decido::earcut(tmp_poly)],byrow=TRUE,ncol=3)
  }
  print_time(verbose, "Triangulated polygons")

  if(length(index_list) == 1) {
    indices_all = matrix(unlist(index_list),ncol=3, byrow = TRUE)
  } else {
    indices_all = do.call("rbind",index_list)
  }
  xyz = nodes[,2:4]
  new_xyz = xyz
  bevel_offsets_polys = c(0,bevel_offsets_polys)
  bevel_heights_polys = c(zero_height_val,bevel_heights_polys)

  if(bevel_offsets_polys[length(bevel_offsets_polys)] != max_time) {
    bevel_offsets_with_max = c(bevel_offsets_polys, max_time)
    bevel_heights_with_max = c(bevel_heights_polys, max(bevel_heights_polys))
  } else {
    bevel_offsets_with_max = bevel_offsets_polys
    bevel_heights_with_max = bevel_heights_polys
  }
  side_top = bevel_heights_with_max[1]

  if(return_skeleton_polygons) {
    attr(reordered_new_ss, "polygons") = polygon_ind
    #These offsets should always be normalized
    attr(reordered_new_ss, "bevel_offsets") = bevel_offsets_with_max / max_time
    attr(reordered_new_ss, "raw_offsets") = raw_offsets
    class(reordered_new_ss) = c("rayskeleton_polygon")
    return(reordered_new_ss)
  }
  #Need to account for non-zero offsets--not all start at 0
  new_xyz[,3] = stats::approx(x=bevel_offsets_with_max,y=bevel_heights_with_max, xout = xyz[,3], rule=2)$y

  xyz = new_xyz
  colnames(xyz) = c("x","y","z")
  original_verts = attr(skeleton,"original_vertices")
  original_holes = attr(skeleton,"original_holes")
  mesh = construct_mesh(vertices = as.matrix(xyz),
                        indices = as.matrix(indices_all)-1) |>
    set_material(bevel_material)

  if(double_sided) {
    xyzflip = xyz
    xyzflip[,3] = -xyzflip[,3]
    indices_flip = indices_all[,3:1,drop=FALSE]
    double_mesh = construct_mesh(vertices = as.matrix(xyzflip),
                                 indices = as.matrix(indices_flip)-1) |>
      set_material(bevel_material)
  }
  if(base && !double_sided) {
    if(length(original_holes) > 0) {
      holes = c(0,utils::head(cumsum(unlist(lapply(original_holes,nrow))),-1)) + nrow(original_verts) + 1
      hole_mat = do.call("rbind", original_holes)
      original_verts_holes = rbind(original_verts, hole_mat)
    } else {
      holes = 0
      original_verts_holes = original_verts
    }
    base_indices = matrix(decido::earcut(original_verts_holes, holes = holes), byrow = TRUE, ncol=3)
    base_indices = base_indices[,3:1,drop = FALSE]
    original_verts_base = cbind(original_verts_holes, rep(0, nrow(original_verts_holes)))
    base_mesh = construct_mesh(vertices = as.matrix(original_verts_base),
                               indices = as.matrix(base_indices)-1) |>
      set_material(material)

    mesh = mesh |>
      add_shape(base_mesh)
  }
  if(double_sided) {
    mesh = add_shape(mesh, double_mesh)
  }
  if((sides || base || double_sided) &&
     ((!double_sided && side_top > 0) || (double_sided && side_top > 0))) {
    if(!double_sided) {
      side_mesh = extrude_sides(original_verts, original_holes, bottom = 0, top = side_top)
    } else {
      side_mesh = extrude_sides(original_verts, original_holes, bottom = -side_top, top = side_top)
    }
    side_mesh = side_mesh |>
      set_material(material)
    mesh = add_shape(mesh, side_mesh)
  }
  if(swap_yz) {
    mesh = swap_yz(mesh)
  }
  if(!is.na(base_height)) {
    if(swap_yz) {
      mesh = translate_mesh(mesh, c(0,base_height,0))
    } else {
      mesh = translate_mesh(mesh, c(0,0,base_height))
    }
  }
  print_time(verbose, "Generated mesh")

  return(mesh)
}

#' Change an existing polygon bevel's bevel profile.
#'
#' This function generates a beveled 3D polygon model from the modified straight skeleton with
#' pre-existing polygons generated from the `generate_beveled_polygon` function when
#' `return_skeleton_polygons = TRUE`.
#'
#' @param skeleton_polygons Default `NULL`. A straight skeleton generated from the `generate_beveled_polygon` function when
#' `return_skeleton_polygons = TRUE`.
#' @param bevel_offsets Default `NULL`. The offset(s) of the bevel.
#' @param bevel_heights Default is set to `bevel_offsets`. Numeric vector specifying the heights of the bevels. Must be of the same length as `bevel_offsets`.
#' @param max_height Default `1`. The maximum height of the polygon.
#' @param set_max_height Default `FALSE`. A logical flag that controls whether to set the max height of the polygon based on the `max_height` argument.
#' @param swap_yz Default `TRUE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh.
#' If `TRUE`, the y and z coordinates will be swapped.
#' @param sides Default `FALSE`. A logical flag on whether to draw the sides. This will automatically be set to `TRUE`
#' if `base = TRUE` and the `base_height` is less than `vertical_offset`.
#' @param vertical_offset Default `0`. The vertical offset of the polygon.
#' @param base Default `TRUE`. A logical flag that controls whether to generate the bottom of the polygon.
#' @param base_height Default `NA`. Height of the base, defaulting to the `min(bevel_heights) + vertical_offset` .
#' @param progress Default `TRUE`. Whether to display a progress bar.
#' @param verbose Default `FALSE`. A logical flag to control whether additional timing information should be displayed.
#' @param scale_all_max Default `FALSE`. If passing in a list of multiple skeletons with polygons, whether to scale each polygon to the overall
#' max height, or whether to scale each max height to the maximum internal distance in the polygon.
#' @param raw_offsets Default `FALSE`. A logical flag indicating whether the `bevel_offsets` are already in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' @param raw_heights Default `FALSE`. A logical flag indicating whether the `bevel_heights` are already in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' @param double_sided Default `FALSE`. A logical flag that controls whether the polygon should be double-sided.
#' @param material Default `material_list()`. Interface to set the color/appearance/material options for the resulting `ray_mesh` mesh.
#' @param bevel_material Default `NA`, uses the material specified in `material`. Interface to set the color/appearance/material options for the resulting `ray_mesh` bevel mesh.
#'
#' @return A 3D mesh of the beveled polygon model.
#'
#' @import rayvertex
#'
#' @export
#' @examples
#' # Skeletonize a complex {sf} object and set return_skeleton_polygons = TRUE in
#' # generate_beveled_polygon(). This returns skeleton object with polygons included, which
#' # allows for quickly generating 3D models with different bevels.
#' if(run_docs_raybevel()) {
#'   library(rayrender)
#'   library(rayvertex)
#'   us_states = spData::us_states
#'   cali = us_states[us_states$NAME == "California",]
#'   cali_skeleton = skeletonize(cali)
#'   plot_skeleton(cali_skeleton)
#'   # We add manual offsets to ensure that the polygon can be morphed all along its interior
#'   bevel = generate_bevel(manual_offsets = seq(0,1,by=0.01), max_height=0.5)
#'   bevel_model_cali = generate_beveled_polygon(cali_skeleton,
#'                                               bevel_offsets = bevel,
#'                                               return_skeleton_polygons = TRUE)
#'
#'   bevel_new = change_polygon_bevel(bevel_model_cali,
#'                                    bevel_offsets = generate_bevel(max_height=0.5,
#'                                    bevel_end=0.5)) |>
#'     center_mesh()
#'
#'   scene_base = xz_rect(xwidth=100,zwidth=100,
#'                        material=diffuse(color="grey20", checkercolor="white")) |>
#'     add_object(sphere(y=8,z=10,x=-3,material=light(intensity=100))) |>
#'     add_object(sphere(y=800,z=10,x=-3,radius=100,material=light(intensity=5)))
#'
#'   raymesh_model(bevel_new, y=0.5, override_material = TRUE,
#'                 material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(0,30,-10), sample_method = "sobol_blue",clamp_value = 10,
#'                  width=800,height=800,fov=0,ortho_dimensions=c(12,12))
#' }
#' # Change to a smooth bevel
#' if(run_docs_raybevel()) {
#'   new_bevel = generate_bevel("circular", bevel_start = 0, bevel_end=1)
#'   bevel_new = change_polygon_bevel(bevel_model_cali,
#'                                    bevel_offsets = new_bevel, solid ) |>
#'     center_mesh()
#'   raymesh_model(bevel_new, override_material = TRUE, y=1,material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(0,30,-10), sample_method = "sobol_blue",clamp_value = 10,
#'                  width=800,height=800,fov=0,ortho_dimensions=c(12,12))
#' }
#'
#' # Make a complex bevel
#' if(run_docs_raybevel()) {
#'   complex_coords = generate_complex_bevel(
#'     bevel_type  = c("angled","flat", "angled", "flat"),
#'     bevel_start = head(seq(0,1,by=0.05),-1),
#'     bevel_end   = tail(seq(0,1,by=0.05),-1),
#'     overall_height = 1,
#'     angle = c(45,45,15,15),
#'     reverse = c(FALSE, FALSE,TRUE,TRUE),
#'     plot_bevel = TRUE
#'   )
#'   bevel_new = change_polygon_bevel(bevel_model_cali,
#'                                    bevel_offsets = complex_coords) |>
#'     center_mesh()
#'   raymesh_model(bevel_new, override_material = TRUE, y=1,material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(0,30,-20), sample_method = "sobol_blue",clamp_value = 10,
#'                  width=800,height=800,fov=0,ortho_dimensions=c(12,12))
#' }
#'
#' # Quickly generate new bevels to inflate California like a balloon using the arctan function.
#' if(run_docs_raybevel()) {
#'   inflate_california = function(magnitudes) {
#'   for(val in magnitudes) {
#'     bevel_new = change_polygon_bevel(bevel_model_cali,
#'                                      bevel_heights = 1/2*atan(seq(0,val,length.out=100)),
#'                                      bevel_offsets = seq(0,1, length.out=100),
#'                                      base = TRUE) |>
#'       translate_mesh(c(-120.49,0,-38.72))
#'     raymesh_model(bevel_new, y = 0, override_material = TRUE,
#'                   material = glossy(color="darkred")) |>
#'       add_object(scene_base) |>
#'       add_object(sphere(x=-30,z=30,y=18,radius=30,material=light(color="white", intensity=5))) |>
#'       render_scene(lookfrom=c(-1, 28, -20.32), lookat=c(-1, 1.46, -2),
#'                    sample_method = "sobol_blue", clamp_value = 10,
#'                    width=800,height=800,fov=20,samples=256)
#'     }
#'   }
#'   inflate_california(c(1,4,16,64))
#' }
change_polygon_bevel = function(skeleton_polygons,
                                bevel_offsets = NULL,
                                bevel_heights = NULL,
                                set_max_height = FALSE,
                                max_height = 1,
                                vertical_offset = 0,
                                base = TRUE,
                                base_height = NA,
                                raw_offsets = FALSE,
                                raw_heights = FALSE,
                                swap_yz = TRUE,
                                progress = TRUE,
                                sides = FALSE,
                                double_sided = FALSE,
                                scale_all_max = FALSE,
                                material = material_list(),
                                bevel_material = NA,
                                verbose = FALSE) {
  if(run_docs_raybevel() || !interactive()) {
    progress = FALSE
  }
  if(!is.list(bevel_material)) {
    bevel_material = material
  }
  init_time()
  if(inherits(skeleton_polygons, "rayskeleton_list_polygons")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating polygon [:bar] eta: :eta",
      total = length(skeleton_polygons), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(vertical_offset) == 1) {
      vertical_offset = rep(vertical_offset,length(skeleton_polygons))
    }
    if(length(max_height) == 1) {
      if(scale_all_max) {
        max_height = rep(max_height,length(skeleton_polygons))
      } else {
        #Calculate proportional maximum heights
        single_max_heights = unlist(lapply(skeleton_polygons, \(x) max(x$nodes$time)))
        max_height = single_max_heights/max(single_max_heights) * max_height
      }
    }
    for(j in seq_len(length(skeleton_polygons))) {
      if(progress) {
        pb$tick()
      }
      meshlist[[counter]] = change_polygon_bevel(skeleton_polygons[[j]],
                                                 bevel_offsets = bevel_offsets,
                                                 bevel_heights = bevel_heights,
                                                 base = base,
                                                 vertical_offset = vertical_offset[j],
                                                 max_height = max_height[j],
                                                 raw_offsets = raw_offsets,
                                                 raw_heights = raw_heights,
                                                 sides = sides,
                                                 progress = FALSE,
                                                 double_sided = double_sided,
                                                 base_height = base_height,
                                                 bevel_material = bevel_material,
                                                 material = material,
                                                 swap_yz = swap_yz)
      counter = counter + 1
    }
    return(scene_from_list(meshlist))
  }
  if(inherits(skeleton_polygons, "ray_mesh")) {
    return(skeleton_polygons)
  }
  if(!inherits(skeleton_polygons, "rayskeleton_polygon")) {
    stop("`skeleton_polygon` must be of class `rayskeleton_polygon`")
  }
  max_time = max(skeleton_polygons$links$destination_time)
  if(is.list(bevel_offsets) &&
     !is.null(bevel_offsets$x) &&
     !is.null(bevel_offsets$y)) {
    bevel_heights = bevel_offsets$y
    bevel_offsets = bevel_offsets$x
    # if(set_max_height) {
    #   warning("`set_max_height` is ignored when passing in bevel curve--set the max height when generating the bevel.")
    #   set_max_height = FALSE
    # }
  }
  if(!raw_offsets) {
    if(any(bevel_offsets > 1 | bevel_offsets < 0)) {
      stop("If using percentage offsets, all `bevel_offsets` must be between 0 and 1.")
    }
    bevel_offsets_pct = bevel_offsets
    bevel_offsets = bevel_offsets * max_time
  } else {
    bevel_offsets_pct = bevel_offsets / max_time
  }
  #Offsets should be in units of the coordinate system at this point
  non_zero_offsets = bevel_offsets[bevel_offsets > 0]
  if(all(non_zero_offsets >= max_time)) {
    interpolated_data = stats::approx(x = bevel_offsets, y = bevel_heights,
                                      xout =  c(0,max_time))
    bevel_offsets = c(0,max_time)
    bevel_offsets_pct = c(0,1)
    bevel_heights = interpolated_data$y
  }
  if(!raw_heights) {
    bevel_heights = bevel_heights * max_time
  }
  if(is.na(base_height)) {
    height_range = range(bevel_heights)
    if(base || double_sided) {
      min_offset = (height_range[2]-height_range[1]) * 1e-6
    } else {
      min_offset = 0
    }
    base_height = min(bevel_heights) + min_offset + vertical_offset
  }

  stopifnot(length(vertical_offset) == 1)

  reordered_new_ss = skeleton_polygons
  old_bevel_offsets = unique(attr(reordered_new_ss, "bevel_offsets"))
  polygon_ind = attr(reordered_new_ss, "polygons")
  last_height = bevel_heights[length(bevel_heights)]
  if(bevel_offsets_pct[length(bevel_offsets_pct)] != 1 ) {
    bevel_offsets_pct = c(bevel_offsets_pct,1)
    bevel_heights_with_last = c(bevel_heights,last_height)
  } else {
    bevel_heights_with_last = bevel_heights
  }
  interpolated_data = stats::approx(x = bevel_offsets_pct, y = bevel_heights_with_last, xout = old_bevel_offsets,
                                    rule = 2)
  bevel_heights = interpolated_data$y
  bevel_offsets = old_bevel_offsets * max_time
  bevel_offsets = bevel_offsets[order(bevel_offsets)]
  bevel_heights = bevel_heights[order(bevel_offsets)]

  height_range = range(bevel_heights)
  height_range = height_range[2]-height_range[1]
  if(set_max_height) {
    min_height = min(bevel_heights)
    offset_bevel_heights = bevel_heights - min_height
    offset_bevel_heights = max_height * offset_bevel_heights / height_range + min_height
    height_range = max_height
    bevel_heights = offset_bevel_heights
  }
  stopifnot(length(vertical_offset) == 1)
  stopifnot(vertical_offset >= 0)
  bevel_heights = bevel_heights + vertical_offset

  valid_bevels = bevel_offsets <= max_time & bevel_offsets > 0
  zero_height_val = bevel_heights[1]
  bevel_offsets_valid = bevel_offsets[valid_bevels]
  bevel_heights_valid = bevel_heights[valid_bevels]
  stopifnot(length(bevel_offsets_valid) == length(bevel_heights_valid))
  #Remove extremely close offsets (usually arising from floating point error)
  for(i in rev(seq_len(length(bevel_offsets_valid)))) {
    all_other_offsets = bevel_offsets_valid[-i]
    if(any(abs(bevel_offsets_valid[i] - all_other_offsets) < 1e-14, na.rm = TRUE)) {
      bevel_offsets_valid[i] = NA
    }
  }
  remove_close_vals = !is.na(bevel_offsets_valid)
  bevel_offsets_valid = bevel_offsets_valid[remove_close_vals]
  bevel_heights_valid = bevel_heights_valid[remove_close_vals]

  nodes = reordered_new_ss$nodes
  index_list = list()
  for(i in seq_len(length(polygon_ind))) {
    tmp_ind = polygon_ind[[i]]
    tmp_poly = nodes[tmp_ind,2:3]
    index_list[[i]] = matrix(tmp_ind[decido::earcut(tmp_poly)], byrow = TRUE, ncol = 3)
  }
  indices_all = do.call("rbind",index_list)
  xyz = nodes[,2:4]

  new_xyz = xyz

  bevel_offsets_polys = c(0,bevel_offsets_valid)
  bevel_heights_polys = c(zero_height_val,bevel_heights_valid)

  if(bevel_offsets_polys[length(bevel_offsets_polys)] != max_time) {
    bevel_offsets_with_max = c(bevel_offsets_polys, max_time)
    bevel_heights_with_max = c(bevel_heights_polys, max(bevel_heights_polys))
  } else {
    bevel_offsets_with_max = bevel_offsets_polys
    bevel_heights_with_max = bevel_heights_polys
  }
  side_top = bevel_heights_with_max[1]

  new_xyz[,3] = stats::approx(x=bevel_offsets_with_max,y=bevel_heights_with_max, xout = xyz[,3], rule=2)$y

  xyz = new_xyz
  colnames(xyz) = c("x","y","z")

  original_verts = attr(reordered_new_ss,"original_vertices")
  original_holes = attr(reordered_new_ss,"original_holes")

  mesh = construct_mesh(vertices = as.matrix(xyz),
                                   indices = as.matrix(indices_all)-1)|>
    set_material(bevel_material)

  if(double_sided) {
    xyzflip = xyz
    xyzflip[,3] = -xyzflip[,3]
    indices_flip = indices_all[,3:1,drop=FALSE]
    double_mesh = construct_mesh(vertices = as.matrix(xyzflip),
                                 indices = as.matrix(indices_flip)-1)|>
      set_material(bevel_material)
  }
  if(base && !double_sided) {
    if(length(original_holes) > 0) {
      holes = c(0,utils::head(cumsum(unlist(lapply(original_holes,nrow))),-1)) + nrow(original_verts) + 1
      hole_mat = do.call("rbind", original_holes)
      original_verts_holes = rbind(original_verts, hole_mat)
    } else {
      holes = 0
      original_verts_holes = original_verts
    }
    base_indices = matrix(decido::earcut(original_verts_holes, holes = holes), byrow = TRUE, ncol=3)
    base_indices = base_indices[,3:1,drop = FALSE]
    original_verts_base = cbind(original_verts_holes, rep(0, nrow(original_verts_holes)))
    base_mesh = construct_mesh(vertices = as.matrix(original_verts_base),
                               indices = as.matrix(base_indices)-1) |>
      set_material(material)

    mesh = mesh |>
      add_shape(base_mesh)
  }
  if(double_sided) {
    mesh = add_shape(mesh, double_mesh)
  }
  if((sides || base || double_sided) &&
     ((!double_sided && side_top > 0) || (double_sided && side_top > 0))) {
    if(!double_sided) {
      side_mesh = extrude_sides(original_verts, original_holes, bottom = 0, top = side_top)
    } else {
      side_mesh = extrude_sides(original_verts, original_holes, bottom = -side_top, top = side_top)
    }
    side_mesh = side_mesh |>
      set_material(material)
    mesh = add_shape(mesh, side_mesh)
  }
  if(swap_yz) {
    mesh = swap_yz(mesh)
  }
  if(!is.na(base_height)) {
    if(swap_yz) {
      mesh = translate_mesh(mesh, c(0,base_height,0))
    } else {
      mesh = translate_mesh(mesh, c(0,0,base_height))
    }
  }
  print_time(verbose, "Generated mesh")

  return(mesh)
}
