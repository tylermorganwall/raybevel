#' Generate a 3D roof model
#'
#' This function generates a 3D roof model from a straight skeleton.
#'
#' @param skeleton Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param max_height Default `NA`. The maximum height of the roof.
#' @param offset Default `0`. The vertical offset of the roof.
#' @param base Default `FALSE`. A logical flag that controls whether to generate the bottom of the roof.
#' @param base_height Default `0`. Height of the base.
#' @param sides Default `FALSE`. A logical flag on whether to draw the sides. This will automatically be set to `TRUE`
#' if `base = TRUE` and the `base_height` is less than `offset`.
#' @param double_sided Default `FALSE`. A logical flag that controls whether the polygon should be double-sided.
#' @param swap_yz Default `TRUE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh.
#' If `TRUE`, the y and z coordinates will be swapped.
#' @param verbose Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#' @param material Default `material_list()`. Interface to set the color/appearance/material options for the resulting `ray_mesh` mesh.
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
#' plot_skeleton(skeleton)
#'
#' #Generate a roof model and specify the material
#' if(run_documentation()) {
#'   library(rayrender)
#'   roof_model = generate_roof(skeleton, material = material_list(diffuse="purple"))
#'   scene_base = xz_rect(xwidth=100,zwidth=100,material=diffuse(color="grey20", checkercolor="white")) |>
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
#' if(run_documentation()) {
#'   roof_model = generate_roof(skeleton, max_height=5)
#'   raymesh_model(roof_model, material = diffuse(color="darkred")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' #Add an offset to the roof
#' if(run_documentation()) {
#'   roof_model = generate_roof(skeleton, offset = 2, base = F)
#'   raymesh_model(roof_model, material = diffuse(color="darkred")) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,10,20), lookat=c(0,2,0), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#'
#' # Skeletonize and turn an {sf} object into a roof
#' if(run_documentation()) {
#'   us_states = spData::us_states
#'   texas = us_states[us_states$NAME == "Texas",]
#'   texas_skeleton = skeletonize(texas)
#'   plot_skeleton(texas_skeleton, arrow_size=0.5)
#'   roof_model_texas = generate_roof(texas_skeleton, max_height = 5) |>
#'     center_mesh() |>
#'     translate_mesh(c(0,2.5,0))
#'
#'   raymesh_model(roof_model_texas, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     add_object(sphere(x=-10,z=-10,y=5,material=light(color="red", intensity=40))) |>
#'     add_object(sphere(x=10,z=-10,y=5,material=light(color="orange", intensity=40))) |>
#'     render_scene(lookfrom=c(0,10,-1), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0, ortho_dimensions=c(15,15))
#' }
generate_roof = function(skeleton, max_height = NA, offset = 0,
                         base = FALSE, base_height = 0,
                         sides = FALSE, double_sided = FALSE,
                         swap_yz = TRUE, verbose = TRUE, material = material_list()) {
  if(inherits(skeleton, "rayskeleton_list")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating roof [:bar] eta: :eta",
      total = length(skeleton), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(offset) == 1) {
      offset = rep(offset,length(skeleton))
    } else {
      stopifnot(length(offset) == length(skeleton))
    }
    if(length(max_height) == 1) {
      max_height = rep(max_height,length(skeleton))
    } else {
      stopifnot(length(max_height) == length(skeleton))
    }
    for(j in seq_len(length(skeleton))) {
      if(verbose) {
        pb$tick()
      }
      meshlist[[counter]] = generate_roof(skeleton[[j]], base = base,
                                          max_height = max_height[j],
                                          sides = sides,
                                          double_sided = double_sided,
                                          base_height = base_height,
                                          offset = offset[j],
                                          swap_yz=swap_yz)
      counter = counter + 1
    }
    return(scene_from_list(meshlist))
  }
  if(!inherits(skeleton, "rayskeleton")) {
    stop("`skeleton` must be of class `rayskeleton`")
  }
  polygon_ind = convert_ss_to_polygons(skeleton, progress = verbose)
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
  original_verts = attr(skeleton,"original_vertices")
  original_holes = attr(skeleton,"original_holes")

  mesh = construct_mesh(vertices = as.matrix(xyz),
                                   indices = as.matrix(indices_all)-1)
  if(is.na(max_height)) {
    scale_val = c(1,1,1)
  } else {
    max_z = get_mesh_bbox(mesh)[2,3]
    scale_val = c(1,1,max_height/max_z)
  }

  if(double_sided) {
    xyzflip = xyz
    xyzflip[,3] = -xyzflip[,3]
    indices_flip = indices_all[,3:1]
    double_mesh = construct_mesh(vertices = as.matrix(xyzflip),
                                 indices = as.matrix(indices_flip)-1) |>
      scale_mesh(scale_val) |>
      translate_mesh(c(0, 0, base_height))
  }
  if(base && !double_sided) {
    if(length(original_holes) > 0) {
      holes = cumsum((unlist(lapply(original_holes,nrow)) + nrow(original_verts)) - nrow(original_holes[[1]])) + 1
      hole_mat = do.call("rbind", original_holes)
      original_verts_holes = rbind(original_verts, hole_mat)
    } else {
      holes = 0
      original_verts_holes = original_verts
    }
    base_indices = matrix(decido::earcut(original_verts_holes, holes = holes), byrow = TRUE, ncol=3)
    base_indices = base_indices[,3:1,drop=FALSE]
    original_verts_base = cbind(original_verts_holes, rep(base_height, nrow(original_verts_holes)))
    base_mesh = construct_mesh(vertices = as.matrix(original_verts_base),
                               indices = as.matrix(base_indices)-1)
    mesh = scale_mesh(mesh, scale_val) |>
      translate_mesh(c(0,0,offset)) |>
      add_shape(base_mesh)
  } else {
    if(double_sided) {
      mesh = scale_mesh(mesh, scale_val) |>
        translate_mesh(c(0,0,offset)) |>
        add_shape(double_mesh)
    }
  }
  if(double_sided) {
    mesh = add_shape(mesh, double_mesh)
  }
  if(swap_yz) {
    mesh = rotate_mesh(mesh, angle=c(90,0,0))
  }
  if(sides || (base && base_height < offset)) {
    side_mesh = extrude_sides(original_verts, original_holes, bottom = base_height, top = offset)
    if(swap_yz) {
      side_mesh = rotate_mesh(side_mesh, angle=c(90,0,0))
    }
    mesh = add_shape(mesh, side_mesh)
  }
  mesh = set_material(mesh, material)
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
#' @param verbose Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#' @param offset Default `0`. The vertical offset of the polygon.
#' @param base Default `FALSE`. A logical flag that controls whether to generate the bottom of the polygon.
#' @param base_height Default `NA`. Height of the base, defaulting to the minimum value of `bevel_heights`.
#' @param sides Default `FALSE`. A logical flag on whether to draw the sides. This will automatically be set to `TRUE`
#' if `base = TRUE` and the `base_height` is less than `offset`.
#' @param raw_offsets Default `FALSE`. A logical flag indicating whether the `bevel_offsets` are already in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' @param raw_heights Default `FALSE`. A logical flag indicating whether the `bevel_heights` are already in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' @param double_sided Default `FALSE`. A logical flag that controls whether the polygon should be double-sided.
#' @param return_skeleton_polygons Default `FALSE`. A logical flag that controls whether to return the skeleton polygons along with the 3D mesh.
#' @param material Default `material_list()`. Interface to set the color/appearance/material options for the resulting `ray_mesh` mesh.
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
#' if(run_documentation()) {
#'   library(rayrender)
#'   scene_base = xz_rect(xwidth=100,zwidth=100,material=diffuse(color="grey20", checkercolor="white")) |>
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
#' if(run_documentation()) {
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
#' if(run_documentation()) {
#'   bevel = generate_bevel("bump", bevel_start = 0, bevel_end = 0.4, max_height=0.20)
#'   roof_model = generate_beveled_polygon(skeleton,offset=0.1,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse="purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Generate a complex bevel
#' if(run_documentation()) {
#'   bevel = generate_complex_bevel(c("bump", "exp", "circular","step"),
#'                                  bevel_start = c(0,0.3,0.7,0.95),
#'                                  bevel_end = c(0.1,0.6,0.95,1),
#'                                  reverse = c(F,F,T,F),
#'                                  max_height = c(0.05,0.2,0.2,1),
#'                                  plot_bevel = TRUE)
#'
#'   roof_model = generate_beveled_polygon(skeleton, offset=0.1,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse="purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom=c(10,30,20), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0,ortho_dimensions=c(10,10))
#' }
#'
#' # Turn the polygon into a ziggurat, using the
#' if(run_documentation()) {
#'   offs = seq(0, 1, by = 0.05)
#'   bevel = generate_complex_bevel("step",
#'                                  bevel_start = offs[-length(offs)],
#'                                  bevel_end = offs[-1],
#'                                  max_height = 0.05)
#'
#'   roof_model = generate_beveled_polygon(skeleton, offset=0.1,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse = "purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom = c(10,30,20), sample_method = "sobol_blue",
#'                  width = 800, height = 800, fov = 0, ortho_dimensions = c(10,10))
#' }
#'
#' # Turn the polygon into a ziggurat
#' if(run_documentation()) {
#'   offs = seq(0, 1, by = 0.05)
#'   bevel = generate_complex_bevel("step",
#'                                  bevel_start = offs[-length(offs)],
#'                                  bevel_end = offs[-1],
#'                                  max_height = 0.05)
#'
#'   roof_model = generate_beveled_polygon(skeleton, offset=0.1,
#'                                         bevel_offsets = bevel,
#'                                         material = material_list(diffuse = "purple"))
#'
#'   raymesh_model(roof_model, override_material = FALSE) |>
#'     add_object(scene_base) |>
#'     render_scene(lookfrom = c(10,30,20), sample_method = "sobol_blue",
#'                  width = 800, height = 800, fov = 0, ortho_dimensions = c(10,10))
#' }
#'
#' # Skeletonize and turn an {sf} object into a roof
#' if(run_documentation()) {
#'   us_states = spData::us_states
#'   texas = us_states[us_states$NAME == "Texas",]
#'   texas_skeleton = skeletonize(texas)
#'   plot_skeleton(texas_skeleton, arrow_size=0.5)
#'   roof_model_texas = generate_roof(texas_skeleton, max_height = 5) |>
#'     center_mesh() |>
#'     translate_mesh(c(0,2.5,0))
#'
#'   raymesh_model(roof_model_texas, material = diffuse(color="purple")) |>
#'     add_object(scene_base) |>
#'     add_object(sphere(x=-10,z=-10,y=5,material=light(color="red", intensity=40))) |>
#'     add_object(sphere(x=10,z=-10,y=5,material=light(color="orange", intensity=40))) |>
#'     render_scene(lookfrom=c(0,10,-1), sample_method = "sobol_blue",
#'                  width=800,height=800,fov=0, ortho_dimensions=c(15,15))
#' }
generate_beveled_polygon = function(skeleton,
                                    bevel_offsets = generate_bevel(),
                                    bevel_heights = NULL,
                                    set_max_height = FALSE,
                                    max_height = 1,
                                    offset = 0,
                                    base = TRUE,
                                    base_height = NA,
                                    raw_offsets = FALSE,
                                    raw_heights = FALSE,
                                    swap_yz = TRUE,
                                    verbose = TRUE,
                                    double_sided = FALSE,
                                    sides = FALSE,
                                    return_skeleton_polygons = FALSE,
                                    material = material_list()) {
  if(inherits(skeleton, "rayskeleton_list")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating roof [:bar] eta: :eta",
      total = length(skeleton), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(offset) == 1) {
      offset = rep(offset,length(skeleton))
    }
    if(length(max_height) == 1) {
      max_height = rep(max_height,length(skeleton))
    }
    for(j in seq_len(length(skeleton))) {
      if(verbose) {
        pb$tick()
      }
      meshlist[[counter]] = generate_beveled_polygon(skeleton[[j]],
                                                     bevel_offsets = bevel_offsets,
                                                     bevel_heights = bevel_heights,
                                                     base = base,
                                                     offset = offset[j],
                                                     max_height = max_height[j],
                                                     raw_offsets = raw_offsets,
                                                     raw_heights = raw_heights,
                                                     sides = sides,
                                                     double_sided = double_sided,
                                                     base_height = base_height,
                                                     return_skeleton_polygons = return_skeleton_polygons,
                                                     swap_yz = swap_yz,
                                                     verbose = verbose,
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
    if(set_max_height) {
      warning("`set_max_height` is ignored when passing in bevel curve--set the max height when generating the bevel.")
      set_max_height = FALSE
    }
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
    return(generate_roof(skeleton,
                         max_height = max_height,
                         offset = offset,
                         swap_yz=swap_yz,
                         verbose = verbose,
                         material = material))
  }
  if(!raw_heights) {
    bevel_heights = bevel_heights * max_time
  }
  bevel_offsets_inserted = modify_bevel_with_skeleton(bevel_offsets, bevel_heights, skeleton)
  bevel_offsets = bevel_offsets_inserted$x
  bevel_heights = bevel_offsets_inserted$y[order(bevel_offsets)]
  bevel_offsets = bevel_offsets[order(bevel_offsets)]

  if(is.na(base_height)) {
    base_height = min(bevel_heights)
  }
  if(double_sided && min(bevel_heights + base_height + offset) <= 0) {
    warning("Double-sided polygons with a minimum height at or equal to zero will intersect with each other, leading to visual artifacts.")
  }

  stopifnot(length(offset) == 1)

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
  beveled_ss = generate_offset_links_nodes(skeleton, bevel_offsets_polys, progress = verbose)
  cleaned_new_ss = remove_node_duplicates(beveled_ss)
  reordered_new_ss = recalculate_ordered_ids(cleaned_new_ss)
  polygon_ind = convert_ss_to_polygons(reordered_new_ss, progress = verbose)

  nodes = reordered_new_ss$nodes
  index_list = list()
  for(i in seq_len(length(polygon_ind))) {
    tmp_ind = polygon_ind[[i]]
    tmp_poly = nodes[tmp_ind,2:3]
    index_list[[i]] = matrix(tmp_ind[decido::earcut(tmp_poly)],byrow=TRUE,ncol=3)
  }
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

  if(return_skeleton_polygons) {
    attr(reordered_new_ss, "polygons") = polygon_ind
    #These offsets should always be normalized
    attr(reordered_new_ss, "bevel_offsets") = bevel_offsets_with_max / max_time
    attr(reordered_new_ss, "raw_offsets") = raw_offsets
    class(reordered_new_ss) = c("rayskeleton_polygon")
    return(reordered_new_ss)
  }

  for(i in seq_len(length(bevel_offsets_with_max))) {
    flat_areas = xyz[,3] >= bevel_offsets_with_max[i]
    new_xyz[flat_areas,3] = bevel_heights_with_max[i]
  }
  xyz = new_xyz
  colnames(xyz) = c("x","y","z")
  original_verts = attr(skeleton,"original_vertices")
  original_holes = attr(skeleton,"original_holes")
  mesh = construct_mesh(vertices = as.matrix(xyz),
                        indices = as.matrix(indices_all)-1)
  if(!set_max_height) {
    scale_val = c(1,1,1)
  } else {
    max_z = get_mesh_bbox(mesh)[2,3]
    scale_val = c(1,1,max_height/max_z)
  }

  if(double_sided) {
    xyzflip = xyz
    xyzflip[,3] = -xyzflip[,3]
    indices_flip = indices_all[,3:1,drop=FALSE]
    double_mesh = construct_mesh(vertices = as.matrix(xyzflip),
                                 indices = as.matrix(indices_flip)-1) |>
      scale_mesh(scale_val) |>
      translate_mesh(c(0, 0, base_height))
  }
  if(base && !double_sided) {
    if(length(original_holes) > 0) {
      holes = cumsum((unlist(lapply(original_holes,nrow)) + nrow(original_verts)) - nrow(original_holes[[1]])) + 1
      hole_mat = do.call("rbind", original_holes)
      original_verts_holes = rbind(original_verts, hole_mat)
    } else {
      holes = 0
      original_verts_holes = original_verts
    }
    base_indices = matrix(decido::earcut(original_verts_holes, holes = holes), byrow = TRUE, ncol=3)
    base_indices = base_indices[,3:1,drop = FALSE]
    original_verts_base = cbind(original_verts_holes, rep(base_height, nrow(original_verts_holes)))
    base_mesh = construct_mesh(vertices = as.matrix(original_verts_base),
                               indices = as.matrix(base_indices)-1)
    mesh = scale_mesh(mesh, scale_val) |>
      translate_mesh(c(0, 0, offset)) |>
      add_shape(base_mesh)
  } else {
    if(double_sided) {
      mesh = scale_mesh(mesh, scale_val) |>
        translate_mesh(c(0,0,offset)) |>
        add_shape(double_mesh)
    }
  }
  if(double_sided) {
    mesh = add_shape(mesh, double_mesh)
  }
  if(swap_yz) {
    mesh = rotate_mesh(mesh, angle=c(90,0,0))
  }

  if(sides || (base && base_height < offset)) {
    side_mesh = extrude_sides(original_verts, original_holes, bottom = base_height, top = offset)
    if(swap_yz) {
      side_mesh = rotate_mesh(side_mesh, angle=c(90,0,0))
    }
    mesh = add_shape(mesh, side_mesh)
  }
  mesh = set_material(mesh, material)
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
#' @param verbose Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#' @param sides Default `FALSE`. A logical flag on whether to draw the sides. This will automatically be set to `TRUE`
#' if `base = TRUE` and the `base_height` is less than `offset`.
#' @param offset Default `0`. The vertical offset of the polygon.
#' @param base Default `FALSE`. A logical flag that controls whether to generate the bottom of the polygon.
#' @param base_height Default `NA`. Height of the base, defaulting to the minimum value of `bevel_heights`.
#' @param raw_offsets Default `FALSE`. A logical flag indicating whether the `bevel_offsets` are already in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' @param raw_heights Default `FALSE`. A logical flag indicating whether the `bevel_heights` are already in raw format and do not need to be multiplied by the maximum time of the skeleton.
#' @param double_sided Default `FALSE`. A logical flag that controls whether the polygon should be double-sided.
#' @param material Default `material_list()`. Interface to set the color/appearance/material options for the resulting `ray_mesh` mesh.
#'
#' @return A 3D mesh of the beveled polygon model.
#'
#' @import rayvertex
#'
#' @export
#' @examples
#' # example code
#'
change_polygon_bevel = function(skeleton_polygons,
                                bevel_offsets = NULL,
                                bevel_heights = NULL,
                                set_max_height = FALSE,
                                max_height = 1,
                                offset = 0,
                                base = FALSE,
                                base_height = 0,
                                raw_offsets = FALSE,
                                raw_heights = FALSE,
                                swap_yz = TRUE,
                                verbose = TRUE,
                                sides = FALSE,
                                double_sided = FALSE,
                                material = material_list()) {
  if(inherits(skeleton_polygons, "rayskeleton_list_polygons")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating polygon [:bar] eta: :eta",
      total = length(skeleton_polygons), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(offset) == 1) {
      offset = rep(offset,length(skeleton_polygons))
    }
    if(length(max_height) == 1) {
      max_height = rep(max_height,length(skeleton_polygons))
    }
    for(j in seq_len(length(skeleton_polygons))) {
      if(verbose) {
        pb$tick()
      }
      meshlist[[counter]] = change_polygon_bevel(skeleton_polygons[[j]],
                                                 bevel_offsets = bevel_offsets,
                                                 bevel_heights = bevel_heights,
                                                 base = base,
                                                 offset = offset[j],
                                                 max_height = max_height[j],
                                                 raw_offsets = raw_offsets,
                                                 raw_heights = raw_heights,
                                                 sides = sides,
                                                 double_sided = double_sided,
                                                 base_height = base_height,
                                                 swap_yz = swap_yz, verbose = verbose)
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
    if(set_max_height) {
      warning("`set_max_height` is ignored when passing in bevel curve--set the max height when generating the bevel.")
      set_max_height = FALSE
    }
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
    base_height = min(bevel_heights)
  }
  if(double_sided && min(bevel_heights + base_height + offset) <= 0) {
    warning("Double-sided polygons with a minimum height at or equal to zero will intersect with each other, leading to visual artifacts.")
  }

  stopifnot(length(offset) == 1)

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

  bevel_offsets_with_max = c(0,bevel_offsets_valid)
  bevel_heights_with_max = c(zero_height_val,bevel_heights_valid)
  for(i in seq_len(length(bevel_offsets_with_max))) {
    #Need to also include a tolerance due to approx() introducing some small floating point error
    flat_areas = xyz[,3] >= bevel_offsets_with_max[i] | abs(xyz[,3] - bevel_offsets_with_max[i]) < 1e-15
    new_xyz[flat_areas,3] = bevel_heights_with_max[i]
  }
  xyz = new_xyz

  colnames(xyz) = c("x","y","z")

  original_verts = attr(reordered_new_ss,"original_vertices")
  original_holes = attr(reordered_new_ss,"original_holes")

  mesh = construct_mesh(vertices = as.matrix(xyz),
                                   indices = as.matrix(indices_all)-1)
  if(!set_max_height) {
    scale_val = c(1,1,1)
  } else {
    max_z = get_mesh_bbox(mesh)[2,3]
    scale_val = c(1,1,max_height/max_z)
  }
  if(double_sided) {
    xyzflip = xyz
    xyzflip[,3] = -xyzflip[,3]
    indices_flip = indices_all[,3:1]
    double_mesh = construct_mesh(vertices = as.matrix(xyzflip),
                                 indices = as.matrix(indices_flip)-1) |>
      scale_mesh(scale_val) |>
      translate_mesh(c(0, 0, base_height))
  }
  if(base && !double_sided) {
    if(length(original_holes) > 0) {
      holes = cumsum((unlist(lapply(original_holes,nrow)) + nrow(original_verts)) - nrow(original_holes[[1]])) + 1
      hole_mat = do.call("rbind", original_holes)
      original_verts_holes = rbind(original_verts, hole_mat)
    } else {
      holes = 0
      original_verts_holes = original_verts
    }
    base_indices = matrix(decido::earcut(original_verts_holes, holes = holes), byrow = TRUE, ncol=3)
    base_indices = base_indices[,3:1,drop=FALSE]
    original_verts_base = cbind(original_verts_holes, rep(base_height, nrow(original_verts_holes)))
    base_mesh = construct_mesh(vertices = as.matrix(original_verts_base),
                               indices = as.matrix(base_indices)-1)
    mesh = scale_mesh(mesh, scale_val) |>
      translate_mesh(c(0,0,offset)) |>
      add_shape(base_mesh)
  } else {
    if(double_sided) {
      mesh = scale_mesh(mesh, scale_val) |>
        translate_mesh(c(0,0,offset)) |>
        add_shape(double_mesh)
    }
  }
  if(double_sided) {
    mesh = add_shape(mesh, double_mesh)
  }
  if(swap_yz) {
    mesh = rotate_mesh(mesh, angle=c(90,0,0))
  }
  if(sides || (base && base_height < offset)) {
    min_offset = bevel_heights_with_max[1]
    side_mesh = extrude_sides(original_verts, original_holes, bottom = base_height, top = offset + min_offset)
    if(swap_yz) {
      side_mesh = rotate_mesh(side_mesh, angle=c(90,0,0))
    }
    mesh = add_shape(mesh, side_mesh)
  }
  mesh = set_material(mesh, material)
  return(mesh)
}
