#' Generate a 3D roof model
#'
#' This function generates a 3D roof model from a straight skeleton.
#'
#' @param skeleton Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param max_height Default `NA`. The maximum height of the roof.
#' @param offset_roof Default `0`. The offset of the roof.
#' @param bottom Default `FALSE`. A logical flag that controls whether to generate the bottom of the roof.
#' @param swap_yz Default `TRUE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh.
#' If `TRUE`, the y and z coordinates will be swapped.
#' @param verbose Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#'
#' @return A 3D mesh of the roof model.
#'
#' @export
generate_roof = function(skeleton, max_height = NA, offset_roof = 0, bottom = FALSE,
                         swap_yz = TRUE, verbose = TRUE) {
  if(inherits(skeleton, "rayskeleton_list")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating roof [:bar] eta: :eta",
      total = length(skeleton), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(offset_roof) == 1) {
      offset_roof = rep(offset_roof,length(skeleton))
    } else {
      stopifnot(length(offset_roof) == length(skeleton))
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
      meshlist[[counter]] = generate_roof(skeleton[[j]], bottom = bottom,
                                          max_height = max_height[j],
                                          offset_roof = offset_roof[j],
                                          swap_yz=swap_yz)
      counter = counter + 1
    }
    return(rayvertex::scene_from_list(meshlist))
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
  # xyz[,3] = xyz[,3]/max(xyz[,3])*max_height + offset_roof
  original_verts = attr(skeleton,"original_vertices")
  original_holes = attr(skeleton,"original_holes")
  if(bottom) {
    if(length(original_holes) > 0) {
      holes = unlist(lapply(original_holes,nrow)) + nrow(original_verts) - nrow(original_holes[[1]])
      hole_mat = do.call("rbind", original_holes)
      hole_mat = hole_mat[rev(seq_len(nrow(hole_mat))),]
      original_verts = rbind(original_verts,hole_mat)
    } else {
      holes = 0
    }
    base_indices = matrix(decido::earcut(original_verts, holes = holes),byrow=TRUE, ncol=3)
    base_indices = base_indices[,3:1]
    original_verts = cbind(original_verts,rep(0,nrow(original_verts)))
    base_indices = base_indices + nrow(xyz)
    original_verts = as.data.frame(original_verts)
    colnames(original_verts) = c("x","y","z")
    xyz = rbind(xyz, original_verts)
    indices_all = rbind(indices_all,base_indices)
    mesh = rayvertex::construct_mesh(vertices = as.matrix(xyz),
                                     indices = as.matrix(indices_all)-1) |>
      rayvertex::rotate_mesh(c(0,0,180))
    if(is.na(max_height)) {
      scale_val = c(1,1,1)
    } else {
      max_z = rayvertex::get_mesh_bbox(mesh)[2,3]
      scale_val = c(1,1,max_height/max_z)
    }
    if(swap_yz) {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof)) |>
        rayvertex::rotate_mesh(angle=c(90,0,0))
    } else {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof))
    }
    # offset_roof_full = c(-0.5,-0.5,offset_roof)[col_order]
    # offset_roof_full = c(0,0,offset_roof)[col_order]
    return(mesh)


    # return(rayvertex::translate_mesh(mesh, offset_roof_full))
  } else {
    mesh = rayvertex::construct_mesh(vertices = as.matrix(xyz),
                                     indices = as.matrix(indices_all)-1) |>
      rayvertex::rotate_mesh(c(0,0,180))
    if(is.na(max_height)) {
      scale_val = c(1,1,1)
    } else {
      max_z = rayvertex::get_mesh_bbox(mesh)[2,3]
      scale_val = c(1,1,max_height/max_z)
    }
    if(swap_yz) {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof)) |>
        rayvertex::rotate_mesh(angle=c(90,0,0))
    } else {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof))
    }
    # offset_roof_full = c(0,0,offset_roof)[col_order]
    # offset_roof_full = c(-0.5,0.5,offset_roof)[col_order]

    # scale_max = c(1,1,max_height/max_z)[col_order]

    # mesh = rayvertex::scale_mesh(mesh, scale = scale_max)
    # return(rayvertex::translate_mesh(mesh, offset_roof_full))
    return(mesh)
  }
}

#' Generate a beveled 3D roof model
#'
#' This function generates a beveled 3D roof model from a straight skeleton.
#'
#' @param skeleton Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param bevel_offsets Default `NULL`. The offset(s) of the bevel.
#' @param max_height Default `1`. The maximum height of the roof.
#' @param bevel_heights Default is set to `bevel_offsets`. Numeric vector specifying the heights of the bevels. Must be of the same length as `bevel_offsets`.
#' @param set_max_height Default `FALSE`. A logical flag that controls whether to set the max height of the roof based on the `max_height` argument.
#' @param swap_yz Default `TRUE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh.
#' If `TRUE`, the y and z coordinates will be swapped.
#' @param verbose Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#' @param offset_roof Default `0`. The offset of the roof.
#' @param base Default `FALSE`. A logical flag that controls whether to generate the bottom of the roof.
#'
#' @return A 3D mesh of the beveled roof model.
#'
#' @export
generate_roof_beveled = function(skeleton,
                                 bevel_offsets,
                                 bevel_heights = NULL,
                                 set_max_height = FALSE,
                                 max_height = 1,
                                 offset_roof = 0,
                                 base = FALSE,
                                 raw_offsets = FALSE,
                                 raw_heights = FALSE,
                                 swap_yz = TRUE,
                                 verbose = TRUE,
                                 double_sided = FALSE,
                                 return_skeleton_polygons = FALSE) {
  if(inherits(skeleton, "rayskeleton_list")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating roof [:bar] eta: :eta",
      total = length(skeleton), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(offset_roof) == 1) {
      offset_roof = rep(offset_roof,length(skeleton))
    }
    if(length(max_height) == 1) {
      max_height = rep(max_height,length(skeleton))
    }
    for(j in seq_len(length(skeleton))) {
      if(verbose) {
        pb$tick()
      }
      meshlist[[counter]] = generate_roof_beveled(skeleton[[j]], bevel_offsets = bevel_offsets,
                                                  bevel_heights = bevel_heights,
                                                  base = base,
                                                  offset_roof = offset_roof[j],
                                                  max_height = max_height[j],
                                                  raw_offsets = raw_offsets,
                                                  raw_heights = raw_heights,
                                                  return_skeleton_polygons = return_skeleton_polygons,
                                                  swap_yz = swap_yz, verbose = verbose)
      counter = counter + 1
    }
    if(return_skeleton_polygons) {
      class(meshlist) = c("rayskeleton_list_polygons", "list")
      return(meshlist)
    }
    return(rayvertex::scene_from_list(meshlist))
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
    message(sprintf("All `bevel_offset` greater than max offset in polygon of %f, calculating full roof model",
                    max(skeleton$nodes[,4])))
    return(generate_roof(skeleton, max_height = max_height, offset_roof = offset_roof,
                         swap_yz=swap_yz, verbose = verbose))
  }
  if(!raw_heights) {
    bevel_heights = bevel_heights * max_time
  }
  bevel_offsets_inserted = modify_bevel_with_skeleton(bevel_offsets, bevel_heights, skeleton)
  bevel_offsets = bevel_offsets_inserted$x
  bevel_heights = bevel_offsets_inserted$y
  if(double_sided && min(bevel_heights) <= 0) {
    warning("Double-sided polygons with a minimum height at or equal to zero will intersect with each other, leading to visual artifacts.")
  }

  stopifnot(length(offset_roof) == 1)

  valid_bevels = bevel_offsets <= max_time & bevel_offsets > 0
  bevel_offsets_polys = bevel_offsets[valid_bevels]
  stopifnot(length(bevel_offsets) == length(bevel_heights))

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
  indices_all = do.call("rbind",index_list)
  xyz = nodes[,2:4]
  new_xyz = xyz
  if(bevel_offsets[length(bevel_offsets)] != max_time) {
    bevel_offsets_with_max = c(bevel_offsets, max_time)
  } else {
    bevel_offsets_with_max = bevel_offsets
  }

  if(return_skeleton_polygons) {
    attr(reordered_new_ss, "polygons") = polygon_ind
    #These offsets should always be normalized
    attr(reordered_new_ss, "bevel_offsets") = bevel_offsets_with_max / max_time

    attr(reordered_new_ss, "raw_offsets") = raw_offsets
    class(reordered_new_ss) = c("rayskeleton_polygon")
    return(reordered_new_ss)
  }

  for(i in seq_len(length(bevel_offsets_with_max)-1)) {
    flat_areas = xyz[,3] >= bevel_offsets_with_max[i]
    new_xyz[flat_areas,3] = bevel_heights[i]
  }
  xyz = new_xyz
  colnames(xyz) = c("x","y","z")
  if(double_sided) {
    xyzflip = xyz
    xyzflip[,3] = -xyzflip[,3]
    indices_flip = indices_all + nrow(xyz)
    indices_flip = indices_flip[,3:1]
    xyz = rbind(xyz,xyzflip)
    indices_all = rbind(indices_all, indices_flip)
  }
  if(set_max_height) {
    xyz[,3] = xyz[,3]/max(xyz[,3])*max_height + offset_roof
  } else {
    xyz[,3] = xyz[,3] + offset_roof
  }
  original_verts = attr(skeleton,"original_vertices")
  original_holes = attr(skeleton,"original_holes")
  if(base) {
    if(length(original_holes) > 0) {
      holes = unlist(lapply(original_holes,nrow)) + nrow(original_verts)
      hole_mat = do.call("rbind", original_holes)
      hole_mat = hole_mat[rev(seq_len(nrow(hole_mat))),]
      original_verts = rbind(original_verts,hole_mat)
    } else {
      holes = 0
    }
    base_indices = matrix(decido::earcut(original_verts, holes = holes),byrow=TRUE, ncol=3)
    base_indices = base_indices[,3:1]
    original_verts = cbind(original_verts,rep(0,nrow(original_verts)))
    base_indices = base_indices + nrow(xyz)
    original_verts = as.data.frame(original_verts)
    colnames(original_verts) = c("x","y","z")
    xyz = rbind(xyz, original_verts)
    indices_all = rbind(indices_all,base_indices)
    mesh = rayvertex::construct_mesh(vertices = as.matrix(xyz),
                                     indices = as.matrix(indices_all)-1) |>
      rayvertex::rotate_mesh(c(0,0,180))
    if(!set_max_height) {
      scale_val = c(1,1,1)
    } else {
      max_z = rayvertex::get_mesh_bbox(mesh)[2,3]
      scale_val = c(1,1,max_height/max_z)
    }
    if(swap_yz) {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof)) |>
        rayvertex::rotate_mesh(angle=c(90,0,0))
    } else {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof))
    }
    return(mesh)
  } else {
    mesh = rayvertex::construct_mesh(vertices = as.matrix(xyz),
                                     indices = as.matrix(indices_all)-1) |>
      rayvertex::rotate_mesh(c(0,0,180))
    if(!set_max_height) {
      scale_val = c(1,1,1)
    } else {
      max_z = rayvertex::get_mesh_bbox(mesh)[2,3]
      scale_val = c(1,1,max_height/max_z)
    }
    if(swap_yz) {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof)) |>
        rayvertex::rotate_mesh(angle=c(90,0,0))
    } else {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof))
    }
    return(mesh)
  }
}

#' Generate a beveled 3D roof model
#'
#' This function generates a beveled 3D roof model from a straight skeleton.
#'
#' @param skeleton_polygons Default `NULL`. A straight skeleton generated from the `generate_roof_beveled` function when
#' `return_skeleton_polygons = TRUE`.
#' @param bevel_offsets Default `NULL`. The offset(s) of the bevel.
#' @param bevel_heights Default is set to `bevel_offsets`. Numeric vector specifying the heights of the bevels. Must be of the same length as `bevel_offsets`.
#' @param set_max_height Default `FALSE`. A logical flag that controls whether to set the max height of the roof based on the `max_height` argument.
#' @param swap_yz Default `TRUE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh.
#' If `TRUE`, the y and z coordinates will be swapped.
#' @param offset_roof Default `0`. The offset of the roof.
#' @param base Default `FALSE`. A logical flag that controls whether to generate the bottom of the roof.
#'
#' @return A 3D mesh of the beveled roof model.
#'
#' @export
generate_new_polygon_heights = function(skeleton_polygons,
                                        bevel_offsets = NULL,
                                        bevel_heights = NULL,
                                        set_max_height = FALSE,
                                        max_height = 1,
                                        offset_roof = 0,
                                        base = FALSE,
                                        raw_offsets = FALSE,
                                        raw_heights = FALSE,
                                        swap_yz = TRUE,
                                        verbose = TRUE,
                                        double_sided = FALSE) {
  if(inherits(skeleton_polygons, "rayskeleton_list_polygons")) {
    pb = progress::progress_bar$new(
      format = ":current/:total Generating roof [:bar] eta: :eta",
      total = length(skeleton_polygons), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(offset_roof) == 1) {
      offset_roof = rep(offset_roof,length(skeleton_polygons))
    }
    if(length(max_height) == 1) {
      max_height = rep(max_height,length(skeleton_polygons))
    }
    for(j in seq_len(length(skeleton_polygons))) {
      if(verbose) {
        pb$tick()
      }
      meshlist[[counter]] = generate_new_polygon_heights(skeleton_polygons[[j]],
                                                         bevel_offsets = bevel_offsets,
                                                         bevel_heights = bevel_heights,
                                                         base = base,
                                                         offset_roof = offset_roof[j],
                                                         max_height = max_height[j],
                                                         raw_offsets = raw_offsets,
                                                         raw_heights = raw_heights,
                                                         swap_yz = swap_yz, verbose = verbose)
      counter = counter + 1
    }
    return(rayvertex::scene_from_list(meshlist))
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
    bevel_heights = interpolated_data$y
  }
  if(!raw_heights) {
    bevel_heights = bevel_heights * max_time
  }
  if(double_sided && min(bevel_heights) <= 0) {
    warning("Double-sided polygons with a minimum height at or equal to zero will intersect with each other, leading to visual artifacts.")
  }

  stopifnot(length(offset_roof) == 1)

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
  interpolated_data = stats::approx(x = bevel_offsets_pct, y = bevel_heights_with_last, xout = old_bevel_offsets)
  bevel_heights = interpolated_data$y
  bevel_offsets = old_bevel_offsets * max_time
  # bevel_offsets = old_bevel_offsets * max_time
  bevel_offsets = bevel_offsets[order(bevel_offsets)]
  bevel_heights = bevel_heights[order(bevel_offsets)]

  valid_bevels = bevel_offsets <= max_time & bevel_offsets > 0
  bevel_offsets = bevel_offsets[valid_bevels]
  bevel_heights = bevel_heights[valid_bevels]
  bevel_heights = bevel_heights
  stopifnot(length(bevel_offsets) == length(bevel_heights))

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
  bevel_offsets_with_max = c(0, bevel_offsets)
  bevel_heights_with_max = c(bevel_heights[1], bevel_heights)
  for(i in seq_len(length(bevel_offsets_with_max)-1)) {
    flat_areas = xyz[,3] >= bevel_offsets_with_max[i] | abs(xyz[,3] - bevel_offsets_with_max[i]) < 1e-10
    new_xyz[flat_areas,3] = bevel_heights_with_max[i]
  }
  xyz = new_xyz
  if(any(is.na(xyz))) {}
  colnames(xyz) = c("x","y","z")
  if(double_sided) {
    xyzflip = xyz
    xyzflip[,3] = -xyzflip[,3]
    indices_flip = indices_all + nrow(xyz)
    indices_flip = indices_flip[,3:1]
    xyz = rbind(xyz,xyzflip)
    indices_all = rbind(indices_all, indices_flip)
  }
  if(set_max_height) {
    xyz[,3] = xyz[,3]/max(xyz[,3])*max_height + offset_roof
  } else {
    xyz[,3] = xyz[,3] + offset_roof
  }
  original_verts = attr(reordered_new_ss,"original_vertices")
  original_holes = attr(reordered_new_ss,"original_holes")
  if(base) {
    if(length(original_holes) > 0) {
      holes = unlist(lapply(original_holes,nrow)) + nrow(original_verts)
      hole_mat = do.call("rbind", original_holes)
      hole_mat = hole_mat[rev(seq_len(nrow(hole_mat))),]
      original_verts = rbind(original_verts,hole_mat)
    } else {
      holes = 0
    }
    base_indices = matrix(decido::earcut(original_verts, holes = holes),byrow=TRUE, ncol=3)
    base_indices = base_indices[,3:1]
    original_verts = cbind(original_verts,rep(0,nrow(original_verts)))
    base_indices = base_indices + nrow(xyz)
    original_verts = as.data.frame(original_verts)
    colnames(original_verts) = c("x","y","z")
    xyz = rbind(xyz, original_verts)
    indices_all = rbind(indices_all,base_indices)
    mesh = rayvertex::construct_mesh(vertices = as.matrix(xyz),
                                     indices = as.matrix(indices_all)-1) |>
      rayvertex::rotate_mesh(c(0,0,180))
    if(!set_max_height) {
      scale_val = c(1,1,1)
    } else {
      max_z = rayvertex::get_mesh_bbox(mesh)[2,3]
      scale_val = c(1,1,max_height/max_z)
    }
    if(swap_yz) {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof)) |>
        rayvertex::rotate_mesh(angle=c(90,0,0))
    } else {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof))
    }
    return(mesh)
  } else {
    mesh = rayvertex::construct_mesh(vertices = as.matrix(xyz),
                                     indices = as.matrix(indices_all)-1) |>
      rayvertex::rotate_mesh(c(0,0,180))
    if(!set_max_height) {
      scale_val = c(1,1,1)
    } else {
      max_z = rayvertex::get_mesh_bbox(mesh)[2,3]
      scale_val = c(1,1,max_height/max_z)
    }
    if(swap_yz) {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof)) |>
        rayvertex::rotate_mesh(angle=c(90,0,0))
    } else {
      mesh = rayvertex::scale_mesh(mesh, scale_val) |>
        rayvertex::translate_mesh(c(0,0,offset_roof))
    }
    return(mesh)
  }
}
