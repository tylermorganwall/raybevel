#' Generate a 3D roof model
#'
#' This function generates a 3D roof model from a straight skeleton.
#'
#' @param ss Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param max_height Default `1`. The maximum height of the roof.
#' @param offset_roof Default `0`. The offset of the roof.
#' @param bottom Default `FALSE`. A logical flag that controls whether to generate the bottom of the roof.
#' @param swap_yz Default `FALSE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh.
#' If `TRUE`, the y and z coordinates will be swapped.
#' @param progress Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#'
#' @return A 3D mesh of the roof model.
#'
#' @export
generate_roof = function(ss, max_height = 1, offset_roof = 0, bottom = FALSE,
                         swap_yz = FALSE, progress = TRUE) {
  if(inherits(ss, "rayskeleton_list")) {
    pb = progress::progress_bar$new(
      format = ":current/:total generating roof [:bar] eta: :eta",
      total = length(ss), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(offset_roof) == 1) {
      offset_roof = rep(offset_roof,length(ss))
    } else {
      stopifnot(length(offset_roof) == length(ss))
    }
    if(length(max_height) == 1) {
      max_height = rep(max_height,length(ss))
    } else {
      stopifnot(length(max_height) == length(ss))
    }
    for(j in seq_len(length(ss))) {
      if(progress) {
        pb$tick()
      }
      meshlist[[counter]] = generate_roof(ss[[j]], bottom = bottom,
                                          max_height = max_height[j],
                                          offset_roof = offset_roof[j],
                                          swap_yz=swap_yz)
      counter = counter + 1
    }
    return(rayvertex::scene_from_list(meshlist))
  }
  if(!inherits(ss, "rayskeleton")) {
    stop("`ss` must be of class `rayskeleton`")
  }
  polygon_ind = convert_ss_to_polygons(ss)
  nodes = ss$nodes
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
  original_verts = attr(ss,"original_vertices")
  original_holes = attr(ss,"original_holes")
  if(!swap_yz) {
    col_order = 1:3
    ind_order = 1:3
  } else {
    col_order = c(1,3,2)
    ind_order = 3:1
  }
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
    mesh = rayvertex::construct_mesh(vertices = as.matrix(xyz)[,col_order],
                                     indices = as.matrix(indices_all)[,ind_order]-1)
    max_z = rayvertex::get_mesh_bbox(mesh)[2,3]
    # offset_roof_full = c(-0.5,-0.5,offset_roof)[col_order]
    offset_roof_full = c(0,0,offset_roof)[col_order]

    scale_max = c(1,1,max_height/max_z)[col_order]

    mesh = rayvertex::scale_mesh(mesh, scale = scale_max)
    return(rayvertex::translate_mesh(mesh, offset_roof_full))
  } else {
    mesh = rayvertex::construct_mesh(vertices = as.matrix(xyz)[,col_order],
                                     indices = as.matrix(indices_all)[,ind_order]-1)
    max_z = rayvertex::get_mesh_bbox(mesh)[2,3]
    offset_roof_full = c(0,0,offset_roof)[col_order]
    # offset_roof_full = c(-0.5,0.5,offset_roof)[col_order]

    scale_max = c(1,1,max_height/max_z)[col_order]

    mesh = rayvertex::scale_mesh(mesh, scale = scale_max)
    return(rayvertex::translate_mesh(mesh, offset_roof_full))
  }
}

#' Generate a beveled 3D roof model
#'
#' This function generates a beveled 3D roof model from a straight skeleton.
#'
#' @param ss Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param bevel_offsets Default `NULL`. The offset(s) of the bevel.
#' @param max_height Default `1`. The maximum height of the roof.
#' @param bevel_heights Default is set to `bevel_offsets`. Numeric vector specifying the heights of the bevels. Must be of the same length as `bevel_offsets`.
#' @param set_max_height Default `FALSE`. A logical flag that controls whether to set the max height of the roof based on the `max_height` argument.
#' @param swap_yz Default `FALSE`. A logical flag that controls whether to swap the y and z coordinates in the resulting mesh.
#' If `TRUE`, the y and z coordinates will be swapped.
#' @param progress Default `TRUE`. A logical flag to control whether a progress bar is displayed during roof generation.
#' @param offset_roof Default `0`. The offset of the roof.
#' @param bottom Default `FALSE`. A logical flag that controls whether to generate the bottom of the roof.
#'
#' @return A 3D mesh of the beveled roof model.
#'
#' @export
generate_roof_beveled = function(ss,
                                 bevel_offsets,
                                 bevel_heights = bevel_offsets,
                                 set_max_height = FALSE,
                                 max_height = 1,
                                 offset_roof = 0,
                                 bottom = FALSE,
                                 swap_yz = FALSE,
                                 progress = TRUE) {

  if(inherits(ss, "rayskeleton_list")) {
    pb = progress::progress_bar$new(
      format = ":current/:total generating roof [:bar] eta: :eta",
      total = length(ss), clear = TRUE, width = 60)
    meshlist = list()
    counter  = 1
    if(length(offset_roof) == 1) {
      offset_roof = rep(offset_roof,length(ss))
    }
    if(length(max_height) == 1) {
      max_height = rep(max_height,length(ss))
    }
    for(j in seq_len(length(ss))) {
      if(progress) {
        pb$tick()
      }
      meshlist[[counter]] = generate_roof_beveled(ss[[j]], bevel_offsets = bevel_offsets,
                                                  bottom = bottom, offset_roof = offset_roof[j],
                                                  max_height = max_height[j],
                                                  swap_yz=swap_yz)
      counter = counter + 1
    }
    return(rayvertex::scene_from_list(meshlist))
  }
  if(!inherits(ss, "rayskeleton")) {
    stop("`ss` must be of class `rayskeleton`")
  }
  max_time = max(ss$links$destination_time)
  if(all(bevel_offsets >= max_time)) {
    message(sprintf("All `bevel_offset` greater than max offset in polygon of %f, calculating full roof model",
            max(ss$nodes[,4])))
    return(generate_roof(ss, max_height = max_height, offset_roof = offset_roof,
                         swap_yz=swap_yz))
  }
  stopifnot(length(offset_roof) == 1)

  valid_bevels = bevel_offsets < max_time & bevel_offsets > 0
  bevel_offsets = bevel_offsets[valid_bevels]
  bevel_heights = bevel_heights[valid_bevels]
  stopifnot(length(bevel_offsets) == length(bevel_heights))

  beveled_ss = generate_offset_links_nodes(ss, bevel_offsets)
  polygon_ind = convert_ss_to_polygons(beveled_ss)
  nodes = beveled_ss$nodes
  index_list = list()
  for(i in seq_len(length(polygon_ind))) {
    tmp_ind = polygon_ind[[i]]
    tmp_poly = nodes[tmp_ind,2:3]
    index_list[[i]] = matrix(tmp_ind[decido::earcut(tmp_poly)],byrow=TRUE,ncol=3)
  }
  indices_all = do.call("rbind",index_list)
  xyz = nodes[,2:4]
  new_xyz = xyz
  bevel_offsets_with_max = c(bevel_offsets, max_time)
  for(i in seq_len(length(bevel_offsets_with_max)-1)) {
    flat_areas = xyz[,3] >= bevel_offsets_with_max[i]
    new_xyz[flat_areas,3] = bevel_heights[i]
  }
  xyz = new_xyz
  colnames(xyz) = c("x","y","z")
  if(set_max_height) {
    xyz[,3] = xyz[,3]/max(xyz[,3])*max_height + offset_roof
  } else {
    xyz[,3] = xyz[,3] + offset_roof
  }
  original_verts = attr(ss,"original_vertices")
  original_holes = attr(ss,"original_holes")
  if(!swap_yz) {
    col_order = 1:3
    ind_order = 1:3
  } else {
    col_order = c(1,3,2)
    ind_order = 3:1
  }
  if(bottom) {
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
    return(rayvertex::construct_mesh(vertices = as.matrix(xyz)[,col_order],
                                     indices = as.matrix(indices_all)[,ind_order]-1))
  } else {
    return(rayvertex::construct_mesh(vertices = as.matrix(xyz)[,col_order],
                                     indices = as.matrix(indices_all)[,ind_order]-1))
  }
}

