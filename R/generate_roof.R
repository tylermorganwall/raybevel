#' Generate a 3D roof model
#'
#' This function generates a 3D roof model from a straight skeleton.
#'
#' @param ss Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param max_height Default `1`. The maximum height of the roof.
#' @param offset_roof Default `0`. The offset of the roof.
#' @param base Default `FALSE`. A logical flag that controls whether to generate the base of the roof.
#' @param bottom Default `FALSE`. A logical flag that controls whether to generate the bottom of the roof.
#'
#' @return A 3D mesh of the roof model.
#'
#' @export
generate_roof = function(ss, max_height = 1, offset_roof = 0, base = FALSE, bottom = FALSE,
                         swap_yz = FALSE) {
  if(inherits(ss, "rayskeleton_list")) {
    meshlist = list()
    counter  = 1
    for(j in seq_len(length(ss))) {
      meshlist[[counter]] = generate_roof(ss, bottom = bottom,
                                          base = base, max_height = max_height,
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
  xyz[,3] = xyz[,3]/max(xyz[,3])*max_height + offset_roof
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
    return(rayvertex::construct_mesh(vertices = as.matrix(xyz)[,col_order],,
                                     indices = as.matrix(indices_all)[,ind_order]-1))
  } else {
    return(rayvertex::construct_mesh(vertices = as.matrix(xyz)[,col_order],,
                                     indices = as.matrix(indices_all)[,ind_order]-1))
  }
}

#' Generate a beveled 3D roof model
#'
#' This function generates a beveled 3D roof model from a straight skeleton.
#'
#' @param ss Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param bevel_offset Default `NULL`. The offset of the bevel.
#' @param max_height Default `1`. The maximum height of the roof.
#' @param offset_roof Default `0`. The offset of the roof.
#' @param base Default `FALSE`. A logical flag that controls whether to generate the base of the roof.
#' @param bottom Default `FALSE`. A logical flag that controls whether to generate the bottom of the roof.
#'
#' @return A 3D mesh of the beveled roof model.
#'
#' @export
generate_roof_beveled = function(ss,
                                 bevel_offset,
                                 max_height = 1,
                                 offset_roof = 0,
                                 base = FALSE, bottom = FALSE,
                                 swap_yz = FALSE) {
  if(inherits(ss, "rayskeleton_list")) {
    meshlist = list()
    counter  = 1
    for(j in seq_len(length(ss))) {
      meshlist[[counter]] = generate_roof_beveled(ss, bevel_offset = bevel_offset,
                                                  bottom = bottom, offset_roof = offset_roof,
                                                  base = base, max_height = max_height,
                                                  swap_yz=swap_yz)
      counter = counter + 1
    }
    return(rayvertex::scene_from_list(meshlist))
  }
  if(!inherits(ss, "rayskeleton")) {
    stop("`ss` must be of class `rayskeleton`")
  }
  if(bevel_offset >= max(ss$links$destination_time)) {
    message(sprintf("`bevel_offset` of %f greater than max offset in polygon of %f, calculating full roof model",
            bevel_offset,max(ss$nodes[,4])))
    return(generate_roof(ss, max_height = max_height, offset_roof = offset_roof,
                         swap_yz=swap_yz))
  }
  beveled_ss = generate_offset_links_nodes(ss, bevel_offset)
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
  flat_areas = xyz[,3] > bevel_offset
  xyz[flat_areas,3] = bevel_offset
  colnames(xyz) = c("x","y","z")
  xyz[,3] = xyz[,3]/max(xyz[,3])*max_height + offset_roof
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

