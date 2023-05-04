#' Generate Offset Polygon
#'
#' @param ss
#' @param offset
#'
#' @return
#' @export
#'
#' @examples
generate_roof = function(ss, max_height = 1, offset_roof = 0) {
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
  rayvertex::construct_mesh(vertices = as.matrix(xyz),
                            indices = as.matrix(indices_all)-1)
}

#' Generate Offset Polygon
#'
#' @param ss
#' @param offset
#'
#' @return
#' @export
#'
#' @examples
generate_roof_beveled = function(ss,
                                 bevel_offset,
                                 max_height = 1,
                                 offset_roof = 0) {
  if(bevel_offset >= max(ss$nodes[,4])) {
    message(sprintf("`bevel_offset` of %f greater than max offset in polygon of %f, calculating full roof model",
            bevel_offset,max(ss$nodes[,4])))
    return(generate_roof(ss, max_height = max_height, offset_roof = offset_roof))
  }
  beveled_ss = rayskeleton:::generate_offset_links_nodes(ss, bevel_offset)
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
  rayvertex::construct_mesh(vertices = as.matrix(xyz),
                            indices = as.matrix(indices_all)-1)
}

