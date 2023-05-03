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
