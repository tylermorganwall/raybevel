#' Title
#'
#' @param vertices DUM
#' @param holes DEE
#' @param tol DOO
#'
#' @return Value
#' @export
#'
#' @examples
#' #Value
skeletonize = function(vertices, holes, tol = 1e-10) {
  stopifnot(vertices[1,] == vertices[nrow(vertices),])
  stopifnot(ncol(vertices) == 2)
  vertices_pad = rbind(vertices[nrow(vertices)-1,], vertices)
  remove_verts = list()


  total_sum_det = 0
  #Remove vertices that come in straight lines
  for(i in seq_len(nrow(vertices_pad)-1)[-1]) {
    v1 = vertices_pad[i,] - vertices_pad[i-1,]
    v2 = vertices_pad[i+1,] - vertices_pad[i,]
    tmp_det = determinant2x2(v1,v2)
    if(abs(tmp_det) < tol) {
      remove_verts[[i]] = i
    }
    total_sum_det = total_sum_det + tmp_det
  }
  if(total_sum_det < 0) {
    stop("`vertices` is not CCW polygon")
  }
  verts_to_remove = c(1,unlist(remove_verts))
  vertices = vertices_pad[-verts_to_remove,]
  return_data = skeletonize_rcpp(vertices, holes, tol)
  ss = return_data$ss

  dp = as.data.frame(return_data$dp)
  lavs = as.data.frame(return_data$lavs)
  debug = as.data.frame(return_data$int_debug_data)
  split_debug = as.data.frame(return_data$split_debug_data)


  names(dp) = c("x","y","vx","vy","id","reflex","split")
  names(lavs) = c("x","y","lav","counter","valid")
  names(debug) = c("o1x","o1y","d1x","d1y",
                   "o2x","o2y","d2x","d2y",
                   "intx","inty","valid", "id", "edge_id")
  names(split_debug) = c("p1x","p1y","d1x","d1y",
                         "p3x","p3y","bx","by",
                         "left","right","edge","id", "edge_id",
                         "vx","vy","d2x","d2y","bpointx","bpointy")

  attr(ss,"dp") = dp
  attr(ss,"lavs") = lavs
  attr(ss,"debug") = debug
  attr(ss,"split_debug") = split_debug

  ss
}

#' Title
#'
#' @param v1 Vector 1
#' @param v2 Vector 2
#'
#' @return Value
#' @keywords internal
#'
#' @examples
#' #None
determinant2x2 = function(v1,v2) {
  det(matrix(c(v1,v2), ncol=2,nrow=2))
}
