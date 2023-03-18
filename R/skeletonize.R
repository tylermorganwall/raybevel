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

  #Remove vertices that come in straight lines
  for(i in seq_len(nrow(vertices_pad)-1)[-1]) {
    v1 = vertices_pad[i,] - vertices_pad[i-1,]
    v2 = vertices_pad[i+1,] - vertices_pad[i,]

    if(abs(determinant2x2(v1,v2)) < tol) {
      remove_verts[[i]] = i
    }
  }
  verts_to_remove = c(1,unlist(remove_verts))
  vertices = vertices_pad[-verts_to_remove,]
  skeletonize_rcpp(vertices, holes, tol)
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
