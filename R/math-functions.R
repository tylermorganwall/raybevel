
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

#' Dot Product
#'
#' @param v1 Vector 1
#' @param v2 Vector 2
#'
#' @return Value
#' @keywords internal
#'
#' @examples
#' #None
dot = function(v1,v2) {
  sum(v1*v2)
}

#' Dot Product
#'
#' @param v1 Vector 1
#' @param v2 Vector 2
#'
#' @return Value
#' @keywords internal
#'
#' @examples
#' #None
unit_vector = function(v) {
  v/sqrt(sum(v*v))
}


#' Title
#'
#' @param node_start
#' @param node_end
#' @param height_start
#' @param height_end
#' @param height
#'
#' @return
#' @export
#'
#' @examples
interpolate_location = function(node_start,
                                node_end,
                                height_start,
                                height_end,
                                height) {
  t = (height - height_start)/(height_end - height_start)
  return(node_start * (1 - t) + node_end * t)
}
