
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
#'
#' @return Value
#' @keywords internal
#'
#' @examples
#' #None
unit_vector = function(v) {
  v/sqrt(sum(v*v))
}


#' Interpolate Location
#'
#' @param node_start Length-2 Numeric x/y position
#' @param node_end   Length-2 Numeric x/y position
#' @param height_start Length-1 Numeric height at start node
#' @param height_end   Length-1 Numeric height at end node
#' @param height Length-1 Numeric height at which to interpolate
#'
#' @keywords internal
interpolate_location = function(node_start,
                                node_end,
                                height_start,
                                height_end,
                                height) {
  if((height_end - height_start) == 0) {
    return(node_end)
  }
  if(height == height_end) {
    return(node_end)
  }
  if(height == height_start) {
    return(node_start)
  }
  t = (height - height_start)/(height_end - height_start)
  return(node_start * (1 - t) + node_end * t)
}
