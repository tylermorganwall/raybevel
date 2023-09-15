#' Generate an offset polygon
#'
#' This function generates an interior offset polygon from a straight skeleton.
#'
#' @param skeleton Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param offset Default `NULL`. The offset(s) of the polygon.
#'
#' @return A list of data frames, each representing a polygon offset by the specified amount.
#'
#' @export
#' @examples
#' # Simple polygon example
#' simple_poly = matrix(c(0,0, 3,0, 3,3, 0,3, 0,0), ncol=2, byrow=TRUE)
#' skeleton = skeletonize(simple_poly)
#' offset_polys = generate_offset_polygon(skeleton, c(0.25, 0.5))
#' print(offset_polys)
#'
#' # Polygon with hole example
#' vertices4 = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)
#' # Holes inside the polygon
#' hole4_1 = matrix(c(1,1, 2,1, 2,2, 1,2, 1,1), ncol = 2, byrow = TRUE)[5:1,]
#' hole4_2 = matrix(c(5,5, 6,5, 6,6, 5,6, 5,5), ncol = 2, byrow = TRUE)[5:1,]
#' skeleton4 = skeletonize(vertices4, holes = list(hole4_1, hole4_2))
#' plot_skeleton(skeleton4)
#' plot_offset_polygon(generate_offset_polygon(skeleton4, c(0.25, 0.5)))
generate_offset_polygon = function(skeleton, offset) {
  if(length(offset) > 1) {
    if(any(offset > max(skeleton$nodes$time))) {
      warning("Removing offsets greater than maximum straight skeleton distance (%f)", max(skeleton$nodes$time))
    }
    offset = offset[offset < max(skeleton$nodes$time)]
    poly_list = list()
    for(i in seq_len(length(offset))) {
      poly_list[[i]] = generate_offset_polygon(skeleton,offset[i])
      names(poly_list[[i]]) = sprintf("%s_off%i",names(poly_list[[i]]),i)
    }
    return(unlist(poly_list,recursive = FALSE))
  }
  return(generate_offset_links_nodes(skeleton, offset, return_polys = TRUE))
}
