#' Generate an offset polygon
#'
#' This function generates an interior offset polygon from a straight skeleton.
#'
#' @param skeleton Default `NULL`. A straight skeleton generated from the `skeletonize` function.
#' @param offset Default `NULL`. The offset(s) of the polygon.
#' @param progress Default `FALSE`. Whether to display a progress bar.
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
#' # Outer polygon
#' vertices = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)
#' # Holes inside the polygon
#' hole_1 = matrix(c(1,1, 2,1, 2,2, 1,2, 1,1), ncol = 2, byrow = TRUE)[5:1,]
#' hole_2 = matrix(c(5,5, 6,5, 6,6, 5,6, 5,5), ncol = 2, byrow = TRUE)[5:1,]
#' skeleton = skeletonize(vertices, holes = list(hole_1, hole_2))
#' plot_skeleton(skeleton)
#'
#' #Generate three offsets
#' plot_offset_polygon(generate_offset_polygon(skeleton, c(0.25,0.75,1.5,2)))
#'
#' #Generate three offsets, including the original polygon
#' plot_offset_polygon(generate_offset_polygon(skeleton, c(0.25,0.75,1.5,2)), skeleton = skeleton)
#'
#' #Generate many offsets
#' plot_offset_polygon(generate_offset_polygon(skeleton, seq(0,2.5,by=0.1)))
#'
#' # Skeletonize and plot an {sf} object
#' if(length(find.package("spData",quiet = TRUE)) > 0) {
#'   us_states = spData::us_states
#'   texas = us_states[us_states$NAME == "Texas",]
#'   texas_skeleton = skeletonize(texas)
#'   plot_offset_polygon(generate_offset_polygon(texas_skeleton, seq(0, 2.5, by = 0.1)),
#'                       border = heat.colors,
#'                       linewidth = 1)
#' }
generate_offset_polygon = function(skeleton, offset, progress = FALSE) {
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
  return(generate_offset_links_nodes(skeleton, offset, return_polys = TRUE, verbose = FALSE, progress = progress))
}
