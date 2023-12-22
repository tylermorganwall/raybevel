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
  offset = offset[order(offset)]
  if(inherits(skeleton, "rayskeleton_list")) {
    is_list = TRUE
    max_internal_distance = max(unlist(lapply(skeleton, \(x) max(x$nodes$time))))
  } else {
    is_list = FALSE
    max_internal_distance =  max(skeleton$nodes$time)
  }
  offset = offset[offset < max_internal_distance]
  if(length(offset) == 0) {
    stop(sprintf("offsets are all greater than maximum internal distance (%f)",max_internal_distance))
  }
  poly_list = list()
  if(is_list) {
    for(j in seq_len(length(skeleton))) {
      poly_list_skeleton = list()
      for(i in seq_len(length(offset))) {
        poly_list_skeleton[[i]] = generate_offset_links_nodes(skeleton[[j]], offset[i], return_polys = TRUE,
                                                              verbose = FALSE, progress = progress)
        class(poly_list_skeleton[[i]]) = c("rayskeleton_offset_polygons", "list")
        num_polygons = length(poly_list_skeleton[[i]])
        attr(poly_list_skeleton[[i]], "number_polygons") = num_polygons
        offset_len = seq_len(length(num_polygons))
        names(poly_list_skeleton[[i]]) = sprintf("%s_off%i",offset_len,i)
      }
      class(poly_list_skeleton) = c("rayskeleton_offset_polygons_collection", "list")
      attr(poly_list_skeleton, "skeleton") = skeleton[[j]]
      attr(poly_list_skeleton, "number_offsets") = length(offset)
      attr(poly_list_skeleton, "original_vertices") = attr(skeleton[[j]], "original_vertices")
      attr(poly_list_skeleton, "original_holes") = attr(skeleton[[j]], "original_holes")
      poly_list[[j]] = poly_list_skeleton
    }
  } else {
    poly_list_skeleton = list()
    for(i in seq_len(length(offset))) {
      poly_list_skeleton[[i]] = generate_offset_links_nodes(skeleton, offset[i], return_polys = TRUE,
                                                   verbose = FALSE, progress = progress)
      class(poly_list_skeleton[[i]]) = c("rayskeleton_offset_polygons", "list")
      num_polygons = length(poly_list_skeleton[[i]])
      attr(poly_list_skeleton[[i]], "number_polygons") = num_polygons
      offset_len = seq_len(length(poly_list_skeleton[[i]]))
      names(poly_list_skeleton[[i]]) = sprintf("offset_%f_%i",offset[i],i)
    }
    class(poly_list_skeleton) = c("rayskeleton_offset_polygons_collection", "list")
    attr(poly_list_skeleton, "skeleton") = skeleton
    attr(poly_list_skeleton, "number_offsets") = length(offset)
    attr(poly_list_skeleton, "original_vertices") = attr(skeleton, "original_vertices")
    attr(poly_list_skeleton, "original_holes") = attr(skeleton, "original_holes")
    poly_list[[1]] = poly_list_skeleton
  }
  class(poly_list) = c("rayskeleton_polygons", "list")
  return(poly_list)
}
