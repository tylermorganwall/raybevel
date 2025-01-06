#' Plot the Straight Skeleton of a Polygon
#'
#' This function visualizes the straight skeleton derived from a given polygon.
#' The original polygon (with holes if present) is plotted in black, while the
#' straight skeleton is plotted in red.
#'
#' @param skeleton A list object of class 'rayskeleton' containing the straight
#'        skeleton details. It should have 'nodes' and 'links' as its primary components.
#' @param use_arrow Default `TRUE`. A logical value indicating whether or not to use arrows
#'        to represent the links of the straight skeleton. Default is TRUE.
#' @param use_points Default `TRUE`. Whether to plot the vertex points as well.
#' @param xlim Default`c(0,1)`. A numeric vector of length 2 specifying the x-limits of the
#'        plot in the form `c(min, max)`. These are proportional limits relative
#'        to the bounding box around the skeleton.
#' @param ylim  Default `c(0,1)`. A numeric vector of length 2 specifying the y-limits of the
#'        plot in the form c(min, max). These are proportional limits relative
#'        to the bounding box around the skeleton.
#' @param arrow_color Default `"red"`. Color of the arrows.
#' @param polygon_color Default `"black"`. Color of the polygon.
#' @param size Default `1`. Size of the vertex points.
#' @param arrow_size Default `1`. Scales the arrow size.
#' @param highlight_links Default `NULL`. A numeric vector indicating which links
#'        (by their index) to highlight. If specified, the corresponding links will
#'        be colored with the `highlight_color`.
#' @param highlight_color Default `"purple"`. Color of the highlighted links.
#' @param return_layers Default `FALSE`, plots the figure. If `TRUE`, this will instead
#' return a list of the ggplot layers.
#'
#' @details The function uses the `ggplot2` package for plotting. The straight skeleton
#'          is visualized based on the details provided in the `skeleton` object.
#'          The original polygon and holes are plotted based on attributes stored
#'          in the `skeleton` object.
#'
#' @return A ggplot object visualizing the straight skeleton and the original polygon.
#' @export
#' @examples
#' # Assuming skeleton1 is already defined as in the previous example
#' # Outer polygon
#' vertices = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)
#' # Holes inside the polygon
#' hole1 = matrix(c(1,1, 1,2, 2,2, 2,1, 1,1), ncol = 2, byrow = TRUE)
#' hole2 = matrix(c(5,5, 5,6, 6,6, 6,5, 5,5), ncol = 2, byrow = TRUE)
#' skeleton = skeletonize(vertices, holes = list(hole1, hole2))
#' if(run_docs_raybevel()) {
#'   plot_skeleton(skeleton)
#' }
#' # Skeletonize and plot an {sf} object
#' if(run_docs_raybevel()) {
#'   us_states = spData::us_states
#'   texas = us_states[us_states$NAME == "Texas",]
#'   plot_skeleton(skeletonize(texas))
#' }
#' # Highlighting certain links in the skeleton
#' max_links =which(skeleton$links$destination_time == max(skeleton$links$destination_time))
#' if(run_docs_raybevel()) {
#'   plot_skeleton(skeleton, highlight_links = max_links, highlight_color = "green")
#' }
plot_skeleton = function(skeleton, use_arrow = TRUE, use_points = TRUE, xlim = c(0,1), ylim = c(0,1),
                         arrow_color = "red", polygon_color = "black", size = 1,
                         arrow_size = 0.05, highlight_links = NULL, highlight_color = "green",
                         return_layers = FALSE) {
  if(!(length(find.package("ggplot2", quiet = TRUE)) > 0)) {
    stop("{ggplot2} package required for plot_skeleton()")
  }
  is_highlighted = mid_x = mid_y = x = y = xend = yend = NULL

  # Check if skeleton is valid
  if (!inherits(skeleton, c("rayskeleton","rayskeleton_polygon", "rayskeleton_list"))) {
    stop(sprintf("Invalid input: The input is not of class: '%s'",
                 paste0(c("rayskeleton","rayskeleton_polygon", "rayskeleton_list"), collapse=", ")))
  }

  #Import external functions from suggested packages
  aes = ggplot2::aes
  ggplot = ggplot2::ggplot
  theme_void = ggplot2::theme_void
  geom_point = ggplot2::geom_point
  geom_segment = ggplot2::geom_segment
  geom_point = ggplot2::geom_point
  scale_color_manual = ggplot2::scale_color_manual
  geom_polygon = ggplot2::geom_polygon
  theme = ggplot2::theme
  coord_fixed = ggplot2::coord_fixed

  # Create base ggplot object
  # p = ggplot() + theme_void()
  p = list()
  if(!inherits(skeleton, "rayskeleton_list")) {
    skeleton_all = list(skeleton)
  } else {
    skeleton_all = skeleton
  }
  final_links_list = list()
  final_nodes_list = list()
  for(i in seq_along(skeleton_all)) {
    skeleton = skeleton_all[[i]]
    final_nodes_list[[i]] = skeleton$nodes

    skeleton$links$mid_x = (skeleton$nodes[skeleton$links$source, 'x'] + skeleton$nodes[skeleton$links$destination, 'x']) / 2
    skeleton$links$mid_y = (skeleton$nodes[skeleton$links$source, 'y'] + skeleton$nodes[skeleton$links$destination, 'y']) / 2
    skeleton$links$x = skeleton$nodes[skeleton$links$source, 'x']
    skeleton$links$y = skeleton$nodes[skeleton$links$source, 'y']
    skeleton$links$xend = skeleton$nodes[skeleton$links$destination, 'x']
    skeleton$links$yend = skeleton$nodes[skeleton$links$destination, 'y']
    skeleton$links$is_highlighted <- ifelse(seq_len(nrow(skeleton$links)) %in% highlight_links, TRUE, FALSE)
    final_links_list[[i]] = skeleton$links
  }
  final_links = do.call("rbind", final_links_list)
  final_nodes = do.call("rbind", final_nodes_list)

  # Plot skeleton nodes
  if(use_points) {
    p[[length(p) + 1]] = geom_point(data = final_nodes,
                       aes(x = x, y = y),
                       color = polygon_color, size = size)
  }

  # Plot skeleton links (arrows or segments)
  if (use_arrow) {
    p[[length(p) + 1]] = geom_segment(data = final_links,
                          aes(x = x,
                              y = y,
                              xend = mid_x,
                              yend = mid_y,
                              color = is_highlighted),
                          arrow = grid::arrow(type = "closed", length = grid::unit(arrow_size, "inches")))
    p[[length(p) + 1]] =   geom_segment(data = final_links,
                 aes(x = mid_x,
                     y = mid_y,
                     xend = xend,
                     yend = yend,
                     color = is_highlighted))
  } else {
    p[[length(p) + 1]] = geom_segment(data = final_links,
                         aes(x = x,
                             y = y,
                             xend = xend,
                             yend = yend,
                             color = is_highlighted))
  }
  p[[length(p) + 1]] = scale_color_manual(values=c(arrow_color, highlight_color))
  p[[length(p) + 1]] =   theme(legend.position = "none")

  # Plot original polygon
  for(i in seq_along(skeleton_all)) {
    skeleton = skeleton_all[[i]]
    original_vertices = attr(skeleton, "original_vertices")
    colnames(original_vertices) = c("x","y")
    p[[length(p) + 1]] = geom_polygon(data = data.frame(original_vertices),
                          aes(x = x, y = y),
                          color = polygon_color, fill = NA)

    # Plot holes if any
    original_holes = attr(skeleton, "original_holes")
    if (length(original_holes) > 0) {
      for (hole in original_holes) {
        hole = data.frame(hole)
        colnames(hole) = c("x","y")

        p[[length(p) + 1]] = geom_polygon(data = hole,
                              aes(x = x, y = y),
                              color = polygon_color, fill = NA)
      }
    }
  }
  if(!return_layers) {
    # Adjust plot limits and maintain aspect ratio
    p[[length(p) + 1]] = coord_fixed(ratio = 1)
    p[[length(p) + 1]] = theme_void()
    p[[length(p) + 1]] = theme(legend.position = "none")

    return(ggplot() + p)

  } else {
    return(p)
  }
}
