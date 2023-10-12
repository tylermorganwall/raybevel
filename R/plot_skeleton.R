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
#' @param xlim Default`c(0,1)`. A numeric vector of length 2 specifying the x-limits of the
#'        plot in the form `c(min, max)`. These are proportional limits relative
#'        to the bounding box around the skeleton.
#' @param ylim  Default `c(0,1)`. A numeric vector of length 2 specifying the y-limits of the
#'        plot in the form c(min, max). These are proportional limits relative
#'        to the bounding box around the skeleton.
#' @param add Default `FALSE`. A logical indicating whether to add the straight skeleton to
#'        an existing plot (`TRUE`) or create a new one (`FALSE`).
#' @param arrow_color Default `"red"`. Color of the arrows.
#' @param polygon_color Default `"black"`. Color of the polygon.
#' @param size Default `1`. Size of the vertex points.
#' @param arrow_size Default `1`. Scales the arrow size.
#'
#' @details The function uses base R plotting capabilities. The original graphics
#'          parameters (`par`) are restored after plotting to ensure no unintended
#'          side-effects.
#'
#' @return A plot visualizing the straight skeleton and the original polygon.
#' @export
#' @examples
#' # Assuming skeleton1 is already defined as in the previous example
#' # Outer polygon
#' vertices = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)
#' # Holes inside the polygon
#' hole1 = matrix(c(1,1, 1,2, 2,2, 2,1, 1,1), ncol = 2, byrow = TRUE)
#' hole2 = matrix(c(5,5, 5,6, 6,6, 6,5, 5,5), ncol = 2, byrow = TRUE)
#' skeleton = skeletonize(vertices, holes = list(hole1, hole2))
#' plot_skeleton(skeleton)
#'
#' # Skeletonize and plot an {sf} object
#' if(length(find.package("spData",quiet = TRUE)) > 0) {
#'   us_states = spData::us_states
#'   texas = us_states[us_states$NAME == "Texas",]
#'   plot_skeleton(skeletonize(texas), arrow_size=0.5)
#' }
plot_skeleton = function(skeleton, use_arrow = TRUE, xlim = c(0,1), ylim = c(0,1), add = FALSE,
                         arrow_color = "red", polygon_color = "black", size = 1, arrow_size = 1, arrow_lwd = 1,
                         highlight_links = NULL, highlight_color = "purple") {
  # Check if skeleton is valid
  if (!inherits(skeleton, c("rayskeleton","rayskeleton_polygon"))) {
    stop("Invalid input: The input is not of class 'rayskeleton'")
  }

  # Save current par settings
  # opar = graphics::par(no.readonly = TRUE)
  # on.exit(graphics::par(opar))

  # Plot settings
  graphics::par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(0,0,0,0))
  stopifnot(length(xlim) == 2 && length(ylim) == 2)
  stopifnot(xlim[1] < xlim[2] && xlim[1] >= 0 && xlim[2] <= 1)
  stopifnot(ylim[1] < ylim[2] && ylim[1] >= 0 && ylim[2] <= 1)

  min_x = min(skeleton$nodes$x) - 1
  min_y = min(skeleton$nodes$y) - 1
  max_x = max(skeleton$nodes$x) + 1
  max_y = max(skeleton$nodes$y) + 1

  span_x = max_x - min_x
  span_y = max_y - min_y

  xlim_vals = c(min_x + span_x * xlim[1], min_x + span_x * xlim[2])
  ylim_vals = c(min_y + span_y * ylim[1], min_y + span_y * ylim[2])

  if (!add) {
    graphics::plot(0, 0, type="n", xlim=xlim_vals, ylim=ylim_vals, xlab="", ylab="",
                   xaxt='n', yaxt='n', frame.plot = FALSE, asp = 1)
  }
  graphics::points(x=skeleton$nodes$x, y=skeleton$nodes$y, pch=16, col=polygon_color,
                   cex = size)

  min_dev_size = min(grDevices::dev.size())
  arrow_size_full = min_dev_size/50 * arrow_size
  # Plot straight skeleton
  for (i in seq_len(nrow(skeleton$links))) {
    if(i %in% highlight_links) {
      arr_color = highlight_color
    } else {
      arr_color = arrow_color
    }
    if(use_arrow) {
      x0=skeleton$nodes[skeleton$links$source[i], 'x']
      x1=skeleton$nodes[skeleton$links$destination[i], 'x']
      y0=skeleton$nodes[skeleton$links$source[i], 'y']
      y1=skeleton$nodes[skeleton$links$destination[i], 'y']
      interp_pos = (c(x0,y0) + c(x1,y1))/2
      graphics::arrows(x0 = x0,
                       x1 = interp_pos[1],
                       y0 = y0,
                       y1 = interp_pos[2],
                       col = arr_color, lwd=arrow_lwd, length = arrow_size_full, angle = 20)
      graphics::segments(x1 = x1,
                         x0 = interp_pos[1],
                         y1 = y1,
                         y0 = interp_pos[2],
                         col = arr_color, lwd = arrow_lwd)
    } else {
      graphics::lines(c(skeleton$nodes[skeleton$links$source[i], 'x'], skeleton$nodes[skeleton$links$destination[i], 'x']),
                      c(skeleton$nodes[skeleton$links$source[i], 'y'], skeleton$nodes[skeleton$links$destination[i], 'y']),
                      col=arr_color, lwd=arrow_lwd)
    }
  }

  original_holes = attr(skeleton, "original_holes")

  # Plot holes if any
  if (length(original_holes) > 0) {
    for (hole in original_holes) {
      graphics::polygon(hole[, 1], hole[, 2], col=NA, lwd=arrow_lwd, border=polygon_color)
    }
  }

  original_vertices = attr(skeleton, "original_vertices")

  # Plot original polygon
  graphics::polygon(original_vertices[, 1], original_vertices[, 2], border=polygon_color, lwd=arrow_lwd, col=NA)
}
