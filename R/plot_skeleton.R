#' Plot the Straight Skeleton of a Polygon
#'
#' This function visualizes the straight skeleton derived from a given polygon.
#' The original polygon (with holes if present) is plotted in black, while the
#' straight skeleton is plotted in red.
#'
#' @param skeleton A list object of class 'rayskeleton' containing the straight
#'        skeleton details. It should have 'nodes' and 'links' as its primary components.
#'
#' @details The function uses base R plotting capabilities. The original graphics
#'          parameters (`par`) are restored after plotting to ensure no unintended
#'          side-effects.
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
plot_skeleton = function(skeleton,use_arrow = TRUE) {
  # Check if skeleton is valid
  if (!inherits(skeleton, "rayskeleton")) {
    stop("Invalid input: The input is not of class 'rayskeleton'")
  }

  # Save current par settings
  # opar = par(no.readonly = TRUE)
  # on.exit(par(opar))

  # Plot settings
  par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(0,0,0,0))
  plot(0, 0, type="n", xlim=c(min(skeleton$nodes$x) - 1, max(skeleton$nodes$x) + 1),
       ylim=c(min(skeleton$nodes$y) - 1, max(skeleton$nodes$y) + 1), xlab="", ylab="",
       xaxt='n', yaxt='n', frame.plot = FALSE)
  points(x=skeleton$nodes$x, y=skeleton$nodes$y, pch=1, col="black")
  range_skeleton_x = range(skeleton$nodes$x)
  range_skeleton_y = range(skeleton$nodes$y)

  min_range = min(c(range_skeleton_x[2]-range_skeleton_x[1],range_skeleton_y[2]-range_skeleton_y[1]))
  arrow_size = min_range/100
  # Plot straight skeleton
  for (i in 1:nrow(skeleton$links)) {
    if(use_arrow) {
      x0=skeleton$nodes[skeleton$links$source[i], 'x']
      x1=skeleton$nodes[skeleton$links$destination[i], 'x']
      y0=skeleton$nodes[skeleton$links$source[i], 'y']
      y1=skeleton$nodes[skeleton$links$destination[i], 'y']
      interp_pos = (c(x0,y0) + c(x1,y1))/2
      arrows(x0=x0,
             x1=interp_pos[1],
             y0=y0,
             y1=interp_pos[2],
            col="red", lwd=2,length = arrow_size, angle = 20)
      segments(x1=x1,
               x0=interp_pos[1],
               y1=y1,
               y0=interp_pos[2],
               col="red", lwd=2)
    } else {
      lines(c(skeleton$nodes[skeleton$links$source[i], 'x'], skeleton$nodes[skeleton$links$destination[i], 'x']),
             c(skeleton$nodes[skeleton$links$source[i], 'y'], skeleton$nodes[skeleton$links$destination[i], 'y']),
             col="red", lwd=2)
    }
  }

  original_holes = attr(skeleton, "original_holes")

  # Plot holes if any
  if (length(original_holes) > 0) {
    for (hole in original_holes) {
      polygon(hole[, 1], hole[, 2], col=NA, lwd=2, border="black")
    }
  }

  original_vertices = attr(skeleton, "original_vertices")

  # Plot original polygon
  polygon(original_vertices[, 1], original_vertices[, 2], border="black", lwd=2, col=NA)
}
