#' Generate 2D Bevel Profile for 3D Polygons
#'
#' @param bevel_type Character `NULL`. Type of the bevel, one of the following options:
#'   - "circular": Creates a rounded bevel resembling a quarter-circle.
#'   - "exp": Creates an exponential curve, starting slow and accelerating.
#'   - "bump": Creates a bump-like profile, rising and falling within the coverage.
#'   - "step": Generates a step-like bevel with a flat top.
#'   - "block": Generates a block-like bevel, jumping straight to the max_height and back to the base.
#'   - "angled": Generates a straight angled bevel. You can optionally set the 'angle' parameter for this bevel.
#' @param percent_coverage Numeric `0.1`. Percentage of the curve to be covered by the bevel.
#' @param max_height Numeric `0.1`. The maximum height of the bevel, as a percentage of the final height.
#' @param angle Numeric `NULL`. Optional angle parameter in degrees for angular bevels.
#' @param curve_points Numeric `50`. Number of points to plot for curve-based bevels.
#' @param reverse Logical `FALSE`. If TRUE, the curve is reversed.
#' @param initial_height Numeric `0`. The initial height from which the bevel starts. The bevel is rescaled to fit within the range from initial_height to max_height.
#'
#' @return List containing 'x' and 'y', which are the coordinates of the 2D bevel profile.
#'
#' @export
#' @examples
#' #Generate a single bevel profile and plot it
#' coords = generate_bevel("circular", 0.2, 0.2)
#' plot(coords$x, coords$y, type = 'l', axes = FALSE, mar = c(0, 0, 0, 0))
#' title(main = "Bevel Type: Circular")
#' box()
#'
#' # Plot all bevel profiles in a grid
#' par(mfrow = c(4, 3),mai = c(0.2, 0.2, 0.5, 0.2))
#' types = rep(c("circular", "exp", "bump", "step", "block", "angled"), 2)
#' rev = c(rep(FALSE, 6), rep(TRUE, 6))
#' for(i in seq_len(length(types))) {
#'   coords = generate_bevel(types[i], 0.5, 0.5, 45, reverse = rev[i])
#'   plot(coords$x, coords$y, type = 'l', main = paste("Bevel Type:", types[i]), axes = FALSE,
#'        xlim = c(-0.1, 1.1), ylim = c(-0.2, 0.8), mar = c(0, 0, 0, 0),xlab = "", ylab = "")
#'   box()
#' }
generate_bevel = function(bevel_type = "angled", percent_coverage = 0.1,
                          max_height = 0.1, angle = 45, curve_points = 50,
                          reverse = FALSE, initial_height = 0) {
  if(bevel_type == "angled") {
    stopifnot(angle < 90 && angle > 0)
    angle = angle * pi / 180  # Convert to radians
    max_height = tan(angle) * percent_coverage
  }

  if (bevel_type == "step") {
    x = c(0, percent_coverage - 0.0001, percent_coverage, 1)
    y = c(0, 0, max_height, max_height)
  } else if (bevel_type == "block") {
    x = c(0, 0.0001, percent_coverage - 0.0001, percent_coverage, 1)
    y = c(0, max_height, max_height, 0, 0)
  } else if(bevel_type == "angled") {
    x = c(0, percent_coverage, 1)
    y = c(0, tan(angle) * percent_coverage, tan(angle) * percent_coverage)
  } else {
    x_curve = seq(0, percent_coverage, length.out = curve_points)
    if(bevel_type == "circular") {
      y_curve = c(sqrt(1 - (x_curve / percent_coverage - 1)^2),1)
    } else if(bevel_type == "exp") {
      y_curve = c(1 - exp(-5 * x_curve / percent_coverage),1)
    } else if(bevel_type == "bump") {
      y_curve = c(sqrt(1 - (2 * x_curve / percent_coverage - 1)^2),0)
    } else {
      stop(sprintf("bevel_type `%s` not recognized.", bevel_type))
    }
    y_curve = y_curve * max_height
    x = c(x_curve, 1)
    y = y_curve
  }
  if(reverse) {
    y = max_height - y
  }
  y = initial_height + (y - min(y)) * (max_height - initial_height) / (max(y) - min(y))

  return(list(x = x, y = y))
}

#' Generate Complex 2D Bevel Profile for 3D Polygons
#'
#' @param bevel_types Vector of character. Types of the bevels. Options are: "circular", "exp", "bump", "step", "block", "angled".
#' @param percent_coverages Vector of numeric. Percentages of the curve to be covered by each bevel.
#' @param max_heights Vector of numeric. The maximum heights of each bevel as a percentage of the final height.
#' @param angles Vector of numeric `NULL`. Optional angle parameter in degrees for angular bevels.
#' @param curve_points Vector of numeric `50`. Number of points for each curve.
#' @param reverses Vector of logical `FALSE`. Whether to reverse each bevel.
#'
#' @return List containing 'x' and 'y', which are the coordinates of the complex 2D bevel profile
#' @export
#' @examples
#' # Generate a complex bevel profile and plot it
#' complex_coords = generate_complex_bevel(
#'   c("circular", "exp", "step"),
#'   c(0.2, 0.5, 0.8),
#'   c(0.1, 0.2, 0.3),
#'   angles = c(NULL, NULL, NULL),
#'   curve_points = c(50, 50, 50),
#'   reverses = c(TRUE, FALSE, FALSE)
#' )
#' plot(complex_coords$x, complex_coords$y, type = 'l', axes = FALSE,
#'      main = "Complex Bevel")
#' box()
generate_complex_bevel = function(bevel_types, percent_coverages,
                                  max_heights = NULL,
                                  angles = NULL, curve_points = rep(50, length(bevel_types)),
                                  reverses = rep(FALSE, length(bevel_types))) {
  if(length(bevel_types) != length(percent_coverages) ||
     # length(bevel_types) != length(max_heights) ||
     length(bevel_types) != length(curve_points) ||
     length(bevel_types) != length(reverses)) {
    stop("All vectors must have the same length.")
  }

  # Sort all vectors based on percent_coverages
  order_indices = order(percent_coverages)
  bevel_types = bevel_types[order_indices]
  percent_coverages = percent_coverages[order_indices]
  max_heights = max_heights[order_indices]
  curve_points = curve_points[order_indices]
  reverses = reverses[order_indices]
  if (!is.null(angles)) angles = angles[order_indices]

  x = c()
  y = c()
  y_prev_end = 0

  for(i in seq_along(bevel_types)) {
    bevel_segment = generate_bevel(
      bevel_type = bevel_types[i],
      percent_coverage = percent_coverages[i],
      max_height = max_heights[i],
      angle = if (!is.null(angles)) angles[i] else NULL,
      curve_points = curve_points[i],
      reverse = reverses[i],
      initial_height = y_prev_end  # Pass the last height of the previous bevel as the initial height
    )

    y_prev_end = tail(bevel_segment$y, n = 1)  # Update y_prev_end for the next iteration

    if(i == 1) {
      x = bevel_segment$x
      y = bevel_segment$y
    } else {
      x = c(x, tail(bevel_segment$x, n = -1) + tail(x, n = 1))
      y = c(y, tail(bevel_segment$y, n = -1))
    }
  }
  x = x / max(x)

  return(list(x = x, y = y))
}
