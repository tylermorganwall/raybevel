#' Generate 2D Bevel Profile for 3D Polygons
#'
#' @param bevel_type Character `angled`. Type of the bevel, one of the following options:
#'   - "circular": Creates a rounded bevel resembling a quarter-circle.
#'   - "exp": Creates an exponential curve, starting slow and accelerating.
#'   - "bump": Creates a bump-like profile, rising and falling within the coverage.
#'   - "step": Generates a step-like bevel with a flat top.
#'   - "block": Generates a block-like bevel, jumping straight to the max_height and back to the base.
#'   - "angled": Generates a straight angled bevel. You can optionally set the 'angle' parameter for this bevel.
#' @param bevel_start Default `0`. The starting point of the bevel along the curve, ranges between 0 and 1.
#' @param bevel_end Default `0.2`. The ending point of the bevel along the curve, ranges between 0 and 1.
#' @param max_height Default `1`. The maximum height of the bevel, as measured from the initial height.
#' @param angle Default `NULL`. Optional angle parameter in degrees for angular bevels.
#' @param curve_points Default `50`. Number of points to plot for curve-based bevels.
#' @param reverse Default `FALSE`. If `TRUE`, the curve is reversed vertically.
#' @param flip Default `FALSE`. If `TRUE`, the curve is flipped horizontally.
#' @param initial_height Default `0`. The initial height from which the bevel starts. The bevel is rescaled to fit within the range from initial_height to max_height.
#' @param manual_offsets Default `NULL`, none. This will force the bevel to add a point (interpolating between the two nearest points)
#' at the specified offsets. This is useful when you want to add points at specific distances along the curve.
#' @param add_end_points Default `TRUE`. Whether to ensure there is a point at zero and a point at one.
#' @param step_epsilon Default `1e-5`. The size for the small percentage step when using a step bevel.
#' @param plot_bevel Default `FALSE`. Whether to plot the bevel.
#'
#' @return List containing 'x' and 'y', which are the coordinates of the 2D bevel profile.
#'
#' @export
#' @examples
#' # Generate a single bevel profile and plot it
#' coords = generate_bevel("circular", 0.2, 0.8, 0.2, plot_bevel = TRUE)
#'
#' # Plot all bevel profiles in a grid
#' par(mfrow = c(4, 3), mai = c(0.2, 0.2, 0.5, 0.2))
#' max_height = c(1,1,1,1)
#' types = rep(c("circular", "exp", "bump", "step", "block", "angled"),2)
#' reverses = c(rep(FALSE,6),rep(TRUE,6))
#' for(i in seq_len(length(types))) {
#'   coords = generate_bevel(types[i], 0.2, 0.8, 1, flip = TRUE,
#'                           angle = 45, reverse = reverses[i], plot_bevel = TRUE)
#' }
generate_bevel = function(bevel_type = "angled", bevel_start = 0, bevel_end = 0.2,
                          max_height = 1, angle = NULL, curve_points = 50,
                          reverse = FALSE, flip = FALSE,
                          initial_height = 0, add_end_points = TRUE,
                          manual_offsets = NULL,
                          step_epsilon = 1e-5,
                          plot_bevel = FALSE) {
  # Check angle constraint if bevel_type is angled
  if(bevel_type == "angled") {
    if(!is.null(angle)) {
      stopifnot(angle < 90 && angle > -90)
      angle = angle*pi/180
      max_height = tan(angle) * (bevel_end - bevel_start)
    }
  }

  # Initialize empty vectors for x and y
  x = c()
  y = c()
  # Define curve points based on bevel_type
  if(bevel_type == "step") {
    x = c(bevel_start, bevel_start + step_epsilon)
    y = c(0, max_height)
  } else if(bevel_type == "block") {
    x = c(bevel_start, bevel_start + step_epsilon, bevel_end - step_epsilon, bevel_end)
    y = c(0,max_height, max_height, 0)
  } else if(bevel_type == "angled") {
    x = c(bevel_start, bevel_end)
    y = c(0, max_height)
  } else {
    x_curve = seq(bevel_start, bevel_end, length.out = curve_points)
    if(bevel_type == "circular") {
      y_curve = sqrt(1 - ((x_curve - bevel_start) / (bevel_end - bevel_start) - 1)^2)
    } else if(bevel_type == "exp") {
      y_curve = 1 - exp(-5 * (x_curve - bevel_start) / (bevel_end - bevel_start))
    } else if(bevel_type == "bump") {
      y_curve = sqrt(1 - (2 * (x_curve - bevel_start) / (bevel_end - bevel_start) - 1)^2)
    } else {
      stop(sprintf("bevel_type `%s` not recognized.", bevel_type))
    }
    y_curve = y_curve * max_height
    x = c(x, x_curve)
    y = c(y, y_curve)
  }
  if(flip) {
    y = rev(y)
  }
  if(add_end_points) {
    if(x[1] != 0) {
      x = c(0,x)
      y = c(y[1],y)
    }
    if(x[length(x)] != 1) {
      x = c(x,1)
      last_val = y[length(y)]
      y = c(y,last_val)
    }
  }
  if(!is.null(manual_offsets)) {
    manual_offsets = manual_offsets[manual_offsets < 1 & manual_offsets > 0]
    for(j in seq_len(length(manual_offsets))) {
      if(any(abs(x-manual_offsets[j]) < 1e-14)) {
        manual_offsets[j] = NA
      }
    }
    manual_offsets = manual_offsets[!is.na(manual_offsets)]
    if(length(manual_offsets) > 0) {
      max_x = max(x)
      manual_vals = stats::approx(x=x,y=y, xout = manual_offsets)$y
      manual_vals[max_x < manual_offsets] = y[length(y)]
      x = c(x,manual_offsets)
      x_order = order(x)
      x = x[x_order]
      y = c(y,manual_vals)
      y = y[x_order]
    }
  }
  # Reverse the bevel if specified
  if(reverse) {
    y = max_height - y
  }
  # Adjust heights based on initial_height
  diff_height = y[1]-initial_height
  y = y - diff_height

  if(plot_bevel) {
    plot(x, y, type = 'l', axes = FALSE, lwd = 2, xlim = c(min(x)-0.1, max(x)+0.1), ylim = c(min(y)-0.1, max(y)+0.1))
    graphics::box()
  }

  return(list(x = x, y = y))
}

#' Generate Complex 2D Bevel Profile for 3D Polygons
#'
#' All arguments are recycled to the length of the longest argument, allowing for the generation of
#' complex and repetitive bevel patterns without manual replication of argument values.
#'
#' @param bevel_type Vector of bevel types. Options are: `"circular"`, `"exp"`, `"bump"`, `"step"`, `"block"`, `"angled"`.
#' Note that for the `"step"` type, the transition occurs at `bevel_start`.
#' @param bevel_start Numeric vector of values between `0` and `1`.
#' Percentage distance in the interior the polygon at which to begin the corresponding `bevel_type`. Note that for the `"step"` type, this is ignored.
#' @param bevel_end Numeric vector of values between `0` and `1`.
#' Percentage distance in the interior the polygon at which to end the corresponding `bevel_type`.
#' @param segment_height Numeric vector. The maximum heights of each bevel, as measured from the initial height at the end of the previous bevel.
#' @param angle Default `NULL`. Numeric vector. Optional angle parameter in degrees for angular bevels (overrides values in `max_height`).
#' @param curve_points Default `50`. Integer vector of number of points for each curve.
#' @param manual_offsets Default `NULL`, none. This will force the bevel to add a point (interpolating between the two nearest points)
#' at the specified offsets. This is useful when you want to add points at specific distances along the curve.
#' @param add_end_points Default `TRUE`. Whether to ensure there is a point at zero and a point at one.
#' @param reverse Default `FALSE`. Whether to reverse each bevel.
#' @param flip Default `FALSE`. Whether to reverse each bevel horizontally.
#' @param plot_bevel Default `FALSE`. Whether to plot the resulting bevel.
#' @param overall_height Default `NA`. Numeric value specifying the overall height of the curve.
#'
#' @return List containing 'x' and 'y', which are the coordinates of the complex 2D bevel profile
#' @export
#' @examples
#' # Generate a complex bevel profile and plot it
#' complex_coords = generate_complex_bevel(
#'   bevel_type  = c("circular", "bump", "step", "block", "angled"),
#'   bevel_start = c(0,   0.2, 0.6, 0.7, 0.9),
#'   bevel_end   = c(0.2, 0.5, 0.7, 0.8, 1.0),
#'   segment_height  = c(0.1, 0.2, 0.2, 0.2, 0.4),
#'   angle = 45,
#'   curve_points = c(50, 50, 50, 1, 1),
#'   reverse = c(FALSE, TRUE, FALSE, FALSE, FALSE),
#'   plot_bevel = TRUE
#' )
#' # Create a step function with reverses to generate a square wave pattern
#' complex_coords = generate_complex_bevel(
#'   bevel_type  = "step",
#'   bevel_start = head(seq(0,1,by=0.05),-1),
#'   bevel_end   = 1,
#'   segment_height  = 0.1,
#'   angle = 45,
#'   reverse = c(FALSE, TRUE),
#'   plot_bevel = TRUE
#' )
#' #Generate an increasing sawtooth pattern with angles
#' complex_coords = generate_complex_bevel(
#'   bevel_type  = "angled",
#'   bevel_start = head(seq(0,1,by=0.05),-1),
#'   bevel_end   = tail(seq(0,1,by=0.05),-1),
#'   segment_height  = 0.1,
#'   angle = c(45,30),
#'   reverse = c(FALSE, TRUE),
#'   plot_bevel = TRUE
#' )
#' # Create a step function to turn polygons into a ziggurat (note bevel_end is ignored)
#' complex_coords = generate_complex_bevel(
#'   bevel_type  = "step",
#'   bevel_start = head(seq(0,1,by=0.05),-1),
#'   bevel_end   = 1,
#'   segment_height  = 0.1,
#'   reverse = FALSE,
#'   plot_bevel = TRUE
#' )
generate_complex_bevel = function(bevel_type,
                                  bevel_start = 0,
                                  bevel_end = 1,
                                  segment_height = 1,
                                  angle = 45,
                                  curve_points = 30,
                                  reverse = FALSE,
                                  flip = FALSE,
                                  manual_offsets = NULL,
                                  add_end_points = TRUE,
                                  plot_bevel = FALSE,
                                  overall_height = NA) {
  completed_vals = data.frame(bevel_type = bevel_type,
                              bevel_start = bevel_start,
                              bevel_end = bevel_end,
                              segment_height = segment_height,
                              angle = angle,
                              curve_points = curve_points,
                              flip = flip,
                              reverse = reverse)
  # Sort by bevel_start for sequential generation
  completed_vals = completed_vals[order(completed_vals$bevel_start), ]

  x = c()
  y = c()
  y_prev_end = 0

  for(i in seq_len(nrow(completed_vals))) {
    row = completed_vals[i, ]
    manual_offset_segment = manual_offsets[manual_offsets >= row$bevel_start &
                                           manual_offsets <= row$bevel_end]
    if(length(manual_offset_segment) == 0) {
      manual_offset_segment = NULL
    }
    bevel_segment = generate_bevel(
      bevel_type = row$bevel_type,
      bevel_start = row$bevel_start,
      bevel_end = row$bevel_end,
      max_height = row$segment_height,
      angle = if (!is.null(row$angle)) row$angle else NULL,
      curve_points = row$curve_points,
      reverse = row$reverse,
      flip = row$flip,
      manual_offsets = manual_offset_segment,
      initial_height = y_prev_end,  # Pass the last height of the previous bevel as the initial height
      add_end_points = FALSE
    )

    y_prev_end = bevel_segment$y[length(bevel_segment$y)]  # Update y_prev_end for the next iteration

    if(i == 1) {
      x = bevel_segment$x
      y = bevel_segment$y
    } else {
      x = c(x, bevel_segment$x)
      y = c(y, bevel_segment$y)
    }
  }

  if(add_end_points) {
    if(x[1] != 0) {
      x = c(0,x)
      y = c(y[1],y)
    }
    if(x[length(x)] != 1) {
      x = c(x,1)
      y = c(y,y[length(y)])
    }
  }

  duplicated_x = duplicated(x)
  x = x[!duplicated_x]
  y = y[!duplicated_x]

  if(!is.na(overall_height)) {
    stopifnot(is.numeric(overall_height) && length(overall_height) == 1)
    min_val = min(y)
    max_val = max(y)
    y = (y - min_val) / (max_val - min_val) * overall_height + min_val
  }

  if(plot_bevel) {
    plot(x, y, type = 'l', axes = FALSE, lwd = 2, xlim = c(min(x)-0.1, max(x)+0.1), ylim = c(min(y)-0.1, max(y)+0.1))
    graphics::box()
  }

  return(list(x = x, y = y))
}
