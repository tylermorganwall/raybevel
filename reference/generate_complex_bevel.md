# Generate Complex 2D Bevel Profile for 3D Polygons

All arguments are recycled to the length of the longest argument,
allowing for the generation of complex and repetitive bevel patterns
without manual replication of argument values.

## Usage

``` r
generate_complex_bevel(
  bevel_type,
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
  overall_height = NA,
  set_minimum_zero = TRUE,
  zero_offset_epsilon = 1e-06
)
```

## Arguments

- bevel_type:

  Vector of bevel types. Options are: \`"circular"\`, \`"exp"\`,
  \`"bump"\`, \`"step"\`, \`"block"\`, \`"angled"\`. Note that for the
  \`"step"\` type, the transition occurs at \`bevel_start\`.

- bevel_start:

  Numeric vector of values between \`0\` and \`1\`. Percentage distance
  in the interior the polygon at which to begin the corresponding
  \`bevel_type\`. Note that for the \`"step"\` type, this is ignored.

- bevel_end:

  Numeric vector of values between \`0\` and \`1\`. Percentage distance
  in the interior the polygon at which to end the corresponding
  \`bevel_type\`.

- segment_height:

  Numeric vector. The maximum heights of each bevel, as measured from
  the initial height at the end of the previous bevel.

- angle:

  Default \`NULL\`. Numeric vector. Optional angle parameter in degrees
  for angular bevels (overrides values in \`max_height\`).

- curve_points:

  Default \`50\`. Integer vector of number of points for each curve.

- reverse:

  Default \`FALSE\`. Whether to reverse each bevel.

- flip:

  Default \`FALSE\`. Whether to reverse each bevel horizontally.

- manual_offsets:

  Default \`NULL\`, none. This will force the bevel to add a point
  (interpolating between the two nearest points) at the specified
  offsets. This is useful when you want to add points at specific
  distances along the curve.

- add_end_points:

  Default \`TRUE\`. Whether to ensure there is a point at zero and a
  point at one.

- plot_bevel:

  Default \`FALSE\`. Whether to plot the resulting bevel.

- overall_height:

  Default \`NA\`. Numeric value specifying the overall height of the
  curve.

- set_minimum_zero:

  Default \`TRUE\`. Whether to offset the lowest point of the bevel so
  it's at zero.

- zero_offset_epsilon:

  Default \`1e-5\`. Amount to offset the bevel to ensure no
  self-intersection with the base.

## Value

List containing 'x' and 'y', which are the coordinates of the complex 2D
bevel profile

## Examples

``` r
# Generate a complex bevel profile and plot it
complex_coords = generate_complex_bevel(
  bevel_type  = c("circular", "bump", "step", "block", "angled"),
  bevel_start = c(0,   0.2, 0.6, 0.7, 0.9),
  bevel_end   = c(0.2, 0.5, 0.7, 0.8, 1.0),
  segment_height  = c(0.1, 0.2, 0.2, 0.2, 0.4),
  angle = 45,
  curve_points = c(50, 50, 50, 1, 1),
  reverse = c(FALSE, TRUE, FALSE, FALSE, FALSE),
  plot_bevel = TRUE
)

# Create a step function with reverses to generate a square wave pattern
complex_coords = generate_complex_bevel(
  bevel_type  = "step",
  bevel_start = head(seq(0,1,by=0.05),-1),
  bevel_end   = 1,
  segment_height  = 0.1,
  angle = 45,
  reverse = c(FALSE, TRUE),
  plot_bevel = TRUE
)

#Generate an increasing sawtooth pattern with angles
complex_coords = generate_complex_bevel(
  bevel_type  = "angled",
  bevel_start = head(seq(0,1,by=0.05),-1),
  bevel_end   = tail(seq(0,1,by=0.05),-1),
  segment_height  = 0.1,
  angle = c(45,30),
  reverse = c(FALSE, TRUE),
  plot_bevel = TRUE
)

# Create a step function to turn polygons into a ziggurat (note bevel_end is ignored)
complex_coords = generate_complex_bevel(
  bevel_type  = "step",
  bevel_start = head(seq(0,1,by=0.05),-1),
  bevel_end   = 1,
  segment_height  = 0.1,
  reverse = FALSE,
  plot_bevel = TRUE
)
```
