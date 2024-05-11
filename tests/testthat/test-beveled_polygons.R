save_3d_sw_test_png = function(code, path) {
  code
  path
}

compare_image = function(path1, path2) {
  image1 = png::readPNG(path1)
  image2 = png::readPNG(path2)
  return(identical(image1, image2))
}

library(rayrender)

run_tests_bevel = function(argument_grid, plot_prefix="", interactive_run=FALSE, seed = 1, ...) {
  stopifnot(inherits(argument_grid,"data.frame"))
  for(i in seq_len(nrow(argument_grid))){
    args = unlist(argument_grid[i,], recursive = FALSE)
    test_filename = sprintf("%s_test%i.png",
                            plot_prefix , i)
    path = tempfile(fileext = ".png")
    skip = FALSE
    args = append(args, ...)
    poly_bevel_mesh = tryCatch({
      do.call("generate_beveled_polygon", args = args)
    }, error = function(e) {
      expect_snapshot_error(do.call("generate_beveled_polygon", args = args))
      skip <<- TRUE
    })
    if(skip) {
      next
    }
    if(interactive_run) {
      set.seed(seed)
      raymesh_model(poly_bevel_mesh, material = diffuse(color="dodgerblue"), override_material = TRUE) |>
        add_object(sphere(y=15,x=10,z=-10,material=light(intensity = 200))) |>
        add_object(sphere(y=-15,x=10,z=-10,material=light(intensity = 200))) |>

        render_scene(lookfrom=c(10,10,0), sample_method = "sobol_blue",fov=50,
                     preview=TRUE, debug_channel="normal",
                     samples=128, width=100,height=100)
    } else {
      set.seed(seed)
      raymesh_model(poly_bevel_mesh, material = diffuse(color="dodgerblue"), override_material = TRUE) |>
        add_object(sphere(y=15,x=10,z=-10,material=light(intensity = 200))) |>
        add_object(sphere(y=-15,x=10,z=-10,material=light(intensity = 200))) |>

        render_scene(lookfrom=c(10,10,0), sample_method = "sobol_blue",fov=50,
                     filename = path, preview=FALSE,debug_channel="normal",
                     samples=1, width=100,height=100)
      expect_snapshot_file(path = path, name = test_filename, compare = compare_image)
    }
  }
}

test_that("Checking generate_beveled_polygon() raw extrusion ", {
  vertices = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)-3.5
  # Holes inside the polygon
  hole_1 = matrix(c(1,1, 2,1, 2,2, 1,2, 1,1), ncol = 2, byrow = TRUE)[5:1,]-3.5
  hole_2 = matrix(c(5,5, 6,5, 6,6, 5,6, 5,5), ncol = 2, byrow = TRUE)[5:1,]-3.5
  skeleton = skeletonize(vertices, holes = list(hole_1,hole_2))
  set.seed(1)

  generate_polygon_args = expand.grid(bevel_offsets = list(c(0.25,0.5,1,1.5,2)),
                              bevel_heights = list(c(0.25,0.25,0.5,0.5,0.75)*2),
                              raw_offsets = list(TRUE),
                              raw_heights = list(TRUE),
                              base = list(FALSE, TRUE),
                              set_max_height = list(TRUE, FALSE),
                              sides = list(TRUE, FALSE),
                              max_height = list(3),
                              double_sided = list(TRUE, FALSE),
                              base_height = list(NA, -1.5,-0.5,0.5),
                              vertical_offset = list(NA,-0.5,0.5))

  run_tests_bevel(generate_polygon_args, plot_prefix = "bevel_raw", interactive_run = FALSE,
                  seed = 1,
                  list(skeleton = skeleton))
})


