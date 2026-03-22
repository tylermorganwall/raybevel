test_that("generate_roof handles invalid core count options", {
  square = matrix(c(0, 0, 10, 0, 10, 10, 0, 10), ncol = 2, byrow = TRUE)
  skeleton = skeletonize(square)
  mat = rayvertex::material_list(diffuse = "grey50")

  old_cores = getOption("cores")
  old_ncpus = getOption("Ncpus")
  on.exit({
    options(cores = old_cores)
    options(Ncpus = old_ncpus)
  }, add = TRUE)

  options(cores = NA_integer_)
  options(Ncpus = NA_integer_)

  expect_no_error({
    mesh = generate_roof(
      skeleton,
      max_height = 1,
      base_height = 0,
      vertical_offset = 1,
      material = mat,
      roof_material = mat,
      sides = TRUE,
      base = TRUE,
      progress = FALSE
    )
  })
  expect_s3_class(mesh, "ray_mesh")
})
