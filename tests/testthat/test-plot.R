library(sf)

test_that("beveled_polygon works", {
  use_states = spData::us_states
  nj = spData::us_states[spData::us_states$NAME == "New Jersey",] |>
    st_transform("EPSG:3081")
  buffer_dist = 15000

  testthat::expect_no_error({nj |>
    skeletonize() |>
    generate_offset_polygon(buffer_dist) |>
    plot_offset_polygon(plot_original_polygon = TRUE)})
  testthat::expect_error({spData::us_states |>
      skeletonize() |>
      generate_offset_polygon(buffer_dist) |>
      plot_offset_polygon(plot_original_polygon = TRUE)})

  testthat::expect_no_error({spData::us_states[spData::us_states$NAME == "New York",] |>
    skeletonize() |>
    generate_offset_polygon(0.1) |>
    plot_offset_polygon(plot_original_polygon = TRUE)})

  testthat::expect_no_error({spData::us_states |>
      skeletonize() |>
      generate_offset_polygon(0.2) |>
      plot_offset_polygon(plot_original_polygon = TRUE)})

  testthat::expect_no_error({
    buffer_vals = seq(0,5,by=1)
    spData::us_states[1:2,] |>
      skeletonize() |>
      generate_offset_polygon(buffer_vals) |>
      plot_offset_polygon(plot_original_polygon = TRUE)
    })

})
