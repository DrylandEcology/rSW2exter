
test_that("Extract from NED USA", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("FedData")
  skip_if_offline()


  label_ned <- "ned_1s_example"
  path_ned <- "."
  filenames_ned_examples <- list(
    elev = paste0(label_ned, "_NED_1.tif"),
    slope = paste0("slope_", label_ned, "_NED_1.tif"),
    aspect = paste0("aspect_", label_ned, "_NED_1.tif")
  )

  locations <- rSW2st::as_points(
    matrix(data = c(-120.325, -120.328, 43.328, 43.242), nrow = 2),
    to_class = "sf",
    crs = 4326
  )
  extent_polygon <- terra::vect(
    1.1 * terra::ext(locations),
    crs = terra::crs(locations)
  )

  ### Download NED
  ned_1s_example <- suppressMessages(FedData::get_ned(
    template = extent_polygon,
    label = label_ned,
    res = 1,
    extraction.dir = path_ned
  ))

  ### Derive slope and aspect
  for (opt in c("slope", "aspect")) {
    tmp <- raster::terrain(
      x = ned_1s_example,
      opt = opt,
      unit = "degrees",
      filename = filenames_ned_examples[[opt]],
      datatype = "FLT4S",
      options = c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND")
    )
  }

  ### Get values
  vals_topo <- extract_topography_NEDUSA(
    locations,
    path = path_ned,
    file_datasets = filenames_ned_examples,
    south_aspect = 180,
    method = "simple"
  )


  #--- Expectations
  expect_identical(nrow(vals_topo), nrow(locations))
  expect_identical(ncol(vals_topo), 3L)
  expect_identical(colnames(vals_topo), c("elev", "slope", "aspect"))
  expect_type(vals_topo, "double")
  expect_true(
    all(
      is.na(vals_topo[, "slope"]) |
      (vals_topo[, "slope"] >= 0 & vals_topo[, "slope"] <= 90)
    )
  )
  expect_true(
    all(
      is.na(vals_topo[, "aspect"]) |
      vals_topo[, "aspect"] == 999 |
      (vals_topo[, "aspect"] >= -180 & vals_topo[, "aspect"] <= 180)
    )
  )


  # Clean up
  unlink(file.path(path_ned, unlist(filenames_ned_examples)))
})
