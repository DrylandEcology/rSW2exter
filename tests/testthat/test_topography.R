
test_that("Extract from NED USA", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  # path relative to rSW2exter/tests/testthat/
  path_ned <- file.path("..", "test_data", "NED1")
  dir.create(path_ned, recursive = TRUE, showWarnings = FALSE)

  label_ned <- "ned_1s_example"
  fnames_ned <- list(
    elev = paste0(label_ned, "_NED_1.tif"),
    slope = paste0("slope_", label_ned, "_NED_1.tif"),
    aspect = paste0("aspect_", label_ned, "_NED_1.tif")
  )
  paths_ned <- file.path(path_ned, fnames_ned)
  names(paths_ned) <- names(fnames_ned)

  locations <- rSW2st::as_points(
    matrix(data = c(-120.34, -120.33, 43.23, 43.24), nrow = 2),
    to_class = "sf",
    crs = 4326
  )
  extent_polygon <- terra::vect(
    1.2 * terra::ext(locations),
    crs = terra::crs(locations)
  )

  ### Download NED if (needed)
  ned_1s_example <- if (file.exists(paths_ned[["elev"]])) {
    terra::rast(paths_ned[["elev"]])

  } else {
    skip_if_not_installed("FedData")
    suppressMessages(FedData::get_ned(
      template = extent_polygon,
      label = label_ned,
      res = 1,
      extraction.dir = path_ned
    ))
  }

  ### Derive slope and aspect
  for (opt in c("slope", "aspect")) {
    if (!file.exists(paths_ned[[opt]])) {
      tmp <- terra::terrain(
        x = ned_1s_example,
        v = opt,
        unit = "degrees",
        filename = paths_ned[[opt]]
      )
    }
  }

  ### Get values
  vals_topo <- extract_topography_NEDUSA(
    locations,
    path = path_ned,
    file_datasets = fnames_ned,
    south_aspect = 180,
    method = "simple"
  )

  ### Get just elevation values
  #nolint start: extraction_operator_linter.
  vals_elev <- extract_topography_NEDUSA(
    locations,
    path = path_ned,
    file_datasets = fnames_ned["elev"],
    method = "simple"
  )
  #nolint end: extraction_operator_linter.


  #--- Expectations
  expect_identical(nrow(vals_topo), nrow(locations))
  expect_identical(ncol(vals_topo), 3L)
  expect_identical(colnames(vals_topo), c("elev", "slope", "aspect"))
  expect_type(as.matrix(vals_topo), "double")
  expect_true(
    all(
      (vals_topo[, "slope"] >= 0 & vals_topo[, "slope"] <= 90)
    )
  )
  expect_true(
    all(
      (vals_topo[, "aspect"] >= -180 & vals_topo[, "aspect"] <= 180)
    )
  )

  expect_identical(nrow(vals_elev), nrow(locations))
  expect_identical(ncol(vals_elev), 1L)
  expect_identical(colnames(vals_elev), "elev")
  expect_type(as.matrix(vals_elev), "double")

  expect_equal(vals_elev, vals_topo["elev"]) #nolint: extraction_operator_linter

})
