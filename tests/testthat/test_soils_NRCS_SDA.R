
test_that("Calculate NRCS organic soil horizons", {
  x <- data.frame(
    taxorder = c("Histosols", "x", "x", "x", "x", "x", NA),
    taxsubgrp = c("x", "histic", "x", "x", "x", "x", NA),
    desgnmaster = c("L", "L", "O", "x", "x", "x", NA),
    texture = c("x", "x", "x", "CE", "x", "x", NA),
    lieutex = c("x", "x", "x", "x", "Muck", "x", NA),
    stringsAsFactors = FALSE
  )

  expect_identical(
    is_NRCS_horizon_organic(x),
    c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, NA)
  )

  expect_false(is_NRCS_horizon_organic(x[1, , drop = FALSE]))
})


test_that("Calculate NRCS soil depth", {
  N <- 11
  soildepth0 <- 38
  var_stxt3 <- c("sand", "clay", "silt")

  x0 <- data.frame(
    id = rep(1, N),
    layer_depth = c(5, 10, 20, 30, 40, 60, 80, 100, 150, 200, 250),
    Horizon_No = seq_len(N),
    rockdepm = soildepth0,
    sand = c(51, 51, 50, 46, 47, 48, NA, NA, NA, NA, NA),
    clay = c(16, 16, 17, 19, 18, 18, NA, NA, NA, NA, NA),
    silt = c(33, 33, 33, 35, 35, 34, NA, NA, NA, NA, NA)
  )

  for (k in c(6, 1)) {
    x <- x0
    x[k:nrow(x), var_stxt3] <- NA
    soildepth <- if (k > 1) soildepth0 else 0
    id_sd <- findInterval(soildepth, c(1, x[, "layer_depth"]))

    locs_table_depths <- calculate_soil_depth_NRCS(
      x,
      target_site_ids = 1,
      restrict_by_ec_or_ph = FALSE,
      var_site_id = "id",
      var_horizon = "Horizon_No",
      var_horizon_lower_depth = "layer_depth",
      var_restrictions = "rockdepm",
      var_soiltexture = var_stxt3
    )

    expect_equal(
      locs_table_depths[1, "N_horizons"],
      id_sd,
      ignore_attr = c("waldo_opts", "type")
    )
    expect_identical(locs_table_depths[1, "SoilDepth_cm"], soildepth)
    expect_identical(locs_table_depths[1, 2 + id_sd], soildepth)
    if (k > 1) {
      expect_identical(
        unname(locs_table_depths[1, 2 + 1:(id_sd - 1)]),
        x[1:(id_sd - 1), "layer_depth"]
      )
    }
  }
})


test_that("Extract soils from NRCS SDA", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("soilDB")
  skip_if_offline()

  locations <- matrix(
    data = c(-120.325, -111.245, 39.855, 36.753),
    nrow = 2
  )

  mukeys <- c(471168L, 1606800L)

  expected_soil_variables <- c("MUKEY", "COKEY", "Horizon_No")
  expected_depth_variables <- c("N_horizons", "SoilDepth_cm")
  expected_obj_results <- c("table_keys", "table_depths", "table_texture")
  expected_obj_variables <- c("ref", expected_obj_results)

  var_restrictions <-
    c("Horizon_depth", "RootZoneRestriction_depth", "Bedrock_depth")
  var_soiltexture <- c("sandtotal_r", "claytotal_r", "silttotal_r")


  x <- fetch_soils_from_NRCS_SDA(mukeys_unique = mukeys)

  expect_true(all(expected_soil_variables %in% colnames(x)))
  expect_true(all(x[, "MUKEY"] %in% mukeys))

  x[, "organic"] <- is_NRCS_horizon_organic(x)
  expect_true(all(x[, "organic"] %in% c(NA, FALSE, TRUE)))

  # nolint start: implicit_assignment_linter.
  expect_silent(
    sd1 <- calculate_soil_depth_NRCS(
      x,
      restrict_by_ec_or_ph = FALSE,
      var_site_id = "COKEY",
      var_horizon = "Horizon_No",
      var_horizon_lower_depth = "hzdepb_r",
      var_restrictions = var_restrictions,
      var_soiltexture = var_soiltexture
    )
  )
  expect_silent(
    sd2 <- calculate_soil_depth_NRCS(
      x,
      restrict_by_ec_or_ph = TRUE,
      var_site_id = "COKEY",
      var_horizon = "Horizon_No",
      var_horizon_lower_depth = "hzdepb_r",
      var_restrictions = var_restrictions,
      var_soiltexture = var_soiltexture
    )
  )
  # nolint end: implicit_assignment_linter.

  expect_true(all(expected_depth_variables %in% colnames(sd1)))
  expect_true(
    rSW2data::check_depth_table(
      table_depths = sd1[, - (1:2)],
      soil_depth = sd1[, "SoilDepth_cm"],
      n_layers = sd1[, "N_horizons"]
    )
  )
  expect_true(all(expected_depth_variables %in% colnames(sd2)))
  expect_true(
    rSW2data::check_depth_table(
      table_depths = sd2[, - (1:2)],
      soil_depth = sd2[, "SoilDepth_cm"],
      n_layers = sd2[, "N_horizons"]
    )
  )


  tmp <- suppressWarnings(fetch_mukeys_spatially_NRCS_SDA(locations))
  expect_identical(tmp[["mukeys"]], mukeys)


  # Test chunking of `locations`
  ids <- rep(seq_len(nrow(locations)), each = 50L)
  tmp <- suppressWarnings(fetch_mukeys_spatially_NRCS_SDA(
    x = locations[ids, , drop = FALSE],
    chunk_size = 10L
  ))
  expect_identical(tmp[["mukeys"]], mukeys[ids])



  # Example 1: extract soils by mukey values
  soils1a <- extract_soils_NRCS_SDA(mukeys = mukeys[[1L]])
  soils1 <- extract_soils_NRCS_SDA(mukeys = mukeys)

  for (kelem in expected_obj_results) {
    expect_identical(soils1a[[kelem]], soils1[[kelem]][1L, , drop = FALSE])
  }

  # Example 2: extract soils by geographic location
  soils2a <- suppressWarnings(extract_soils_NRCS_SDA(x = locations[1L, ]))
  soils2 <- suppressWarnings(extract_soils_NRCS_SDA(x = locations))

  for (kelem in expected_obj_results) {
    expect_identical(soils2a[[kelem]], soils2[[kelem]][1L, , drop = FALSE])
  }


  expect_identical(soils1, soils2)
  expect_named(soils1, expected_obj_variables)
  expect_true(
    rSW2data::check_depth_table(
      table_depths = soils1[["table_depths"]][, - (1:2)],
      soil_depth = soils1[["table_depths"]][, "SoilDepth_cm"],
      n_layers = soils1[["table_depths"]][, "N_horizons"]
    )
  )
  expect_true(
    rSW2data::check_texture_table(
      table_texture = soils1[["table_texture"]],
      n_layers = soils1[["table_depths"]][, "N_horizons"]
    )[["checks_passed"]]
  )


  # Example 3: first identify mukey values by geographic location,
  # then query soils from SSURGO by mukey,
  # but still pass locations in case we need to query STATSGO as well
  soils3 <- extract_soils_NRCS_SDA(
    x = locations,
    mukeys = mukeys,
    method = "SSURGO_then_STATSGO",
    remove_organic_horizons = "at_surface",
    replace_missing_fragvol_with_zero = "at_surface",
    estimate_missing_bulkdensity = TRUE,
    restrict_by_ec_or_ph = FALSE,
    impute = TRUE,
    progress_bar = FALSE,
    verbose = FALSE
  )

  expect_named(soils3, expected_obj_variables)
  expect_true(
    rSW2data::check_depth_table(
      table_depths = soils3[["table_depths"]][, - (1:2)],
      soil_depth = soils3[["table_depths"]][, "SoilDepth_cm"],
      n_layers = soils3[["table_depths"]][, "N_horizons"]
    )
  )
  expect_true(
    rSW2data::check_texture_table(
      table_texture = soils3[["table_texture"]],
      n_layers = soils3[["table_depths"]][, "N_horizons"]
    )[["checks_passed"]]
  )

})
