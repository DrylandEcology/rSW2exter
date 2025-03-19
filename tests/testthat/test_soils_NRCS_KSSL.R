
test_that("Fix horizon soil depths", {
  x <- data.frame(hzn_top = c(NA, 3, 5, 10), hzn_bot = c(3, 5, 10, NA))
  res <- fixHorizonDepths(x)

  expect_identical(res[1L, "hzn_top"], 0)
  expect_identical(res[1L, "MethodFixedDepth"], "Hz1Set0cmTop")
  expect_identical(res[nrow(res), "hzn_bot"], res[nrow(res), "hzn_top"] + 1)
  expect_identical(
    res[nrow(res), "MethodFixedDepth"], "Hz4DeepestCreated1cmBottom"
  )


  x <- data.frame(hzn_top = c(0, NA, 5, 10), hzn_bot = c(3, 5, 10, 15))
  res <- fixHorizonDepths(x)

  expect_identical(res[2L, "hzn_top"], 3)
  expect_identical(res[2L, "MethodFixedDepth"], "Hz2ImputedMissingTopDepth")


  x <- data.frame(hzn_top = c(0, 0, 0, 0), hzn_bot = c(NA, NA, NA, NA))
  res <- fixHorizonDepths(x)

  expect_identical(sum(res[, 1:2]), 0)
  expect_identical(
    res[, "MethodFixedDepth"],
    c(
      "Hz1ImputedMissingBottomDepth", "Hz2ImputedMissingBottomDepth",
      "Hz3ImputedMissingBottomDepth", "Hz4DeepestCreated1cmBottom"
    )
  )


  x <- data.frame(hzn_top = c(0, 3, 5, 10), hzn_bot = c(3, 5, 20, 15))
  res <- fixHorizonDepths(x)

  expect_identical(res[3L, "hzn_bot"], 10)
  expect_identical(
    res[3L, "MethodFixedDepth"], "Hz3ResetBottomDepthToDeeperTopDepth(old=20)"
  )


  x <- data.frame(
    hzn_top = c(0, 3, 5, 10, 15, 30), hzn_bot = c(3, 5, 20, 15, 45, 40)
  )
  res <- fixHorizonDepths(x)

  expect_identical(res[c(3L, 5L), "hzn_bot"], c(10, 30))
  expect_identical(
    res[c(3L, 5L), "MethodFixedDepth"],
    c(
      "Hz3ResetBottomDepthToDeeperTopDepth(old=20)",
      "Hz5ResetBottomDepthToDeeperTopDepth(old=45)"
    )
  )


  x <- data.frame(hzn_top = c(0, 3, 7, 10), hzn_bot = c(3, 5, 10, 15))
  res <- fixHorizonDepths(x)

  expect_identical(res[2L, "hzn_bot"], 6)
  expect_identical(res[3L, "hzn_top"], 6)
  expect_identical(
    res[2L, "MethodFixedDepth"], "Hz2ResetToAverageBottomTopDepths(old=5|7)"
  )


  x <- data.frame(
    hzn_top = c(0, 3, 5, 10, 15, 45), hzn_bot = c(3, 5, 20, 15, 30, 40)
  )
  res <- fixHorizonDepths(x)

  expect_identical(res[3L, "hzn_bot"], 10)
  expect_identical(
    res[3L, "MethodFixedDepth"], "Hz3ResetBottomDepthToDeeperTopDepth(old=20)"
  )
  expect_identical(res[5L, "hzn_bot"], 38)
  expect_identical(res[6L, "hzn_top"], 38)
  expect_identical(
    res[5L, "MethodFixedDepth"], "Hz5ResetToAverageBottomTopDepths(old=30|45)"
  )
})

test_that("Extract soils from KSSL", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  x <- c("04N0873", "10N1336", "02N0184")
  res <- extract_soils_KSSL(x = x, what = "pedlabsampnum")

  expect_named(res, c("ref", "table_keys", "table_depths", "table_texture"))

  expect_true(
    rSW2data::check_depth_table(
      table_depths = res[["table_depths"]][, - (1:2), drop = FALSE],
      soil_depth = res[["table_depths"]][, "SoilDepth_cm"],
      n_layers = res[["table_depths"]][, "N_horizons"]
    )
  )

  expect_true(
    rSW2data::check_texture_table(
      table_texture = res[["table_texture"]],
      n_layers = res[["table_depths"]][, "N_horizons"]
    )[["checks_passed"]]
  )
})
