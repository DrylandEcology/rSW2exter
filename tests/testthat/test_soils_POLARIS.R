
test_that("Extract soils from POLARIS", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()


  path_polaris <- file.path("..", "test_data", "polaris_example")
  vars <- c("bd", "sand", "clay", "silt")
  stat <- "mean"

  ## Check that we have POLARIS data
  has_POLARIS <- isTRUE(all(
    check_POLARIS(path = path_polaris, vars = vars, stat = stat)
  ))

  if (has_POLARIS) {

    locations <- matrix(
      data = c(-120.325, -111.245, 39.855, 36.753),
      nrow = 2
    )

    ## Extract median of mean gridcell values across 100-m buffer
    ## around point locations
    res1 <- extract_soils_POLARIS(
      x = locations,
      vars = vars,
      stat = stat,
      path = path_polaris,
      buffer_m = 100,
      fun = median,
      na.rm = TRUE
    )

    ## Extract mean gridcell values at point locations and use 70-m buffer at
    ## sites with bad values
    res2 <- extract_soils_POLARIS(
      x = locations,
      vars = vars,
      stat = stat,
      path = path_polaris,
      method = "fix_with_buffer",
      fix_criteria = list(
        bd = list(op = "<", value = 0.6),
        texture = list(op = "<", value = 50)
      ),
      buffer_m = 70,
      fun = list(
        bd = function(x, na.rm = TRUE) median(x[x > 0.6], na.rm = na.rm),
        texture = median
      ),
      na.rm = TRUE,
      digits = 3
    )

  } else {
    skip("No POLARIS data available to test.")
  }
})
