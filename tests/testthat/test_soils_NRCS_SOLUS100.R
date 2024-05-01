
test_that("Extract soils from SOLUS100", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline()

  # path relative to rSW2exter/tests/testthat/
  path_solus100 <- file.path("..", "test_data", "SOLUS100")
  dir.create(path_solus100, recursive = TRUE, showWarnings = FALSE)

  vars_solus100 <- c("resdept_cm", "sandtotal", "silttotal", "claytotal")
  req_depths <- c(0, 5, 150)
  requested_layer_depths <- c(5, 15, 30, 100, 150, 200)

  ## Download data (if needed)
  fns_solus100 <- download_SOLUS100(
    path = path_solus100,
    vars = vars_solus100,
    depths = req_depths
  )

  ## Check that we have SOLUS100 data
  has_SOLUS100 <- isTRUE(all(
    check_SOLUS100(
      path = path_solus100,
      vars = vars_solus100,
      depths = req_depths
    )
  ))

  expect_true(has_SOLUS100)

  locations <- matrix(
    data = c(-120.1286878, -111.8511136, 39.8182913, 36.9047396),
    nrow = 2
  )

  ## k == 1: extract values at surface
  ## k == 2: extract all depth values (which allows to interpolate by layer)
  for (k in 1:2) {
    kids <- if (k == 1L) 1L else seq_along(req_depths)

    ## Extract gridcell values at point locations
    res1 <- extract_soils_SOLUS100(
      x = locations,
      vars = vars_solus100,
      depths = req_depths[kids],
      path = path_solus100
    )

    expect_named(res1, c("ref", "table_depths", "table_texture"))
    if (k == 1L) {
      expect_true(
        rSW2data::check_texture_table(
          table_texture = res1[["table_texture"]],
          n_layers = res1[["table_depths"]][, "N_horizons"]
        )[["checks_passed"]]
      )
    }

    ## Extract gridcell values
    ##   * use 700-m buffer at sites with bad values
    ##   * interpolate vertically by layers
    res2 <- extract_soils_SOLUS100(
      x = locations,
      vars = vars_solus100,
      depths = req_depths[kids],
      requested_layer_depths = requested_layer_depths,
      path = path_solus100,
      method_vertical = if (length(kids) > 1L) {
        "interpolate_by_layer"
      } else {
        "asis"
      },
      method_horizontal = "fix_with_buffer",
      fix_criteria = list(
        dbovendry = list(op = "<", value = 0.6),
        texture = list(op = "<", value = 50)
      ),
      buffer_m = 700,
      fun = list(
        dbovendry = function(x, na.rm = TRUE) median(x[x > 0.6], na.rm = na.rm),
        texture = median
      ),
      na.rm = TRUE,
      digits = 3L
    )


    expect_named(res2, c("ref", "table_depths", "table_texture"))
    expect_true(
      rSW2data::check_texture_table(
        table_texture = res2[["table_texture"]],
        n_layers = res2[["table_depths"]][, "N_horizons"]
      )[["checks_passed"]]
    )
  }
})
