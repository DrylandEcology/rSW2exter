
#' Functionality to use the \var{CONUSSoil} soil dataset
#' for \pkg{SOILWAT2} applications
#'
#' @param path A character string. The path to the local copy of the
#'   \var{CONUSSoil} folder hierarchy, e.g.,
#'   \code{dirname(prepare_script_for_Miller1998_CONUSSoil())}.
#' @param vars A vector of character strings. The requested variables as
#'   used by \var{CONUSSoil}; see Miller et al. 1998.
#' @param lower_limits_by_vars A named numeric vector. The names correspond
#'   to \code{vars} and values represent the lower limits that are conditioned/
#'   masked out, i.e., set to \code{NA}.
#' @param digits An integer value. The number of digits to which soil texture
#'   variables are rounded. Skip rounding if \code{NA} or \code{NULL}.
#' @param verbose A logical value.
#'
#' @references
#'  Miller, D. A., and R. A. White. 1998. A conterminous United States
#'  multilayer soil characteristics dataset for regional climate and
#'  hydrology modeling. Earth Interactions 2:1-26.
#'  \url{http://www.soilinfo.psu.edu/index.cgi?soil_data&conus}
#'
#' @seealso \code{\link{extract_soils_Miller1998_CONUSSoil}}
#'
#' @name conussoil
#' @aliases miller1998
NULL


create_reference_for_Miller1998_CONUSSoil <- function() {
  paste0(
    "Miller, D. A., and R. A. White. 1998. A conterminous United States ",
    "multilayer soil characteristics dataset for regional climate and ",
    "hydrology modeling. Earth Interactions 2:1-26. ",
    "http://dx.doi.org/10.1175%2F1087-3562%281998%29002%3C0001%3AACUSMS%3E2.3.CO%3B2 ", #nolint
    "Data accessed [",
    format(as.POSIXlt(Sys.Date()), "%Y-%b-%e"),
    "]"
  )
}



#' Create \var{wget} script to prepare soil data files from \var{CONUSSoil}
#'
#' This function only writes out a text file to disk. The user
#' is responsible to run the script after making sure that \var{wget} is
#' available and the script is executable.
#' The script will download the \var{CONUSSoil} files
#' for the requested variables and convert the \var{ESRI} formatted files to
#' \var{GeoTIFF}.
#'
#' @param path A character string. The path to where the local copy of the
#'   \var{CONUSSoil} folder hierarchy and files should be downloaded.
#' @inheritParams conussoil
#'
#' @return (Invisibly) the file path of the generated bash script.
#'
#' @section Notes: this function is not yet implemented!
#'
#' @inherit conussoil references
#'
#' @examples
#' fname_wget_miller1998 <- prepare_script_for_Miller1998_CONUSSoil()
#'
#' ## in a shell
#' ## give execute permission if needed: chmod +x <fname_wget_miller1998>
#' ## download data: ./<fname_wget_miller1998>
#'
#' unlink(fname_wget_miller1998)
#'
#' @export
prepare_script_for_Miller1998_CONUSSoil <- function(
  path = ".",
  vars = c("rockdepm", "rockvol", "bd", "sand", "clay", "silt")
) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # TODO: implement `prepare_script_for_Miller1998_CONUSSoil()`
  warning("`prepare_script_for_Miller1998_CONUSSoil()` is not yet implemented.")

  file <- file.path(
    path,
    paste0("wget_CONUSSoil_", format(as.POSIXlt(Sys.Date()), "%Y%m%d"), ".sh")
  )

  invisible(file)
}


#' Mask out unrealistic variable values in \var{CONUSSoil}
#'
#' This function creates new \var{CONUSSoil-GeoTIFFs} where
#' values below a limit are masked out (set as missing). This can be helpful
#' when aggregating values from multiple grid cells for problematic variables
#' such as bulk density (\var{bd})
#' for which some value ranges are unrealistic for representing soils .
#'
#' @inheritParams conussoil
#'
#' @return This function is called for its side effects of creating
#'   \var{GeoTIFFs} if they don't already exist.
#'
#' @export
create_conditioned_Miller1998_CONUSSoil <- function(
  path,
  vars = c("rockdepm", "rockvol", "bd", "sand", "clay", "silt"),
  lower_limits_by_vars = c(
    rockdepm = 0, rockvol = 0, bd = 30, sand = 0, clay = 0, silt = 0
  )
) {

  stopifnot(vars %in% names(lower_limits_by_vars))

  for (k in seq_along(vars)) {
    ftmp_orig <- file.path(path, paste0(vars[k], ".tif"))
    limit <- as.integer(lower_limits_by_vars[vars[k]])
    ftmp_cond <- filepath_Miller1998_CONUSSoil(path, vars[k], limit)

    if (!file.exists(ftmp_cond)) {
      fun_cond <- compiler::cmpfun(
        function(x) ifelse(!is.na(x) & x > limit, x, NA)
      )

      tmp <- try(raster::calc(
        x = raster::brick(ftmp_orig),
        fun = fun_cond,
        filename = ftmp_cond
      ))

      if (inherits(tmp, "try-error")) {
        warning(
          "Was not able to calculate and create conditioned CONUSSoil: ",
          shQuote(basename(ftmp_cond))
        )
      }
    }
  }

}


depth_profile_Miller1998_CONUSSoil <- function() {
  c(5, 10, 20, 30, 40, 60, 80, 100, 150, 200, 250)
}


filepath_Miller1998_CONUSSoil <- function(path, var, lower_limit) {
  tmp <- if (!missing(lower_limit) && is.finite(lower_limit)) {
    paste0(var, "_cond", as.integer(lower_limit), ".tif")
  } else {
    paste0(var, ".tif")
  }

  file.path(path, tmp)
}



#' Check that \var{CONUSSoil} soil data are locally available
#'
#' @inheritParams conussoil
#'
#' @return A logical vector of the length of \code{vars}.
#'
#' @inherit conussoil references
#'
#' @examples
#' script_to_download_conussoil <- prepare_script_for_Miller1998_CONUSSoil()
#'
#' ## Execute script to download data
#' ## (or set `path_conussoil` to your local copy)
#' path_conussoil <- dirname(script_to_download_conussoil)
#'
#' ## Mask out unrealistic variable values
#' create_conditioned_Miller1998_CONUSSoil(path = path_conussoil)
#'
#' ## Check that we have CONUSSoil data
#' has_CONUSSoil <- check_Miller1998_CONUSSoil(path = path_conussoil)
#'
#' # Do we have all files?
#' isTRUE(all(has_CONUSSoil))
#'
#' # If not, then examine which variables are missing
#' has_CONUSSoil
#'
#' @export
check_Miller1998_CONUSSoil <- function(
  path = ".",
  vars = c("rockdepm", "rockvol", "bd", "sand", "clay", "silt"),
  lower_limits_by_vars = c(
    rockdepm = 0, rockvol = 0, bd = 30, sand = 0, clay = 0, silt = 0
  )
) {

  sapply(
    vars,
    function(var) {
      file.exists(filepath_Miller1998_CONUSSoil(
        path = path,
        var = var,
        lower_limit = lower_limits_by_vars[var]
      ))
    }
  )
}




#' Extract soil information from the \var{CONUSSoil} soil dataset
#'
#' @inheritParams conussoil
#' @inheritParams rSW2st::as_points
#'
#' @section Notes: This is a function with minimal functionality;
#' use \code{\link{extract_soils_Miller1998_CONUSSoil}}
#' for a user-friendly interface.
#'
#' @inherit conussoil references
#'
#' @export
fetch_soils_from_Miller1998_CONUSSoil <- function(
  x, crs, vars, lower_limits_by_vars, path, verbose
) {

  #--- Make sure inputs are correctly formatted
  depths <- depth_profile_Miller1998_CONUSSoil()
  locations <- rSW2st::as_points(x, to_class = "sf", crs = crs)

  # Align with data crs
  ftmp <- filepath_Miller1998_CONUSSoil(
    path = path,
    var = vars[1],
    lower_limit = lower_limits_by_vars[vars[1]]
  )

  tmp_crs <- sf::st_crs(raster::brick(ftmp))

  if (sf::st_crs(locations) != tmp_crs) {
    locations <- sf::st_transform(locations, crs = tmp_crs)
  }


  #--- Prepare result object
  res <- array(
    NA,
    dim = c(nrow(locations), length(vars), length(depths)),
    dimnames = list(NULL, vars, depths)
  )


  #--- Extract values
  for (iv in seq_along(vars)) {
    ftmp <- filepath_Miller1998_CONUSSoil(
      path = path,
      var = vars[iv],
      lower_limit = lower_limits_by_vars[vars[iv]]
    )


    if (file.exists(ftmp)) {
      if (verbose) {
        message(Sys.time(), " extracting ", shQuote(vars[iv]))
      }

      tmp <- raster::extract(
        x = raster::brick(ftmp),
        y = locations,
        method = "simple"
      )

      # nolint start
      # tmp <- do.call(
      #   "extract_rSFSW2",
      #   args = list(
      #     x = raster::brick(ftmp),
      #     y = locations, # rSW2st::as_points(locations, to_class = "sp"),
      #     type = sim_space[["scorp"]],
      #     method = "simple"
      #   )
      # )
      # nolint end

      ntmp <- ncol(tmp)
      if (ntmp == 1 || ntmp == length(depths)) {
        res[, iv, ] <- tmp
      } else {
        res[, iv, seq_len(ncol(tmp))] <- tmp
      }

    } else {
      stop("Miller1998/CONUSSoil data ", shQuote(basename(ftmp)), " not found.")
    }
  }

  res
}





#' Extract soil information from the \var{CONUSSoil} soil dataset
#' for \pkg{SOILWAT2} applications
#'
#' @inheritParams conussoil
#' @inheritParams rSW2st::as_points
#' @inheritParams extract_soils_NRCS_SDA
#'
#' @section Notes: A local copy of \var{CONUSSoil} is required. The function
#'   \code{\link{prepare_script_for_Miller1998_CONUSSoil}} creates a script
#'   that can be used to download and prepare \var{CONUSSoil} files.
#'
#' @inherit conussoil references
#'
#' @seealso \code{\link[raster]{extract}}
#'
#' @examples
#' script_to_download_conussoil <- prepare_script_for_Miller1998_CONUSSoil()
#'
#' ## Execute script to download data
#' ## (or set `path_conussoil` to your local copy)
#' path_conussoil <- dirname(script_to_download_conussoil)
#'
#' ## Mask out unrealistic variable values
#' create_conditioned_Miller1998_CONUSSoil(path = path_conussoil)
#'
#' ## Check that we have CONUSSoil data
#' has_CONUSSoil <- isTRUE(all(
#'   check_Miller1998_CONUSSoil(path = path_conussoil)
#' ))
#'
#' if (has_CONUSSoil) {
#'
#'   locations <- matrix(
#'     data = c(-120.1286878, -111.8511136, 39.8182913, 36.9047396),
#'     nrow = 2
#'   )
#'
#'   res <- extract_soils_Miller1998_CONUSSoil(
#'     x = locations,
#'     path = path_conussoil,
#'     verbose = TRUE
#'   )
#' }
#'
#' # Clean up example
#' unlink(script_to_download_conussoil)
#'
#'
#' @export
extract_soils_Miller1998_CONUSSoil <- function(
  x,
  crs = 4326,
  path,
  vars = c("bd", "rockvol", "sand", "clay", "silt"),
  lower_limits_by_vars = c(bd = 30, rockvol = 0, sand = 0, clay = 0, silt = 0),
  replace_missing_fragvol_with_zero = c("none", "all", "at_surface"),
  impute = FALSE,
  digits = 3L,
  verbose = FALSE
) {

  #--- Make sure inputs are correctly formatted
  var_depth <- "rockdepm"
  vars_all <- unique(c(var_depth, vars))
  var_stxt3 <- c("sand", "clay", "silt")
  var_stxt <- intersect(var_stxt3, vars_all)
  var_others <- setdiff(vars_all, var_stxt)

  if (!(var_depth %in% names(lower_limits_by_vars))) {
    lower_limits_by_vars <- c(rockdepm = 0, lower_limits_by_vars)
  }

  locations <- rSW2st::as_points(x, to_class = "sf", crs = crs)


  #--- Extract values
  res <- fetch_soils_from_Miller1998_CONUSSoil(
    x = locations,
    crs = crs,
    vars = vars_all,
    lower_limits_by_vars = lower_limits_by_vars,
    path = path,
    verbose = verbose
  )

  N_layers <- dim(res)[3]


  # Calculate restriction depth by >99% rock volume
  if ("rockvol" %in% colnames(res)) {
    tmp <- apply(
      res[, "rockvol", ],
      MARGIN = 1,
      FUN = function(x) {
        tmp <- x >= 99
        if (any(tmp, na.rm = TRUE)) min(which(tmp), na.rm = TRUE) else NA
      }
    )

    depths <- c(0, depth_profile_Miller1998_CONUSSoil())
    solid_rock_restriction_cm <- depths[tmp]
  }


  # Convert to site + layer ~ variable format
  res <- reshape2::acast(reshape2::melt(res), Var1 + Var3 ~ Var2)

  # Assign site ids, layer depths, and layer number (per site)
  tmp <- strsplit(rownames(res), split = "_", fixed = TRUE)
  tmp <- matrix(
    data = as.integer(unlist(tmp)),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("id", "layer_depth"))
  )

  res <- data.frame(
    tmp,
    Horizon_No = rep(seq_len(N_layers), times = nrow(locations)),
    res
  )

  if ("rockvol" %in% colnames(res)) {
    res[, "rockvol_depth"] <- rep(solid_rock_restriction_cm, each = N_layers)
  }



  #--- Deduce soil texture iff one of three values is missing
  if (all(var_stxt3 %in% colnames(res))) {
    res <- rSW2data::deduce_complete_soil_texture(
      x = res,
      var_stxt = var_stxt3,
      val_total = 100,
      ignore_le = 5
    )
  }


  #--- Interpret missing values for rock/gravel fragments as 0 %
  if ("rockvol" %in% colnames(res)) {
    replace_missing_fragvol_with_zero <-
      match.arg(replace_missing_fragvol_with_zero)

    res <- rSW2data::set_missing_soils_to_value(
      x = res,
      variable = "rockvol",
      value = 0,
      where = replace_missing_fragvol_with_zero,
      verbose = verbose
    )
  }



  #--- Estimate soil bulk density if missing
  # Cannot estimate density from porosity because porosity was calculated
  # from density (Miller et al. 1998)


  #--- Calculate soil depth of profile (per site id)
  # and convert depth table to wide-format for output
  if (all(var_stxt3 %in% colnames(res))) {
    locs_table_depths <- calculate_soil_depth_NRCS(
      x = res,
      target_site_ids = seq_len(nrow(locations)),
      restrict_by_ec_or_ph = FALSE,
      var_site_id = "id",
      var_horizon = "Horizon_No",
      var_horizon_lower_depth = "layer_depth",
      var_restrictions = c(
        "rockdepm",
        if ("rockvol_depth" %in% colnames(res)) "rockvol_depth"
      ),
      var_soiltexture = var_stxt3
    )


    # Transfer final soil depth and (potentially adjusted depth_L1)
    ids <- match(res[, "id"], rownames(locs_table_depths))
    res[, "SoilDepth_cm"] <- locs_table_depths[ids, "SoilDepth_cm"]
    res[, "N_horizons"] <- locs_table_depths[ids, "N_horizons"]

    is_shallowest <- res[, "Horizon_No"] == 1
    res[is_shallowest, "layer_depth"] <-
      locs_table_depths[ids[is_shallowest], "depth_L1"]

  } else {
    warning(
      "Soil depth was not adjusted ",
      "because soil texture variables were not extracted."
    )

    #--- Set (fixed) soil depth of profile in wide-format for output
    layer_depths <- depth_profile_Miller1998_CONUSSoil()

    locs_table_depths <- cbind(
      N_horizons = N_layers,
      SoilDepth_cm = res[res[, "Horizon_No"] == 1, "rockdepm"],
      matrix(
        data = layer_depths,
        nrow = nrow(locations),
        ncol = N_layers,
        byrow = TRUE,
        dimnames = list(NULL, paste0("depth_L", seq_len(N_layers)))
      )
    )
  }


  #--- Last step: impute remaining missing values per location
  # by shallow-depth value carried deeper (in analogy to LOCF)
  # but do not impute missing values in the shallowest horizon
  if (impute) {
    res <- rSW2data::impute_soils(
      x = res,
      var_site_id = "id",
      var_horizon = "Horizon_No",
      var_values = vars,
      verbose = verbose
    )
  }



  #--- Convert units & rounding
  # Convert % to fraction
  var_pct_to_fraction <- intersect(
    c("bd", "rockvol", "sand", "clay", "silt"),
    colnames(res)
  )
  res[, var_pct_to_fraction] <- res[, var_pct_to_fraction] / 100

  # Round
  if (is.finite(digits)) {

    if (all(var_stxt3 %in% colnames(res))) {
      has_vals <-
        complete.cases(res[, var_stxt3]) &
        apply(res[, var_stxt3, drop = FALSE], 1, sum, na.rm = TRUE) > 0

      res[has_vals, var_stxt3] <- rSW2utils::scale_rounded_by_sum(
        x = res[has_vals, var_stxt3],
        digits = digits,
        icolumn_adjust = 3
      )

      var_others2 <- var_others

    } else {
      var_others2 <- c(var_others, var_stxt)
    }

    var_others2 <- intersect(var_others2, colnames(res))
    res[, var_others2] <- round(res[, var_others2], digits)
  }


  #--- Create texture table
  # Convert to wide format (one row for each point location)
  locs_table_texture <- reshape2::acast(
    data = reshape2::melt(
      data = cbind(
        res[, c("Horizon_No", "id", vars)]
      ),
      id.vars = c("Horizon_No", "id")
    ),
    formula = id ~ Horizon_No + variable
  )

  colnames(locs_table_texture) <- sapply(
    X = strsplit(colnames(locs_table_texture), split = "_"),
    FUN = function(x) paste0(x[2], "_L", x[1])
  )



  #--- Return tables
  list(
    ref = create_reference_for_Miller1998_CONUSSoil(),
    table_depths = locs_table_depths,
    table_texture = locs_table_texture
  )
}
