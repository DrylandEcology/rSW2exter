create_reference_for_SOLUS100 <- function() {
  paste0(
    "Nauman, T. 2024. ",
    "Data from: Soil Landscapes of the United States 100-meter (`SOLUS100`) ",
    "soil property maps project repository. Ag Data Commons.",
    "https://doi.org/10.15482/USDA.ADC/25033856.V1.",
    "Accessed [", format(Sys.Date(), "%Y-%b-%e"), "]"
  )
}

#' List of variables available from `SOLUS100`
#'
#' @section Organic matter:
#' We provide `SOLUS100` soil organic carbon `soc` as mass fraction `[0-1]`.
#' The relationship with `SSURGO` organic matter is `soc = 0.58 * om` where
#' `om` is "decomposed plant and animal residue expressed as a" mass fraction
#' "of the less than 2 mm soil material".
#'
#' @md
#' @export
variables_SOLUS100 <- function() {
  data.frame(
    type = c("depth", "depth", rep("property", 18L)),
    name = c(
      "anylithicdpt_cm", "resdept_all_cm",
      "caco3",
      "cec7",
      "claytotal",
      "dbovendry",
      "ec", "ecec",
      "fragvol",
      "gypsum",
      "ph1to1h2o",
      "sandco", "sandfine", "sandmed", "sandtotal", "sandvc", "sandvf",
      "sar",
      "silttotal",
      "soc"
    ),
    scaling_factor = c(
      1, 1, # depth
      100, # caco3 [% -> fraction]
      10, # cec7,
      100, # claytotal [% -> fraction]
      100, # dbovendry
      10, 10, # ec, ecec
      100, # fragvol [% -> fraction]
      10 * 100, # gypsum [% -> fraction]
      100, # ph1to1h2o
      rep(100, 6L), # sand* [% -> fraction]
      1, # sar
      100, # silttotal [% -> fraction]
      1000 * 100 # soc [% -> fraction]
    ),
    stringsAsFactors = FALSE
  )
}


#' List of soil depths available from `SOLUS100`
#' @md
#' @export
depth_profile_SOLUS100 <- function() {
  c(0L, 5L, 15L, 30L, 60L, 100L, 150L)
}


#' Compose a file name of `SOLUS100`
#'
#' @param vars A vector of variable names. See [variables_SOLUS100()]
#' @param depths Soil depths in centimeters from surface.
#' See [depth_profile_SOLUS100()]
#' @param stat A vector of character strings. See Nauman et al. 2024
#'
#' @return A vector of file names.
#'
#' @md
#' @export
filenames_SOLUS100 <- function(vars, depths, stat) {
  tmp <- variables_SOLUS100()
  tmp <- tmp[tmp[["type"]] == "depth", "name", drop = TRUE]
  vars_depth <- intersect(tmp, vars)
  vars_bylayer <- setdiff(vars, tmp)

  c(
    if (length(vars_depth) > 0L) paste0(vars_depth, "_", stat, ".tif"),
    if (length(vars_bylayer) > 0L) {
      vapply(
        depths,
        function(d) paste0(vars_bylayer, "_", d, "_cm_", stat, ".tif"),
        FUN.VALUE = rep(NA_character_, times = length(vars_bylayer))
      )
    }
  )
}


#' Check local copy of `SOLUS100`
#'
#' @param path A character string. The path to the local copy of `SOLUS100`.
#' @inheritParams filenames_SOLUS100
#'
#' @references
#' Nauman, T. 2024.
#' Data from: Soil Landscapes of the United States 100-meter (`SOLUS100`)
#' soil property maps project repository. Ag Data Commons.
#' <https://doi.org/10.15482/USDA.ADC/25033856.V1>.
#'
#' @seealso [download_SOLUS100()]
#'
#' @examples
#' dir_tmp <- tempdir()
#' has_solus <- check_SOLUS100(dir_tmp, vars = "resdept_all_cm")
#'
#' \dontrun{
#' if (curl::has_internet()) {
#'   fns_solus <- download_SOLUS100(dir_tmp, vars = "resdept_all_cm")
#'   files_solus <- file.path(dir_tmp, fns_solus)
#'   terra::plot(terra::rast(files_solus))
#'   has_solus <- check_SOLUS100(dir_tmp, vars = "resdept_all_cm")
#'
#'   unlink(files_solus) # clean up
#' }
#' }
#'
#' @md
#' @export
check_SOLUS100 <- function(
  path = ".",
  vars = c(
    "resdept_all_cm",
    "dbovendry", "fragvol", "sandtotal", "silttotal", "claytotal", "soc"
  ),
  depths = depth_profile_SOLUS100(),
  stat = c("p", "l", "h", "rpi")
) {
  vars <- intersect(vars, variables_SOLUS100()[["name"]])
  depths <- intersect(depths, depth_profile_SOLUS100())
  stat <- match.arg(stat)

  #--- Put together requested layer names
  requested_filenames <- filenames_SOLUS100(vars, depths, stat)

  #--- Check which requested layers are (not) already downloaded
  res <- file.exists(file.path(path, requested_filenames))
  names(res) <- requested_filenames
  res
}


#' Download soil layers from `SOLUS100`
#'
#' @inheritParams filenames_SOLUS100
#' @inheritParams check_SOLUS100
#' @param url_solus100 The `URL` to the `SOLUS100` data repository.
#' See Nauman et al. 2024
#' @param verbose A logical value.
#'
#' @return File names to local copies of requested soil layers.
#'
#' @references
#' Nauman, T. 2024.
#' Data from: Soil Landscapes of the United States 100-meter (`SOLUS100`)
#' soil property maps project repository. Ag Data Commons.
#' <https://doi.org/10.15482/USDA.ADC/25033856.V1>.
#'
#' @seealso [check_SOLUS100()]
#'
#' @examples
#' \dontrun{
#' if (curl::has_internet()) {
#'   dir_tmp <- tempdir()
#'   fsolus <- download_SOLUS100(dir_tmp, vars = "resdept_all_cm")
#'   terra::plot(terra::rast(file.path(dir_tmp, fsolus)))
#'   unlink(file.path(dir_tmp, fsolus)) # clean up
#' }
#' }
#'
#' @md
#' @export
download_SOLUS100 <- function(
  path = ".",
  vars = c(
    "resdept_all_cm",
    "dbovendry", "fragvol", "sandtotal", "silttotal", "claytotal", "soc"
  ),
  depths = depth_profile_SOLUS100(),
  stat = c("p", "l", "h", "rpi"),
  url_solus100 = "https://storage.googleapis.com/solus100pub/",
  verbose = FALSE
) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  vars <- intersect(vars, variables_SOLUS100()[["name"]])
  depths <- intersect(depths, depth_profile_SOLUS100())
  stat <- match.arg(stat)

  #--- Check which requested layers are (not) already downloaded
  has_filenames <- check_SOLUS100(path, vars, depths, stat)
  requested_filenames <- names(has_filenames)

  todo_filenames <- requested_filenames[!has_filenames]


  #--- Download needed files
  Nall <- length(requested_filenames)
  Ntodo <- length(todo_filenames)

  if (verbose) {
    pb <- utils::txtProgressBar(max = Nall, style = 3L)
    vpbi <- Nall - Ntodo
    utils::setTxtProgressBar(pb, value = vpbi)
  }

  for (k in seq_len(Ntodo)) {
    try(
      utils::download.file(
        url = paste0(url_solus100, todo_filenames[[k]]),
        destfile = file.path(path, todo_filenames[[k]]),
        quiet = TRUE
      )
    )

    if (verbose) utils::setTxtProgressBar(pb, value = vpbi + k)
  }

  if (verbose) close(pb)

  intersect(
    list.files(path, pattern = ".tif$", recursive = FALSE),
    requested_filenames
  )
}



#' Extract soil information from the `SOLUS100` soil dataset
#'
#' @inheritParams check_SOLUS100
#' @inheritParams rSW2st::as_points
#' @param fun A function. Summarizing gridcell values if more than one value
#'   is extracted per location. See [terra::extract()].
#' @param na.rm A logical value. Passed to `fun`.
#' @param verbose A logical value.
#'
#' @section Notes: This is a function with minimal functionality;
#' use [extract_soils_SOLUS100()] for a user-friendly interface.
#'
#' @references
#' Nauman, T. 2024.
#' Data from: Soil Landscapes of the United States 100-meter (`SOLUS100`)
#' soil property maps project repository. Ag Data Commons.
#' <https://doi.org/10.15482/USDA.ADC/25033856.V1>.
#'
#' @md
#' @export
fetch_soils_from_SOLUS100 <- function(
  x,
  vars,
  depths,
  stat,
  path = ".",
  fun = NULL,
  na.rm = TRUE,
  verbose = FALSE
) {
  #--- Prepare result object
  res <- array(
    NA,
    dim = c(nrow(x), length(vars), length(depths)),
    dimnames = list(NULL, vars, depths)
  )

  #--- Extract values
  for (iv in seq_along(vars)) {
    ftmp <- file.path(path, filenames_SOLUS100(vars[iv], depths, stat))

    has <- file.exists(ftmp)

    if (!all(has)) {
      stop(
        "SOLUS100 data ",
        toString(shQuote(basename(ftmp[!has]))), " not found.",
        call. = FALSE
      )
    }

    list_args <- list(
      x = terra::rast(ftmp),
      y = x,
      ID = FALSE,
      raw = TRUE
    )

    if (!is.null(fun)) {
      list_args <- c(list_args, list(fun = fun, na.rm = na.rm))
    }

    tmp <- do.call(terra::extract, args = list_args)

    res[, iv, seq_along(ftmp)] <- data.matrix(tmp)
  }

  res
}



#' Extract soil information from the `SOLUS100` soil dataset
#' for \pkg{SOILWAT2} applications
#'
#' @inheritParams check_SOLUS100
#' @inheritParams fetch_soils_from_SOLUS100
#' @inheritParams rSW2st::as_points
#' @param var_depth A character string or `NULL`.
#' If `NULL`, then soil depth is determined based on
#'    (i) available data layers `depths` (up to 201 cm; see Nauman et al. 2024),
#'    (ii) depth of non-missing values in extracted `vars` (which may be 0).
#' Otherwise, soil depth is the extracted value of the `SOLUS100`
#' variable `var_depth`.
#' Soil depth is set to 0 if is missing or all `vars` values are missing.
#' @param method_vertical A character string that is
#' `"asis"` or `"interpolate_by_layer"`:
#'   (i) method `"asis"` extracts the values as provided by `SOLUS100`,
#'       i.e., as point estimates at specified depths
#'   (ii) method `"interpolate_by_layer"` interpolates extracted values
#'       for each centimeter depth increment and averages
#'       the interpolated values across requested soil layers
#'       (see `requested_layer_depths`).
#'       Soils properties for a soil with a missing depth value or a
#'       depth of less than 1 centimeter are set to `NA`.
#' @param requested_layer_depths An integer vector
#' (used if `method_vertical = "interpolate_by_layer"`).
#' Soil depths (in centimeters) at lower layer boundaries used for output
#' If `NULL`, then layer boundaries are assumed to be `c(depths, 201)`.
#' @param method_horizontal A character string.
#' Method that determines the extraction approach across grid cells:
#'   (i) values are extracted using arguments
#'       `buffer_m`, `fun`, and `na.rm`
#'       and are returned `"asis"` or
#'   (ii) values are extracted for point locations,
#'       i.e., temporarily setting `buffer_m = NULL`; then,
#'       sites with problematic values (as determined by `fix_criteria`)
#'       are extracted again under `"fix_with_buffer"` based on
#'       `buffer_m`, `fun`, and `na.rm`
#' @param fix_criteria A named list
#' (used if `method_horizontal = "fix_with_buffer"`).
#' Names match values of `vars` or one of the names can be `"texture"`
#' (in which case the criterion is applied to sand, clay, and silt).
#' Each element is applied to the variable that corresponds to the name
#' and is used to determine whether a site has problematic values.
#' Elements themselves are a named list with two sub-elements
#' `"op"` for the relationship operator, e.g., `"<"`, and
#' `"value"` for the value to compare against. See examples.
#' @param buffer_m A numeric value. The radius of a buffer around each point
#'   from which to extract cell values and across which `fun` is applied.
#'   Set to `NULL` to extract `SOLUS100` gridcell values at point locations.
#' @param fun A function or a named list containing functions
#' (used if `method_horizontal = "fix_with_buffer"`).
#' Names match values of `vars` or one of the names can be `"texture"`
#' (in which case the criterion is applied to sand, clay, and silt).
#' The function(s) summarize(s) extracted values if more than one value
#' is extracted per location (based on `buffer_m`).
#' @param digits An integer value. The number of digits to which soil texture
#' variables are rounded. Skip rounding if `NA` or `NULL`.
#'
#' @section Notes: A local copy of `SOLUS100` is required. The function
#' [download_SOLUS100()] can be used to download `SOLUS100` files.
#'
#' @references
#' Nauman, T. 2024.
#' Data from: Soil Landscapes of the United States 100-meter (`SOLUS100`)
#' soil property maps project repository. Ag Data Commons.
#' <https://doi.org/10.15482/USDA.ADC/25033856.V1>.
#'
#' @seealso [terra::extract()]
#'
#' @examples
#' \dontrun{
#' if (curl::has_internet()) {
#' path_solus100 <- tempdir()
#' req_vars <- c("resdept_all_cm", "sandtotal")
#' req_depths <- 0
#'
#' ## Download data
#' fns_solus100 <- rSW2exter::download_SOLUS100(
#'   path = path_solus100,
#'   vars = req_vars,
#'   depths = req_depths
#' )
#'
#' ## Check that we have SOLUS100 data
#' has_SOLUS100 <- isTRUE(all(
#'   check_SOLUS100(
#'     path = path_solus100,
#'     vars = req_vars,
#'     depths = req_depths
#'   )
#' ))
#'
#' if (has_SOLUS100) {
#'   locations <- matrix(
#'     data = c(-120.1286878, -111.8511136, 39.8182913, 36.9047396),
#'     nrow = 2
#'   )
#'
#'   ## Extract gridcell values at point locations
#'   res <- extract_soils_SOLUS100(
#'     x = locations,
#'     vars = req_vars,
#'     depths = req_depths,
#'     path = path_solus100
#'   )
#' }
#'
#' # Clean up example
#' unlink(file.path(path_solus100, fns_solus100))
#' }
#' }
#'
#' @md
#' @export
extract_soils_SOLUS100 <- function(
  x,
  crs = 4326,
  vars = c(
    "dbovendry", "fragvol", "sandtotal", "silttotal", "claytotal", "soc"
  ),
  var_depth = "resdept_all_cm",
  depths = depth_profile_SOLUS100(),
  stat = c("p", "l", "h", "rpi"),
  path = ".",
  method_vertical = c("asis", "interpolate_by_layer"),
  requested_layer_depths = NULL,
  method_horizontal = c("asis", "fix_with_buffer"),
  fix_criteria = list(
    dbovendry = list(op = "<", value = 0.6),
    texture = list(op = "<", value = 0.5)
  ),
  buffer_m = NULL, fun = NULL, na.rm = TRUE,
  digits = 3L,
  verbose = FALSE
) {

  vars_solus100 <- variables_SOLUS100()
  max_depth <- 201L # see Nauman et al. 2024

  #--- * Make sure inputs are correctly formatted ------
  stat <- match.arg(stat)
  method_vertical <- match.arg(method_vertical)
  method_horizontal <- match.arg(method_horizontal)

  var_depth <- var_depth[[1L]]
  tmp <- intersect(c(var_depth, vars), vars_solus100[["name"]])
  if (!setequal(tmp, vars)) {
    warning(
      "Ignoring requested variables ",
      toString(shQuote(setdiff(vars, vars_solus100[["name"]]))),
      " that are not provided by SOLUS100.",
      call. = FALSE
    )
  }
  vars <- tmp

  has_rld <- length(requested_layer_depths) > 0L
  requested_layer_depths <- sort(as.integer(requested_layer_depths))
  if (any(requested_layer_depths <= 0L)) {
    warning(
      "Ignoring requested soil layers shallower than minimum depth of 1 cm.",
      call. = FALSE
    )
    ids <- requested_layer_depths > 0L
    requested_layer_depths <- requested_layer_depths[ids]
  }
  if (any(requested_layer_depths > max_depth)) {
    warning(
      "Ignoring requested soil layers deeper than maximum depth of ",
      max_depth, " cm; assigning maximum depth as boundary of deepest layer.",
      call. = FALSE
    )
    ids <- requested_layer_depths <= max_depth
    requested_layer_depths <- c(requested_layer_depths[ids], max_depth)
  }
  if (has_rld && length(requested_layer_depths) == 0L) {
    stop("Failed to process 'requested_layer_depths'.", call. = FALSE)
  }

  depths <- sort(as.integer(depths))
  tmp <- intersect(depths, depth_profile_SOLUS100())
  if (!setequal(tmp, depths)) {
    warning(
      "Ignoring requested depths ",
      toString(shQuote(setdiff(depths, depth_profile_SOLUS100()))),
      " that are not provided by SOLUS100.",
      call. = FALSE
    )
  }
  depths <- tmp

  #--- * Identify variables ------
  has_solus100 <- check_SOLUS100(path, vars, depths, stat)
  stopifnot(has_solus100)

  tmp <- vars_solus100[vars_solus100[["type"]] == "depth", "name", drop = TRUE]
  vars_depth <- intersect(tmp, vars)
  vars_bylayer <- setdiff(vars, tmp)

  var_stxt3 <- c("sandtotal", "silttotal", "claytotal")
  var_stxt <- intersect(var_stxt3, vars)
  var_others <- setdiff(vars, c(var_stxt, vars_depth))

  #--- * Transform locations to CRS of SOLUS100 ------
  locations <- sf::st_transform(
    rSW2st::as_points(x, to_class = "sf", crs = crs),
    sf::st_crs(
      terra::rast(
        file.path(path, filenames_SOLUS100(vars[[1L]], depths[[1L]], stat))
      )
    )
  )

  N_sites <- nrow(locations)

  #--- * Extract values from SOLUS100 ------
  res <- fetch_soils_from_SOLUS100(
    x = locations,
    vars = vars,
    depths = depths,
    stat = stat,
    path = path,
    fun = if (identical(method_horizontal, "fix_with_buffer")) NULL else fun,
    na.rm = na.rm,
    verbose = verbose
  )


  #--- * Replace sites with problematic values by buffered extractions ------
  if (identical(method_horizontal, "fix_with_buffer")) {
    locs_buffered <- sf::st_buffer(locations, dist = buffer_m)

    # Determine for which variables we have criteria to determine problems
    tmp <- intersect(c(vars, "texture"), names(fix_criteria))
    ok <- vapply(
      X = fix_criteria[tmp],
      FUN = function(x) all(c("op", "value") %in% names(x)),
      FUN.VALUE = NA
    )
    check_vars <- tmp[ok]

    # Is `fix_criteria` well formed?
    if (!all(ok)) {
      warning(
        "Cannot apply `fix_with_buffer` for ",
        toString(shQuote(tmp[!ok])),
        " because of incomplete criteria.",
        call. = FALSE
      )
    }


    # Determine whether we have one `fun` to be applied to all fixes or
    # separate `fun`s
    one_fun <- !is.list(fun) && is.function(try(match.fun(fun), silent = TRUE))
    ok <- if (one_fun) TRUE else check_vars %in% names(fun)

    # Is `fun` well formed?
    if (!all(ok)) {
      warning(
        "Cannot apply `fix_with_buffer` for ",
        toString(shQuote(tmp[!ok])),
        " because of missing summarizing function `fun`.",
        call. = FALSE
      )
    }


    # Fix for texture variables
    if ("texture" %in% check_vars) {
      hasnot_texture <- !(var_stxt3 %in% vars)

      if (any(hasnot_texture)) {
        warning(
          "Cannot apply `fix_with_buffer` for `texture` because of ",
          "missing texture variables: ",
          toString(shQuote(var_stxt3[hasnot_texture])),
          call. = FALSE
        )

      } else {
        tmp <- fix_criteria[["texture"]]

        is_missing <- apply(res[, var_stxt3, , drop = FALSE], 1L, anyNA)

        is_bad_texture <- apply(
          X = apply(res[, var_stxt3, , drop = FALSE], c(1L, 3L), sum),
          MARGIN = 1L,
          FUN = function(x) {
            any(do.call(tmp[["op"]], args = list(x, tmp[["value"]])))
          }
        )

        ids_fix_with_buffer <- which(is_missing | is_bad_texture)

        if (length(ids_fix_with_buffer) > 0L) {
          res[ids_fix_with_buffer, var_stxt3, ] <- fetch_soils_from_SOLUS100(
            x = locs_buffered[ids_fix_with_buffer, , drop = FALSE],
            vars = var_stxt3,
            depths = depths,
            stat = stat,
            path = path,
            fun = if (one_fun) fun else fun[["texture"]],
            na.rm = na.rm,
            verbose = verbose
          )
        }
      }

      check_vars <- grep(
        "texture",
        x = check_vars,
        value = TRUE,
        invert = TRUE,
        fixed = TRUE
      )
    }

    # Fix for all other variables
    for (k in seq_along(check_vars)) {
      tmp <- fix_criteria[[check_vars[k]]]

      is_missing <- apply(res[, check_vars[k], , drop = FALSE], 1L, anyNA)

      is_bad <- apply(
        X = res[, check_vars[k], , drop = FALSE],
        MARGIN = 1L,
        FUN = function(x) {
          any(do.call(tmp[["op"]], args = list(x, tmp[["value"]])))
        }
      )

      ids_fix_with_buffer <- which(is_missing | is_bad)

      if (length(ids_fix_with_buffer) > 0L) {
        res[ids_fix_with_buffer, check_vars[k], ] <- fetch_soils_from_SOLUS100(
          x = locs_buffered[ids_fix_with_buffer, , drop = FALSE],
          vars = check_vars[k],
          depths = depths,
          stat = stat,
          path = path,
          fun = if (one_fun) fun else fun[[check_vars[k]]],
          na.rm = na.rm,
          verbose = verbose
        )
      }
    }
  }


  #--- * Apply scaling value ------
  vars_res <- dimnames(res)[[2L]]

  ids <- match(vars_res, vars_solus100[["name"]], nomatch = 0L)
  for (k in seq_along(vars_res)) {
    res[, k, ] <- res[, k, ] / vars_solus100[ids[[k]], "scaling_factor"]
  }


  #--- * Identify soil depth to nearest centimeter ------
  ids <- apply(
    res[, vars_bylayer, , drop = FALSE],
    MARGIN = 1L,
    FUN = function(x) min(rowSums(!is.na(x)))
  )

  if (length(vars_depth) > 0L) {
    tmp <- if (var_depth %in% vars) var_depth else vars_depth[[1L]]
    soildepth <- round(res[, tmp, 1L, drop = TRUE])
    # Set soil depth to 0 if missing or all properties are missing
    soildepth[is.na(soildepth) | ids == 0L] <- 0L

    res_depths <- array(
      round(res[, vars_depth, 1L]),
      dim = c(N_sites, length(vars_depth)),
      dimnames = list(NULL, vars_depth)
    )

  } else {
    soildepth <- c(depths[-1L], max_depth)[ids]
    res_depths <- NULL
  }


  #--- * Apply vertical method ------
  if (identical(method_vertical, "interpolate_by_layer")) {
    if (is.null(requested_layer_depths)) {
      requested_layer_depths <- setdiff(c(depths, max_depth), 0L)
    }

    dr <- dim(res)
    N_layers1 <- dr[[3L]]
    N_layers2 <- length(requested_layer_depths)

    # Add soil depth to array of extracted values
    tmp1 <- array(
      dim = c(dr[1:2], N_layers1 + 1L),
      dimnames = c(dimnames(res)[1:2], list(c(depths, "soildepth")))
    )
    tmp1[, , seq_along(depths)] <- res
    tmp1[, , length(depths) + 1L] <- soildepth

    # Interpolate in 1-cm depth intervals and average to requested soil layers
    # return NAs if soil depth is missing or less than 1 cm
    tmp2 <- apply(
      tmp1[, vars_bylayer, , drop = FALSE],
      MARGIN = c(1L, 2L),
      FUN = function(x) {
        # x[1:N_layers1] represents soil property
        # x[N_layers1 + 1L] represents soil depth
        res <- if (isTRUE(x[[N_layers1 + 1L]] >= 1L)) {
          md <- min(x[[N_layers1 + 1L]], max(requested_layer_depths))
          d1s <- seq.int(from = 1L, to = md, by = 1L)
          itvs <- findInterval(
            d1s,
            vec = c(0L, requested_layer_depths),
            left.open = TRUE
          )

          tmpa <- stats::approx(
            x = depths,
            y = x[seq_len(N_layers1)],
            xout = d1s,
            rule = c(1L, 2L) # NA if < 0, last value if > 150
          )

          tapply(tmpa[["y"]], itvs, mean)
        }

        c(res, rep(NA, times = N_layers2 - length(res)))
      }
    )

    res <- aperm(tmp2, perm = c(2L, 3L, 1L))
    rm(tmp1, tmp2)
    used_depths <- requested_layer_depths

  } else {
    res <- res[, vars_bylayer, , drop = FALSE] # remove depth from properties
    used_depths <- depths
  }


  #--- * Rounding ------
  N_layers <- dim(res)[[3L]]

  if (is.finite(digits)) {
    if (all(var_stxt3 %in% vars_res)) {
      for (k in seq_len(N_layers)) {
        has_vals <-
          complete.cases(res[, var_stxt3, k]) &
          rowSums(res[, var_stxt3, k, drop = FALSE], na.rm = TRUE) > 0

        res[has_vals, var_stxt3, k] <- rSW2utils::scale_rounded_by_sum(
          x = res[has_vals, var_stxt3, k],
          digits = digits,
          icolumn_adjust = 3
        )
      }

      var_others2 <- var_others

    } else {
      var_others2 <- c(var_others, var_stxt)
    }

    var_others2 <- intersect(var_others2, vars_res)
    res[, var_others2, ] <- round(res[, var_others2, ], digits)
  }


  #--- * Create soil depth output table ------
  tmpd <- matrix(
    data = used_depths,
    nrow = N_sites,
    ncol = N_layers,
    byrow = TRUE,
    dimnames = list(NULL, paste0("depth_L", seq_len(N_layers)))
  )

  if (identical(method_vertical, "interpolate_by_layer")) {
    #--- Set lowest layer depth to soil depth
    tmp2 <- cbind(soildepth, tmpd)

    # Identify layer index that contains soil depth
    L_at_soildepth <- apply(
      X = tmp2,
      MARGIN = 1L,
      FUN = function(x) {
        findInterval(x[[1L]], c(0L, na.exclude(x[-1L])), left.open = TRUE)
      }
    )
    ids <- which(
      !apply(
        X = tmp2,
        MARGIN = 1L,
        FUN = function(x) {
          x[[1L]] == 0L || all(is.na(x[-1])) || x[[1L]] %in% x[-1L]
        }
      )
    )

    # Update lower boundary of layer to be soil depth
    tmpd[cbind(ids, L_at_soildepth[ids])] <- soildepth[ids]
    tmpd[soildepth < tmpd] <- NA_integer_ # Set layers below soil depth to NA
  }


  #--- Put final table together
  locs_table_depths <- cbind(
    N_horizons = rowSums(!is.na(tmpd)), # Count number of soil layers
    SoilDepth_cm = soildepth,
    if (!is.null(res_depths)) res_depths,
    tmpd
  )


  #--- * Create texture output table ------

  # Convert to wide format (one row for each point location)
  locs_table_texture <- reshape2::acast(reshape2::melt(res), Var1 ~ Var3 + Var2)
  colnames(locs_table_texture) <- paste0(
    rep(vars_bylayer, times = N_layers),
    "_L",
    rep(seq_len(N_layers), each = length(vars_bylayer))
  )


  #--- Return tables
  list(
    ref = create_reference_for_SOLUS100(),
    table_depths = locs_table_depths,
    table_texture = locs_table_texture
  )
}
