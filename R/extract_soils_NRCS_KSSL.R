create_reference_for_KSSL <- function() {
  paste0(
    "National Cooperative Soil Survey, ",
    "United States Department of Agriculture. ",
    "National Cooperative Soil Survey Soil Characterization Database. ",
    "Available online at http://ncsslabdatamart.sc.egov.usda.gov/. ",
    "Accessed [",
    format(as.POSIXlt(Sys.Date()), "%Y-%b-%e"),
    "]"
  )
}

#' Variables from `KSSL` made available
#'
#' @md
#' @export
variables_KSSL <- function() {
  data.frame(
    type = c("depth", "depth", rep("property", 6L)),
    name = c(
      "hzn_top",
      "hzn_bot",
      "bulk_density_oven_dry",
      "total_frag_wt_pct_gt_2_mm_ws", # equal sum(wf_25, wf_520, wf_2075)?
      "sand_total",
      "clay_total",
      "silt_total",
      "estimated_organic_matter"
    ),
    newName = c(
      "hzdept",
      "hzdepb",
      "dbovendry",
      "fragvol", # equal sum(wf_25, wf_520, wf_2075)?
      "sandtotal",
      "claytotal",
      "silttotal",
      "om"
    ),
    scaling_factor = c(
      1, 1, # depth [cm]
      1, # dbovendry [g cm-3]
      100, # fragvol [% -> fraction] # TODO: weight or volume?
      100, # sand_total [% -> fraction]
      100, # clay_total [% -> fraction]
      100, # silt_total [% -> fraction]
      100 # som [% -> fraction] # TODO: weight or volume?
    ),
    stringsAsFactors = FALSE
  )
}

#' Fix several inconsistencies in horizon depths
#'
#' @param x A data frame with horizon depths and possibly other columns.
#' @param vars_depth A vector with two character strings. The column names of
#' `x` with the top horizon depths (first value) and the bottom horizon depths
#' (second value).
#'
#' @return A copy of `x` with possibly updated columns `vars_depth` and a new
#' column `"MethodFixedDepth"` that describes the applied fixes.
#'
#' @section Details:
#'   * `"Hz1Set0cmTop"`: Add 1 cm to bottom-most horizons with a
#'     missing (`NA`) bottom depth (see also `aqp::accumulateDepths`).
#'   * `"HzkDeepestCreated1cmBottom"`: Set a missing (`NA`) top depth of
#'     top-most horizon to 0.
#'   * `"HzkImputedMissingTopDepth"`, `"HzkImputedMissingBottomDepth"`:
#'     Set remaining missing (`NA`) depth values to non-missing adjacent value.
#'   * `"HzkResetBottomDepthToDeeperTopDepth(old=value)"`: Reset bottom depth
#'     of a horizon `k` where the bottom depth is deeper than
#'     the next horizon's (`k + 1`) bottom depth to that horizon's top depth.
#'   * `"HzkResetToAverageBottomTopDepths(old=value|value)"`:
#'     Recalculate bottom depth of horizon `k` and top depth of horizon `k + 1`
#'     if these values disagree but are within the top depth of horizon `k` and
#'     the bottom depth of horizon `k + 1`.
#'
#' @examples
#' x <- data.frame(hzn_top = c(NA, 3, 5, 10), hzn_bot = c(3, 5, 10, NA))
#' fixHorizonDepths(x)
#'
#' x <- data.frame(hzn_top = c(0, NA, 5, 10), hzn_bot = c(3, 5, 10, 15))
#' fixHorizonDepths(x)
#'
#' x <- data.frame(hzn_top = c(0, 0, 0, 0), hzn_bot = c(NA, NA, NA, NA))
#' fixHorizonDepths(x)
#'
#' x <- data.frame(hzn_top = c(0, 3, 5, 10), hzn_bot = c(3, 5, 20, 15))
#' fixHorizonDepths(x)
#'
#' x <- data.frame(hzn_top = c(0, 3, 7, 10), hzn_bot = c(3, 5, 10, 15))
#' fixHorizonDepths(x)
#'
#' @md
#' @export
fixHorizonDepths <- function(x, vars_depth = c("hzn_top", "hzn_bot")) {
  dx <- x[, vars_depth, drop = FALSE]
  nk <- nrow(x)

  # Set a missing top depth of top-most horizon to 0
  if (is.na(dx[1L, 1L])) {
    x[1L, "MethodFixedDepth"] <- toString(
      c(na.omit(x[1L, "MethodFixedDepth"]), "Hz1Set0cmTop")
    )
    dx[1L, 1L] <- 0L
  }

  # Add 1 cm to bottom-most horizons with a missing bottom depth
  if (is.na(dx[nk, 2L])) {
    x[nk, "MethodFixedDepth"] <- toString(
      c(
        na.omit(x[nk, "MethodFixedDepth"]),
        sprintf("Hz%dDeepestCreated1cmBottom", nk)
      )
    )
    dx[nk, 2L] <- if (any(nk == 0L, nk > 0L && isTRUE(dx[nk, 1L] > 0L))) {
      dx[nk, 1L] + 1
    } else {
      dx[nk, 1L]
    }
  }

  # Set remaining missing depth values to adjacent value
  if (anyNA(dx)) {
    ids <- which(apply(dx, 1L, anyNA))
    for (k in ids) {
      if (is.na(dx[k, 1L]) && k > 1L) {
        x[k, "MethodFixedDepth"] <- toString(
          c(
            na.omit(x[k, "MethodFixedDepth"]),
            sprintf("Hz%dImputedMissingTopDepth", k)
          )
        )
        dx[k, 1L] <- dx[k - 1L, 2L]
      }
      if (is.na(dx[k, 2L]) && k < nk) {
        x[k, "MethodFixedDepth"] <- toString(
          c(
            na.omit(x[k, "MethodFixedDepth"]),
            sprintf("Hz%dImputedMissingBottomDepth", k)
          )
        )
        dx[k, 2L] <- dx[k + 1L, 1L]
      }
    }
  }

  # Reset bottom depth of a horizon where the bottom depth is deeper than
  # the next horizon's bottom depth to that horizon's top depth
  ids <- which(dx[-nk, 2L] > dx[-1L, 2L])
  if (length(ids) > 0L) {
    x[ids, "MethodFixedDepth"] <- vapply(
      ids,
      function(k) {
        toString(
          c(
            na.omit(x[k, "MethodFixedDepth"]),
            sprintf(
              "Hz%dResetBottomDepthToDeeperTopDepth(old=%.0f)", k, dx[k, 2L]
            )
          )
        )
      },
      FUN.VALUE = NA_character_
    )
    dx[ids, 2L] <- dx[ids + 1L, 1L]
  }

  # "HzkResetToAverageBottomTopDepths"
  # Recalculate bottom depth of horizon `k` and top depth of horizon `k + 1`
  # if these values disagree but are within the top depth of horizon `k` and
  # the bottom depth of horizon `k + 1`
  ids <- which(dx[-nk, 2L] != dx[-1L, 1L])
  if (length(ids) > 0L) {
    x[ids, "MethodFixedDepth"] <- vapply(
      ids,
      function(k) {
        toString(
          c(
            na.omit(x[k, "MethodFixedDepth"]),
            sprintf(
              "Hz%dResetToAverageBottomTopDepths(old=%.0f|%.0f)",
              k, dx[k, 2L], dx[k + 1L, 1L]
            )
          )
        )
      },
      FUN.VALUE = NA_character_
    )
    newValues <- vapply(
      ids,
      function(k) round(mean(c(dx[k, 2L], dx[k + 1L, 1L]))),
      FUN.VALUE = NA_real_
    )
    dx[ids, 2L] <- newValues
    dx[ids + 1L, 1L] <- newValues
  }


  x[, vars_depth] <- dx
  x
}



#' Extract soil information from the `KSSL` soil dataset via `SDA`
#' for \pkg{SOILWAT2} applications
#'
#' @param x A vector of character strings. Soil identifier(s);
#' the type of identifier is specified by `what`.
#' @param what A character string.
#' `"pedlabsampnum"` (Laboratory Pedon ID), `"upedonid"` (User Pedon ID),
#' or `"pedon_key"` (Pedon ID).
#' see [`variables_KSSL()`] for implemented variables.
#' @param replace_missing_fragvol_with_zero A logical value. Replace missing
#' values of coarse fragments with 0.
#' @param replace_missing_om_with_zero A character value. Replace missing
#' values of organic matter with 0.
#' @param fix_depths A logical value. Fix depth issues, see [fixHorizonDepths()]
#' @inheritParams extract_soils_NRCS_SDA
#'
#'
#' @seealso [soilDB::fetchLDM()]
#'
#' @examples
#' \dontrun{
#' if (curl::has_internet()) {
#' x <- c("04N0873", "10N1336", "10N1239", "02N0184")
#' res <- extract_soils_KSSL(x = x, what = "pedlabsampnum", fix_depths = TRUE)
#' }
#' }
#'
#' @md
#' @export
extract_soils_KSSL <- function(
  x,
  what = c("pedlabsampnum", "upedonid", "pedon_key"),
  replace_missing_fragvol_with_zero = FALSE,
  replace_missing_om_with_zero = FALSE,
  estimate_missing_bulkdensity = FALSE,
  fix_depths = FALSE,
  digits = 3L,
  verbose = FALSE
) {
  stopifnot(requireNamespace("soilDB"), curl::has_internet())

  vars_KSSL <- variables_KSSL()

  #--- * Make sure inputs are correctly formatted ------
  what <- match.arg(what)

  if (identical(replace_missing_fragvol_with_zero, "all")) {
    replace_missing_fragvol_with_zero <- TRUE
  }

  if (identical(replace_missing_fragvol_with_zero, "none")) {
    replace_missing_fragvol_with_zero <- FALSE
  }

  if (identical(replace_missing_om_with_zero, "all")) {
    replace_missing_om_with_zero <- TRUE
  }

  if (identical(replace_missing_om_with_zero, "none")) {
    replace_missing_om_with_zero <- FALSE
  }

  #--- * Identify variables ------
  vars_request <- vars_KSSL[["name"]]
  vars_request2 <- c(
    vars_request,
    "bulk_density_oven_dry_ws", # to convert frag by weight to fragvol
    if (isTRUE(estimate_missing_bulkdensity)) "theta_s"
  )

  tmp <- vars_KSSL[vars_KSSL[["type"]] == "depth", "name", drop = TRUE]
  vars_depth <- intersect(tmp, vars_request)
  vars_bylayer <- setdiff(vars_request, tmp)

  var_stxt3 <- c("sand_total", "silt_total", "clay_total")
  var_stxt <- intersect(var_stxt3, vars_request)
  var_others <- setdiff(vars_request, c(var_stxt, vars_depth))
  var_output <- vars_bylayer

  unit_id <- "pedon_key"
  var_hid <- c(unit_id, "layer_key", "layer_sequence")
  var_kid <- "row_id"
  var_keys <- c(
    "pedlabsampnum", "upedonid", unit_id,
    "site_key", "user_site_id",
    "longitude_std_decimal_degrees", "latitude_std_decimal_degrees" # WGS84
  )

  #--- * Identify queries ------
  queries <- if (is.null(dim(x))) {
    tmp <- data.frame(x = x)
    colnames(tmp) <- what
    tmp
  } else {
    x
  }

  N_queries <- nrow(queries)

  locs_keys <- data.frame(
    array(
      dim = c(N_queries, length(var_kid) + length(var_keys)),
      dimnames = list(NULL, c(var_kid, var_keys))
    ),
    MethodFixedDepth = NA_character_,
    MethodFixedProperties = NA_character_
  )
  locs_keys[["row_id"]] <- seq_len(N_queries)
  locs_keys[[what]] <- queries[[what]]


  #--- * Extract values from KSSL ------
  resKSSL <- suppressMessages(
    soilDB::fetchLDM(x = queries[, what, drop = TRUE], what = what)
  )

  stopifnot(identical(resKSSL@idcol, unit_id))

  tmp <- intersect(var_keys, colnames(resKSSL@site))
  ids <- match(queries[, what, drop = TRUE], resKSSL@site[[what]], nomatch = 0L)
  locs_keys[ids > 0L, tmp] <- resKSSL@site[ids, tmp, drop = FALSE]


  res_soils <- data.frame(
    Horizon_No = NA_integer_,
    Horizon_depth = NA_real_,
    array(
      dim = c(nrow(resKSSL@horizons), length(var_hid) + length(vars_request2)),
      dimnames = list(NULL, c(var_hid, vars_request2))
    ),
    MethodFixedDepth = NA_character_,
    MethodFixedProperties = NA_character_
  )
  tmp <- c(var_hid, intersect(vars_request2, colnames(resKSSL@horizons)))
  res_soils[, tmp] <- resKSSL@horizons[, tmp, drop = FALSE]


  #--- * Identify (and fix) horizons ------
  ids <- order(res_soils[[unit_id]], res_soils[["layer_sequence"]])
  res_soils <- res_soils[ids, , drop = FALSE]

  tmp <- by(
    res_soils,
    INDICES = res_soils[[unit_id]],
    FUN = function(x) {
      x[["Horizon_No"]] <- seq_len(nrow(x))
      if (isTRUE(fix_depths)) x <- fixHorizonDepths(x, vars_depth)
      x[["Horizon_depth"]] <- max(x[, vars_depth, drop = FALSE])
      x
    }
  )
  res_soils <- do.call(rbind, args = tmp)



  #--- * Deduce soil texture iff one of three values is missing ------
  if (all(var_stxt3 %in% colnames(res_soils))) {
    ids <- which(apply(res_soils[, var_stxt3, drop = FALSE], 1L, anyNA))
    res_soils[ids, "MethodFixedProperties"] <- vapply(
      res_soils[ids, "MethodFixedProperties"],
      function(txt) toString(c(na.omit(txt), "Estimated1MissingSoilTexture")),
      FUN.VALUE = NA_character_
    )
    res_soils <- rSW2data::deduce_complete_soil_texture(
      x = res_soils,
      var_stxt = var_stxt3,
      val_total = 100,
      ignore_le = 5
    )
  }

  #--- * Calculate fragment volume ------
  if (
    all(
      c("bulk_density_oven_dry_ws", "total_frag_wt_pct_gt_2_mm_ws") %in%
        colnames(res_soils)
    )
  ) {
    # weight% = vol% * densityTotal / density
    res_soils[, "fragWeightPercent"] <-
      res_soils[, "total_frag_wt_pct_gt_2_mm_ws"]

    res_soils[, "total_frag_wt_pct_gt_2_mm_ws"] <-
      res_soils[, "fragWeightPercent"] *
      res_soils[, "bulk_density_oven_dry_ws"] / 2.65
  }

  #--- * Interpret missing values for rock/gravel fragments as 0 % ------
  if (
    all(
      "total_frag_wt_pct_gt_2_mm_ws" %in% colnames(res_soils),
      isTRUE(replace_missing_fragvol_with_zero)
    )
  ) {
    ids <- which(
      !is.finite(res_soils[, "total_frag_wt_pct_gt_2_mm_ws", drop = TRUE])
    )
    res_soils[ids, "MethodFixedProperties"] <- vapply(
      res_soils[ids, "MethodFixedProperties"],
      function(txt) toString(c(na.omit(txt), "SetMissingFragvol0")),
      FUN.VALUE = NA_character_
    )

    res_soils <- rSW2data::set_missing_soils_to_value(
      x = res_soils,
      variable = "total_frag_wt_pct_gt_2_mm_ws",
      value = 0,
      where = "all",
      horizon = "Horizon_No",
      verbose = verbose
    )
  }


  #--- * Interpret missing values for organic matter as 0 % ------
  if (
    all(
      "estimated_organic_matter" %in% colnames(res_soils),
      isTRUE(replace_missing_om_with_zero)
    )
  ) {
    ids <- which(
      !is.finite(res_soils[, "estimated_organic_matter", drop = TRUE])
    )
    res_soils[ids, "MethodFixedProperties"] <- vapply(
      res_soils[ids, "MethodFixedProperties"],
      function(txt) toString(c(na.omit(txt), "SetMissingOM0")),
      FUN.VALUE = NA_character_
    )

    res_soils <- rSW2data::set_missing_soils_to_value(
      x = res_soils,
      variable = "estimated_organic_matter",
      value = 0,
      where = "all",
      horizon = "Horizon_No",
      verbose = verbose
    )
  }

  #--- * Estimate soil bulk density if missing ------
  if (
    all(
      c("bulk_density_oven_dry", "theta_s") %in% colnames(res_soils),
      isTRUE(estimate_missing_bulkdensity)
    )
  ) {
    ids <- which(!is.finite(res_soils[, "bulk_density_oven_dry"]))

    if (length(ids) > 0L) {
      thetaSat <- res_soils[ids, "theta_s"]
      thetaSat[which(thetaSat > 1 | thetaSat < 0)] <- NA

      if (anyNA(thetaSat) && all(var_stxt3 %in% colnames(res_soils))) {
        idsnots <- which(is.na(thetaSat))
        ra <- soilDB::ROSETTA(
          x = res_soils[ids[idsnots], var_stxt3],
          vars = var_stxt3,
          v = "3"
        )
        thetaSat[idsnots] <- ra[, "theta_s"]
      }

      res_soils[ids, "bulk_density_oven_dry"] <-
        rSW2data::estimate_bulkdensity(
          theta_saturated = thetaSat,
          gravel_volume = res_soils[ids, "total_frag_wt_pct_gt_2_mm_ws"] / 100
        )

      res_soils[ids, "MethodFixedProperties"] <- vapply(
        res_soils[ids, "MethodFixedProperties"],
        function(txt) toString(c(na.omit(txt), "EstimatedBulkDensity")),
        FUN.VALUE = NA_character_
      )
    }
  }


  #--- * Apply scaling value ------
  vars_res <- dimnames(res_soils)[[2L]]

  ids <- match(vars_res, vars_KSSL[["name"]], nomatch = 0L)
  ids0 <- which(ids > 0L)

  for (k in ids0) {
    res_soils[, k] <- res_soils[, k] / vars_KSSL[ids[[k]], "scaling_factor"]
  }


  #--- * Calculate soil depth of profile (per unique soil unit) ------
  # and convert depth table to wide-format for output
  locs_table_depths <- calculate_soil_depth_NRCS(
    x = res_soils,
    target_site_ids = locs_keys[, unit_id, drop = TRUE],
    restrict_by_ec_or_ph = FALSE,
    var_site_id = unit_id,
    var_horizon = "Horizon_No",
    var_horizon_lower_depth = vars_depth[[2L]],
    var_restrictions = "Horizon_depth",
    var_soiltexture = var_stxt3
  )

  tmp <- aggregate(
    res_soils[["MethodFixedDepth"]],
    res_soils[unit_id],
    function(x) toString(na.omit(x))
  )
  ids <- match(locs_keys[, unit_id, drop = TRUE], tmp[[1L]], nomatch = 0L)
  locs_keys[ids > 0L, "MethodFixedDepth"] <- tmp[ids, 2L]

  # Transfer final soil depth and (potentially adjusted depth_L1)
  ids <- match(res_soils[[unit_id]], rownames(locs_table_depths))
  res_soils[, "SoilDepth_cm"] <- locs_table_depths[ids, "SoilDepth_cm"]
  res_soils[, "N_horizons"] <- locs_table_depths[ids, "N_horizons"]

  tmp <- res_soils[, "Horizon_No"] == 1L | res_soils[, "N_horizons"] == 0L
  is_shallowest <- tmp & !is.na(tmp)
  res_soils[is_shallowest, vars_depth[[2L]]] <-
    locs_table_depths[ids[is_shallowest], "depth_L1"]

  # Fix rownames of depth table
  rownames(locs_table_depths) <- locs_keys[, "row_id"]



  #--- * Rounding ------
  if (is.finite(digits)) {

    if (all(var_stxt3 %in% colnames(res_soils))) {
      has_vals <-
        complete.cases(res_soils[, var_stxt3]) &
        rowSums(res_soils[, var_stxt3, drop = FALSE], na.rm = TRUE) > 0

      res_soils[has_vals, var_stxt3] <- rSW2utils::scale_rounded_by_sum(
        x = res_soils[has_vals, var_stxt3],
        digits = digits,
        icolumn_adjust = 3
      )

      var_others2 <- setdiff(vars_bylayer, var_stxt3)

    } else {
      var_others2 <- vars_bylayer
    }

    var_others2 <- intersect(var_others2, colnames(res_soils))
    res_soils[, var_others2] <- round(res_soils[, var_others2], digits)
  }


  #--- * Create texture table ------
  # Rename variables
  tmp <- res_soils[, c("Horizon_No", unit_id, var_output)]
  ids <- match(colnames(tmp), vars_KSSL[["name"]], nomatch = 0L)
  colnames(tmp)[ids > 0L] <- vars_KSSL[["newName"]][ids]

  # Convert to wide format (one row for each point location)
  tmp_texture <- reshape2::acast(
    data = reshape2::melt(
      data = tmp,
      id.vars = c("Horizon_No", unit_id)
    ),
    formula = stats::as.formula(paste(unit_id, "~ Horizon_No + variable"))
  )

  ids <- match(locs_keys[, unit_id], rownames(tmp_texture), nomatch = NA)
  locs_table_texture <- tmp_texture[ids, , drop = FALSE]

  colnames(locs_table_texture) <- vapply(
    X = strsplit(colnames(locs_table_texture), split = "_", fixed = TRUE),
    FUN = function(x) paste0(x[[2L]], "_L", x[[1L]]),
    FUN.VALUE = NA_character_
  )
  rownames(locs_table_texture) <- locs_keys[, "row_id"]

  tmp <- aggregate(
    res_soils[["MethodFixedProperties"]],
    res_soils[unit_id],
    function(x) {
      res <- toString(na.omit(x))
      res <- strsplit(res, split = ", ", fixed = TRUE)[[1L]]
      toString(unique(res))
    }
  )
  ids <- match(locs_keys[, unit_id, drop = TRUE], tmp[[1L]], nomatch = 0L)
  locs_keys[ids > 0L, "MethodFixedProperties"] <- tmp[ids, 2L]


  #--- * Return tables ------
  list(
    ref = create_reference_for_KSSL(),
    table_keys = locs_keys,
    table_depths = locs_table_depths,
    table_texture = locs_table_texture
  )
}

