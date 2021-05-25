
create_reference_for_NRCS_SDA <- function() {
  paste0(
    "Soil Survey Staff, Natural Resources Conservation Service, ",
    "United States Department of Agriculture. Web Soil Survey. ",
    "Available online at https://websoilsurvey.nrcs.usda.gov/. ",
    "Accessed [",
    format(as.POSIXlt(Sys.Date()), "%Y-%b-%e"),
    "]"
  )
}



#' Check whether a \var{NRCS} soil horizon is organic
#'
#' Based on function \code{CheckTexture()}, version 2020-08-31, of the
#' \var{SoilDataDevelopmentToolbox}.
#'
#' @param x A two-dimensional character object. Required columns are
#'   \var{taxorder}, \var{taxsubgrp}, \var{desgnmaster}, \var{texture},
#'   and \var{lieutex}. Each horizon is represented by exactly one row.
#'
#' @return A logical vector representing each horizon. \code{TRUE} indicates
#'   that a horizon is considered organic; default is \code{FALSE}
#'   representing mineral soils. \code{NA}s in the input propagate.
#'
#' @references Code based on \var{CheckTexture()} version \var{2020-Aug-31} from
# nolint start
#'   \url{https://github.com/ncss-tech/SoilDataDevelopmentToolbox/blob/master/SDA_Valu2Table.py}
# nolint end
#'
#' @examples
#' x <- data.frame(
#'   taxorder = c("Histosols", "x", "x", "x", "x", "x", NA),
#'   taxsubgrp = c("x", "histic", "x", "x", "x", "x", NA),
#'   desgnmaster = c("L", "L", "O", "x", "x", "x", NA),
#'   texture = c("x", "x", "x", "CE", "x", "x", NA),
#'   lieutex = c("x", "x", "x", "x", "Muck", "x", NA)
#' )
#'
#' cbind(
#'   organic = is_NRCS_horizon_organic(x),
#'   expected_organic = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, NA)
#' )
#'
#' \dontrun{
#' if (curl::has_internet()) {
#'   x <- fetch_soils_from_NRCS_SDA(mukeys_unique = c(471168, 1606800))
#'   is_NRCS_horizon_organic(x)
#' }
#' }
#'
#' @export
is_NRCS_horizon_organic <- function(x) {

  vars <- c("taxorder", "taxsubgrp", "desgnmaster", "texture", "lieutex")
  cns <- colnames(x)

  for (var in vars) {
    if (!(var %in% cns)) {
      stop(shQuote(var), " is a required column name, but cannot be found.")
    }
  }

  lieuList <- c(
    "Slightly decomposed plant material",
    "Moderately decomposed plant material",
    "Highly decomposed plant material",
    "Undecomposed plant material",
    "Muck",
    "Mucky peat",
    "Peat",
    "Coprogenous earth"
  )

  txList <- c(
    "CE", "COP-MAT", "HPM", "MPM", "MPT", "MUCK", "PDOM", "PEAT",
    "SPM", "UDOM"
  )

  # propagate NAs conservatively
  is_mineral <-
    x[, "taxorder"] == "Histosols" |
    grepl("histic", x[, "taxsubgrp"], ignore.case = TRUE)

  is_organic <- cbind(
    desgnmaster = x[, "desgnmaster"] %in% c("O", "L"),
    texture = x[, "texture"] %in% txList,
    lieutex = x[, "lieutex"] %in% lieuList
  )
  is_organic[is.na(x[, c("desgnmaster", "texture", "lieutex")])] <- NA

  !is_mineral & apply(is_organic, 1, any)
}


#' Determine soil depth for \var{NRCS} soil data
#'
#' @param x A \code{data.frame} or \code{matrix}.
#'   Soil horizons/layers are organized in rows
#'   and soil texture variables in columns.
#' @param target_site_ids A vector. The unique location identifiers, e.g.,
#'   \var{cokey} values, for which soil depth is to be determined.
#' @param restrict_by_ec_or_ph A logical value. Include depth restrictions
#'   by \code{ph <= 3.5} or \code{ec >= 16}. This option requires additional
#'   variables.
#' @param var_site_id A character string. The column name of \code{x} which
#'   contains the unique location identifiers.
#' @param var_horizon A character string. The column name of \code{x} which
#'   contains the horizon/layer numbers where the shallowest horizon/layer
#'   is number one.
#' @param var_horizon_lower_depth A character string. The column name of
#'   \code{x} that contains the lower depth limit of horizons/layers.
#' @param var_restrictions A vector of character strings. The column names of
#'   \code{x} that contain depth restrictions, e.g., bedrock.
#' @param var_soiltexture A vector of character strings. The column names of
#'   \code{x} that contain the three soil texture variables sand, clay,
#'   and silt.
#'
#'
#' @return A \code{data.frame} with at least three columns. Each row represents
#'   one value of \code{target_cokeys}. The columns are: \itemize{
#'     \item \var{N_horizons}: The number of soil horizons/layers.
#'     \item \var{SoilDepth_cm}: The soil depth in centimeters.
#'     \item \var{depth_Lx}: The lower depth of soil horizon/layer \var{x}.
#'       Note: \var{depth_L1} may vary from \code{x[, "hzdepb_r"]}.
#'   }
#'
#' @references Code based on \var{CalcRZDepth()} version 2020-08-31:
# nolint start
#'   \url{https://github.com/ncss-tech/SoilDataDevelopmentToolbox/blob/master/SDA_Valu2Table.py}
# nolint end
#'   Note: currently ignores "dense" layer restrictions
#'
#' @examples
#' \dontrun{
#' if (curl::has_internet()) {
#'   var_stxt3 <- c("sandtotal_r", "claytotal_r", "silttotal_r")
#'
#'   x <- fetch_soils_from_NRCS_SDA(mukeys_unique = c(471168, 1606800))
#'
#'   calculate_soil_depth_NRCS(
#'     x,
#'     restrict_by_ec_or_ph = FALSE,
#'     var_soiltexture = var_stxt3
#'   )
#'
#'   x2 <- cbind(x, organic = is_NRCS_horizon_organic(x))
#'   calculate_soil_depth_NRCS(
#'     x2,
#'     restrict_by_ec_or_ph = TRUE,
#'     var_soiltexture = var_stxt3
#'   )
#' }
#' }
#'
#' @export
calculate_soil_depth_NRCS <- function(
  x,
  target_site_ids,
  restrict_by_ec_or_ph = TRUE,
  var_site_id = "COKEY",
  var_horizon = "Horizon_No",
  var_horizon_lower_depth = "hzdepb_r",
  var_restrictions =
    c("Horizon_depth", "RootZoneRestriction_depth", "Bedrock_depth"),
  var_soiltexture = c("sand", "clay", "silt")
) {

  vars <- c(
    var_site_id,
    var_horizon, var_horizon_lower_depth,
    var_restrictions,
    var_soiltexture
  )

  if (restrict_by_ec_or_ph) {
    vars <- c(
      vars,
      "hzdept_r", "taxorder", "taxsubgrp", "organic", "ec_r", "ph1to1h2o_r"
    )
  }


  cns <- colnames(x)

  for (var in vars) {
    if (!(var %in% cns)) {
      stop(shQuote(var), " is a required column name, but cannot be found.")
    }
  }

  if (missing(target_site_ids)) {
    target_site_ids <- x[, var_site_id]
  }

  # Determine number of soil horizons with complete/partial soil texture values
  tmp <- aggregate(
    x = apply(
      x[, var_soiltexture, drop = FALSE],
      MARGIN = 1,
      FUN = function(x) sum(!is.na(x))
    ),
    by = list(id = x[, var_site_id]),
    FUN = function(x) c(complete = sum(x == 3), partial = sum(x > 0))
  )

  ids <- match(target_site_ids, tmp[["id"]], nomatch = NA)
  target_complete_soiltexture <- tmp[["x"]][ids, "complete"]
  target_partial_soiltexture <- tmp[["x"]][ids, "partial"]


  # Calculate additional restrictions
  if (restrict_by_ec_or_ph) {
    x[, "is_organic"] <- x[, "organic"] %in% TRUE
    x[, "is_histosol_histic"] <-
      x[, "taxorder"] %in% "Histosols" |
      grepl("histic", x[, "taxsubgrp"], ignore.case = TRUE)

    # Restrictions pH < 3.5 or EC > 16 apply only
    # if horizon is non-organic and not a histosol/histic soil
    x[, "check"] <- !x[, "is_organic"] & !x[, "is_histosol_histic"]

    tmp <- by(
      data = x,
      INDICES = x[, var_site_id],
      FUN = function(xc) {
        is_ec_restricted <-
          xc[, "check"] & !is.na(xc[, "ec_r"]) & xc[, "ec_r"] >= 16
        is_ph_restricted <-
          xc[, "check"] & !is.na(xc[, "ph1to1h2o_r"]) &
          xc[, "ph1to1h2o_r"] <= 3.5

        c(
          ec_restriction_depth = xc[which(is_ec_restricted)[1], "hzdept_r"],
          ph_restriction_depth = xc[which(is_ph_restricted)[1], "hzdept_r"]
        )
      },
      simplify = FALSE
    )

    restriction2_depth <- matrix(
      data = unlist(tmp[match(x[, var_site_id], names(tmp))]),
      ncol = 2,
      byrow = TRUE
    )
  }


  # Convert to wide format (one row for each point location)
  tmp_depths <- reshape2::acast(
    data = as.data.frame(
      x[, c(var_site_id, var_horizon, var_horizon_lower_depth)]
    ),
    formula = formula(paste(var_site_id, "~", var_horizon)),
    value.var = var_horizon_lower_depth
  )

  ids <- match(target_site_ids, rownames(tmp_depths), nomatch = NA)
  locs_table_depths <- tmp_depths[ids, , drop = FALSE]
  colnames(locs_table_depths) <- paste0("depth_L", colnames(locs_table_depths))


  # Determine soil depth as depth of shallowest restriction
  tmp <- x[, var_restrictions, drop = FALSE]

  if (restrict_by_ec_or_ph) {
    tmp <- cbind(tmp, restriction2_depth)
  }

  soil_depth_cm <- apply(
    X = tmp,
    MARGIN = 1,
    FUN = min,
    na.rm = TRUE
  )

  ids <- match(target_site_ids, x[, var_site_id], nomatch = NA)
  soil_depth_cm <- unname(round(soil_depth_cm)[ids])


  # Case:
  #   * soil depth is missing and
  #   * no complete soil texture available
  # ==> adjust soil depth to 0
  ids <-
    !is.finite(soil_depth_cm) &
    target_complete_soiltexture == 0
  soil_depth_cm[ids] <- 0

  # Adjust soil depth and/or layers where needed
  locs_table_depths <- cbind(
    SoilDepth_cm = soil_depth_cm,
    depth_L = locs_table_depths
  )


  # Case:
  #   * soil_depth disagrees with horizon_depth
  #   * soil_depth > 0 and N_horizons_tmp > 0
  # ==> adjust depth_Lx to soil_depth
  L_at_soildepth <- apply(
    X = locs_table_depths,
    MARGIN = 1,
    FUN = function(x) {
      findInterval(x[1], c(0, na.exclude(x[-1])), left.open = TRUE)
    }
  )
  ids <- which(!apply(
    X = locs_table_depths,
    MARGIN = 1,
    FUN = function(x) x[1] == 0 || all(is.na(x[-1])) || x[1] %in% x[-1]
  ))
  locs_table_depths[cbind(ids, 1 + L_at_soildepth[ids])] <-
    locs_table_depths[ids, "SoilDepth_cm"]

  # Case:
  #   * soil_depth > 0 and horizon_depth L1 in (0, NA) and
  #   * target_complete_soiltexture > 0
  # ==> adjust depth_L1 to soil_depth
  ids <- apply(
    X = cbind(
      target_complete_soiltexture,
      locs_table_depths
    ),
    MARGIN = 1,
    FUN = function(x) !anyNA(x[1:2]) & x[1] > 0 & x[2] > 0 & x[3] %in% c(0, NA)
  )
  locs_table_depths[ids, "depth_L1"] <- locs_table_depths[ids, "SoilDepth_cm"]

  # Case:
  #   * soil_depth > 0 and horizon_depth L1 in (0, NA) and
  #   * target_complete_soiltexture == 0
  # ==> adjust soil_depth to 0
  ids <- apply(
    X = cbind(
      target_complete_soiltexture,
      locs_table_depths
    ),
    MARGIN = 1,
    FUN = function(x) !anyNA(x[1:2]) & x[1] == 0 & x[2] > 0 & x[3] %in% c(0, NA)
  )
  locs_table_depths[ids, "SoilDepth_cm"] <- 0

  # Case:
  #  * soil_depth > 0
  #  * target_partial_soiltexture == 0
  # ==> adjust soil_depth to 0
  ids <- apply(
    X = cbind(
      target_partial_soiltexture,
      locs_table_depths
    ),
    MARGIN = 1,
    FUN = function(x) !is.na(x[1]) & x[1] == 0 & x[2] > 0
  )
  locs_table_depths[ids, "SoilDepth_cm"] <- 0

  # Case:
  #   * soil_depth == 0 and horizon_depth L1 > 0 and
  #   * target_complete_soiltexture > 0
  # ==> adjust soil_depth to depth_L1
  ids <- apply(
    X = cbind(
      target_complete_soiltexture,
      locs_table_depths
    ),
    MARGIN = 1,
    FUN = function(x) {
      !is.na(x[1]) & x[1] > 0 & x[2] == 0 & !is.na(x[3]) & x[3] > 0
    }
  )
  locs_table_depths[ids, "SoilDepth_cm"] <- locs_table_depths[ids, "depth_L1"]


  # Clean layer depths > soil depth
  ids <- locs_table_depths[, "SoilDepth_cm"] < locs_table_depths[, -1]
  locs_table_depths[, -1][ids] <- NA


  # Put together for output
  cbind(
    N_horizons = apply(
      X = locs_table_depths[, -1, drop = FALSE],
      MARGIN = 1,
      function(x) sum(!is.na(x))
    ),
    depth_L = locs_table_depths
  )
}



#' Spatially query \var{mukey} values for point locations from \var{NRCS}
#' web-based \var{SDA} service
#'
#' @inheritParams rSW2st::as_points
#' @param db A character string. Query \var{mukey} from the \var{SSURGO} or
#'   from the \var{STATSGO} soil database.
#' @param ... Currently ignored.
#' @param chunk_size An integer value. The size of chunks into which
#'   \code{locations} is broken up and looped over for processing.
#' @param progress_bar A logical value. Display a progress bar as the code
#'   loops over the chunks?
#'
#' @return A named list with two elements: \itemize{
#'   \item{\var{ref}} The data reference.
#'   \item{\var{mukeys}} A vector with a \var{mukey} value for each
#'     \code{locations}.
#' }
#'
#' @examples
#' \dontrun{
#' if (curl::has_internet()) {
#'   locations <- matrix(
#'     data = c(-120.325, -111.245, 39.855, 36.753),
#'     nrow = 2
#'   )
#'
#'   fetch_mukeys_spatially_NRCS_SDA(locations)
#' }
#' }
#'
#' @export
fetch_mukeys_spatially_NRCS_SDA <- function(
  x,
  crs = 4326,
  db = c("SSURGO", "STATSGO"),
  ...,
  chunk_size = 50L,
  progress_bar = FALSE
) {

  stopifnot(
    requireNamespace("soilDB"),
    "db" %in% methods::formalArgs(soilDB::SDA_spatialQuery),
    curl::has_internet()
  )

  #------ Make sure inputs are correctly formatted
  db <- match.arg(db)

  # We convert to `sp` because of `soilDB::SDA_spatialQuery` (v2.5.7)
  locations <- rSW2st::as_points(x, to_class = "sp", crs = crs)


  #--- Extract mukeys for each point location
  res <- list()

  ids_chunks <- rSW2utils::make_chunks(
    nx = length(locations), #TODO: change to `nrow` once locations is "sf"
    chunk_size = chunk_size
  )

  N_chunks <- length(ids_chunks)
  progress_bar <- progress_bar && N_chunks > 2

  if (progress_bar) {
    message("Fetch 'mukey' values from ", shQuote(db))

    has_progress_bar <- requireNamespace("utils")

    if (has_progress_bar) {
      pb <- utils::txtProgressBar(max = N_chunks, style = 3)
    } else {
      warning("Progress bar requested but package 'utils' is not available.")
    }

  } else {
    has_progress_bar <- FALSE
  }

  for (k in seq_len(N_chunks)) {
    res_mukeys <- try(
      soilDB::SDA_spatialQuery(
        geom = locations[ids_chunks[[k]], ],
        db = db,
        what = "geom"
      ),
      silent = FALSE
    )

    if (inherits(res_mukeys, "try-error")) {
      warning("Spatial SDA query produced error: chunk = ", k)
      res[[k]] <- rep(NA, length(ids_chunks[[k]]))

    } else if (!inherits(res_mukeys, "SpatialPolygons")) {
      warning("Spatial SDA query returned non-spatial object: chunk = ", k)
      res[[k]] <- rep(NA, length(ids_chunks[[k]]))

    } else {
      # Return values of `SDA_spatialQuery` are not ordered by input `geom`,
      # as of soilDB v2.5.7
      res[[k]] <- sp::over(
        x = sp::spTransform(
          locations[ids_chunks[[k]], ],
          CRSobj = sp::proj4string(res_mukeys)
        ),
        y = res_mukeys
      )[, "mukey"]
    }

    if (has_progress_bar) {
      utils::setTxtProgressBar(pb, k)
    }
  }

  if (has_progress_bar) {
    close(pb)
  }

  list(
    ref = create_reference_for_NRCS_SDA(),
    mukeys = unlist(res)
  )
}



#' Download soil data from \var{NRCS} \var{SDA} web service
#'
#' @param mukeys_unique An integer vector with unique \var{mukey} values.
#' @param sql_template A character vector.
#'   A valid \var{T-SQL} query with a \var{WHERE} clause so that the code can
#'   inject chunks of \code{mukeys_unique} values,
#'   i.e., \var{"mapunit.mukey IN (\%s)"}.
#'   If \code{NA}, then the default query is loaded, see examples.
#' @param majcompflag A character string. \var{"subset"} keeps
#'   the WHERE clause \var{component.majcompflag = 'Yes'} that is contained in
#'   \code{sql_template}; \var{"ignore"} removes it from the query. Note that
#'   the field \var{"majcompflag} exists only in the \var{SSURGO} version
#'   of the \var{component} table, but not in the \var{STATSGO} version.
#' @param chunk_size An integer value. The size of chunks into which
#'   \code{mukeys_unique} is broken up and looped over for processing.
#' @param progress_bar A logical value. Display a progress bar as the code
#'   loops over the chunks?
#'
#' @return A \var{data.frame} according to the specifications of \code{sql}.
#'
#' @section Notes: A live internet connection is required to access \var{SDA}.
#'
#' @section Notes: This is a function with minimal functionality;
#' use \code{\link{extract_soils_NRCS_SDA}} for a user-friendly interface.
#'
#' @seealso \code{\link[soilDB]{SDA_query}}
#'
#' @examples
#' res1 <- fetch_soils_from_NRCS_SDA(mukeys_unique = 67616)
#'
#' sql <- readLines(
#'   system.file("NRCS", "nrcs_sql_template.sql", package = "rSW2exter")
#' )
#'
#' res2 <- fetch_soils_from_NRCS_SDA(mukeys_unique = 67616, sql_template = sql)
#'
#' @export
fetch_soils_from_NRCS_SDA <- function(
  mukeys_unique,
  sql_template = NA,
  majcompflag = c("subset", "ignore"),
  chunk_size = 1000L,
  progress_bar = FALSE
) {

  stopifnot(requireNamespace("soilDB"), curl::has_internet())

  majcompflag <- match.arg(majcompflag)

  mukeys_unique <- as.integer(mukeys_unique)
  stopifnot(!anyDuplicated(mukeys_unique))

  ids_chunks <- rSW2utils::make_chunks(
    nx = length(mukeys_unique),
    chunk_size = chunk_size
  )

  N_chunks <- length(ids_chunks)

  if (isTRUE(is.na(sql_template))) {
    sql_template <- readLines(
      system.file(
        "NRCS", "nrcs_sql_template.sql",
        package = "rSW2exter",
        mustWork = TRUE
      )
    )
  }


  #--- Extract soil horizon data for mukeys (chunked)
  res <- list()

  # trim off comments at top of file
  sql_base <- sql_template[-{1:(grep("SELECT", sql_template)[1] - 1)}]

  # remove majcompflag (may be necessary for STATSGO)
  if (majcompflag == "ignore") {
    txt_majcompflag <- "AND component.majcompflag = 'Yes'"
    tmp <- regexpr(txt_majcompflag, sql_base, fixed = TRUE)
    iline <- which(tmp > 0)[1]
    sql_base[iline] <- sub(txt_majcompflag, "", sql_base[iline])
  }

  progress_bar <- progress_bar && N_chunks > 2

  if (progress_bar) {
    print("Fetch soil information from NRCS SDA:")

    has_progress_bar <- requireNamespace("utils")

    if (has_progress_bar) {
      pb <- utils::txtProgressBar(max = N_chunks, style = 3)
    } else {
      warning("Progress bar requested but package 'utils' is not available.")
    }

  } else {
    has_progress_bar <- FALSE
  }


  for (k in seq_along(ids_chunks)) {
    # Prepare SQL query for SDA
    sql <- sql_base

    # Insert requested mukey values
    tmp <- regexpr("mukey IN (%s)", sql, fixed = TRUE)
    iline <- which(tmp > 0)[1]
    sql[iline] <- sprintf(
      fmt = sql[iline],
      paste(shQuote(mukeys_unique[ids_chunks[[k]]]), collapse = ",")
    )

    # Send query to SDA
    # Suppress messages about returning a data.frame
    res[[k]] <- suppressMessages(soilDB::SDA_query(paste(sql, collapse = " ")))
    stopifnot(!inherits(res[[k]], "try-error"))

    if (has_progress_bar) {
      utils::setTxtProgressBar(pb, k)
    }
  }

  if (has_progress_bar) {
    close(pb)
  }

  do.call(rbind, res)
}


#' Extract soil information from the Soil Data Access \var{SDA} service by
#' \var{NRCS} for \pkg{SOILWAT2} applications
#'
#' @inheritParams rSW2st::as_points
#' @inheritParams fetch_mukeys_spatially_NRCS_SDA
#' @param mukeys A character or integer vector. List of soil map unit keys
#'   for which soil information should be extracted. Provide \code{locations}
#'   or \code{mukeys}.
#' @param method A character string. Method indicating whether \var{SDA}
#'   should query \var{"SSURGO"}, \var{"STATSGO"}, or
#'   \var{"SSURGO_then_STATSGO"} which attempts to replace
#'   \code{locations} without \var{"SSURGO"} soil information,
#'   e.g., due to unmapped areas, with \var{"STATSGO"} data.
#' @param only_majcomp A logical value. If \code{TRUE} and extraction is
#'   from \var{"SSURGO"}, then the query extracts only major components.
#'   If \code{FALSE}, then the query extracts components that are major and
#'   those that are not (see \var{majcompflag} of
#'   \code{\link{fetch_soils_from_NRCS_SDA}}).
#'   Ignored if extraction is from \var{"STATSGO"}.
#' @param remove_organic_horizons A character string. Method
#'   indicating how to deal with organic horizons as determined by
#'   function \code{\link{is_NRCS_horizon_organic}}. See details.
#' @param replace_missing_fragvol_with_zero A character string. Method
#'   indicating how missing/null values of rock/gravel fragment fractions
#'   should be interpreted;
#'   passed to \code{\link[rSW2data]{set_missing_soils_to_value}}.
#'   The options are one of
#'   \describe{
#'     \item{\var{"all"}}{
#'       Missing/null values of rack/gravel fragments are
#'       replaced by 0 %.
#'       See also argument \var{nullFragsAreZero} of
#'       function \code{\link[soilDB]{fetchSDA}}.
#'     }
#'     \item{\var{"at_surface"}}{
#'       Missing/null values of rack/gravel fragments are
#'       replaced by 0 % in the shallowest horizon only.
#'       Note, remaining missing values in deeper horizons
#'       can subsequently be imputed by argument \code{impute}.
#'     }
#'     \item{\var{"none"}}{
#'        Missing/null values remain unmodified unless
#'        argument \code{impute} is activated.
#'     }
#'   }
#' @param estimate_missing_bulkdensity A logical value. Estimate missing
#'   bulk density values from saturated water content and gravel volume.
#'   See \code{\link[rSW2data]{estimate_bulkdensity}}.
#' @param impute A logical value. Impute missing values with a
#'   shallow-depth value carried deeper approach (in analogy to \var{LOCF}).
#'   Consequently, missing values in the shallowest horizon are not imputed.
#'   See \code{\link[rSW2utils]{impute_df}}.
#' @param digits An integer value. The number of digits to which soil texture
#'   variables are rounded. Skip rounding if \code{NA} or \code{NULL}.
#' @param verbose A logical value.
#' @inheritParams fetch_soils_from_NRCS_SDA
#' @inheritParams calculate_soil_depth_NRCS
#'
#' @section Details: \var{NRCS} soil datasets \var{SSURGO} and \var{STATSGO} are
#'   organized in soil map units \var{mukey} that are
#'   spatially explicit (i.e., we can query their values by geographic location)
#'   and within each \var{mukey} into soil map unit components \var{cokey}
#'   which have no explicit spatial arrangement within a soil map unit.
#'   Because soil texture information is specific to soil map unit components,
#'   geographic location alone is insufficient to query soil texture.
#'
#'   This function relies that soil information of exactly one \var{cokey}
#'   per each \code{location} or \code{mukeys} is returned by
#'   \code{\link{fetch_soils_from_NRCS_SDA}}. The default \var{SQL} template
#'   \var{"nrcs_sql_template.sql"} extracts the "dominant component".
#'   The dominant component is defined as the the first \var{cokey} with the
#'   highest representative component percent \var{comppct_r}.
#'   See \var{GetDominantComponent.py}
#'   from \url{https://github.com/ncss-tech/SoilDataDevelopmentToolbox}.
#'
#' @section Details:
#'   The argument \code{remove_organic_horizons} is one of
#'   \describe{
#'     \item{\var{"all"}}{
#'       All organic layers (at surface or buried) are removed and horizon
#'       number and depths are recalculated.
#'     }
#'     \item{\var{"at_surface"}}{
#'       Organic layer(s) at the soil surface are removed and horizon
#'       number and depths are recalculated. Buried organic horizons remain
#'       unmodified.
#'     }
#'     \item{\var{"none"}}{
#'        Horizons are not modified.
#'     }
#'   }
#'
#' @section Notes: A live internet connection is required to access \var{SDA}.
#'
#' @seealso \code{\link[soilDB]{fetchSDA}} and \code{\link[soilDB]{SDA_query}}
#'
#' @examples
#' \dontrun{
#' if (curl::has_internet()) {
#'   locations <- matrix(
#'     data = c(-120.325, -111.245, 39.855, 36.753),
#'     nrow = 2
#'   )
#'
#'   # Example 1: extract soils by mukey values
#'   extract_soils_NRCS_SDA(mukeys = c(471168, 1606800))
#'
#'   # Example 2: extract soils by geographic location
#'   extract_soils_NRCS_SDA(x = locations)
#'
#'   # Example 3: first identify mukey values by geographic location,
#'   # then query soils from SSURGO by mukey,
#'   # but still pass locations in case we need to query STATSGO as well
#'
#'   mukeys <- fetch_mukeys_spatially_NRCS_SDA(
#'     x = locations,
#'     db = "SSURGO",
#'     progress_bar = TRUE
#'   )
#'
#'   extract_soils_NRCS_SDA(
#'     x = locations,
#'     mukeys = mukeys[["mukeys"]],
#'     method = "SSURGO_then_STATSGO",
#'     remove_organic_horizons = "at_surface",
#'     replace_missing_fragvol_with_zero = "at_surface",
#'     estimate_missing_bulkdensity = TRUE,
#'     restrict_by_ec_or_ph = FALSE,
#'     impute = TRUE,
#'     progress_bar = TRUE,
#'     verbose = TRUE
#'   )
#' }
#' }
#'
#' @export
extract_soils_NRCS_SDA <- function(
  x,
  crs = 4326,
  mukeys,
  method = c("SSURGO", "STATSGO", "SSURGO_then_STATSGO"),
  sql_template = NA,
  only_majcomp = TRUE,
  remove_organic_horizons = c("none", "all", "at_surface"),
  replace_missing_fragvol_with_zero = c("none", "all", "at_surface"),
  estimate_missing_bulkdensity = FALSE,
  restrict_by_ec_or_ph = TRUE,
  impute = FALSE,
  digits = 3L,
  chunk_size = 1000L,
  progress_bar = FALSE,
  verbose = FALSE
) {

  if (!missing(x)) {
    x <- rSW2st::as_points(x, to_class = "sf", crs = crs)
  }

  stopifnot(
    !(missing(x) && missing(mukeys)),
    curl::has_internet(),
    missing(x) || missing(mukeys) || nrow(x) == length(mukeys)
  )

  method <- match.arg(method)

  if (missing(x) && method == "SSURGO_then_STATSGO") {
    warning(
      "'method' == \"SSURGO_then_STATSGO\" has no effect ",
      "if locations are missing"
    )
  }

  db <- if (method == "STATSGO") "STATSGO" else "SSURGO"

  locs_keys <- data.frame(
    source = db,
    mukey = if (missing(mukeys)) {
      fetch_mukeys_spatially_NRCS_SDA(
        x = x,
        crs = crs,
        db = db,
        progress_bar = progress_bar
      )[["mukeys"]]

    } else {
      mukeys
    },
    cokey = NA,
    compname = NA,
    compkind = NA,
    comppct_r = NA
  )

  stopifnot(!anyNA(locs_keys[["mukey"]]))


  # Download soil data from NRCS SDA web service
  res <- fetch_soils_from_NRCS_SDA(
    mukeys_unique = unique(locs_keys[["mukey"]]),
    sql_template = sql_template,
    majcompflag = if (only_majcomp) {
      switch(db, SSURGO = "subset", STATSGO = "ignore")
    } else {
      "ignore"
    },
    chunk_size = chunk_size,
    progress_bar = progress_bar
  )


  #--- Assign (dominant) cokey to point locations
  ids <- match(locs_keys[, "mukey"], res[, "MUKEY"], nomatch = 0)
  tmp_vars <- c("compname", "compkind", "comppct_r")
  tmp_vars <- intersect(tmp_vars, colnames(res))
  locs_keys[ids > 0, c("cokey", tmp_vars)] <- res[ids, c("COKEY", tmp_vars)]


  #--- Identify which variables are fixed per COKEY
  var_stxt3 <- c("sandtotal_r", "claytotal_r", "silttotal_r")
  var_stxt <- intersect(var_stxt3, colnames(res))
  var_output <- c(
    "dbovendry_r",
    "fragvol_r",
    var_stxt,
    "organic", "om_r"
  )

  tmp <- by(
    data = res,
    INDICES = res[["COKEY"]],
    FUN = function(x) {
      sapply(
        X = x,
        FUN = function(v) {
          if (is.numeric(v)) {
            var(v, na.rm = TRUE) > 0
          } else {
            nlevels(factor(v)) > 1
          }
        }
      )
    },
    simplify = FALSE
  )

  tmp <- matrix(unlist(tmp), ncol = ncol(res), byrow = TRUE)

  fx_per_cokey <- colnames(res)[!apply(tmp, 2, any, na.rm = TRUE)]

  fx_per_cokey <- union(fx_per_cokey, c("MUKEY", "COKEY"))
  fx_per_cokey <- setdiff(fx_per_cokey, c("hzdept_r", "hzdepb_r", var_output))


  #--- Deduce soil texture iff one of three values is missing
  if (all(var_stxt3 %in% colnames(res))) {
    res <- rSW2data::deduce_complete_soil_texture(
      x = res,
      var_stxt = var_stxt,
      val_total = 100,
      ignore_le = 5
    )
  }


  #--- Interpret missing values for rock/gravel fragments as 0 %
  if ("fragvol_r" %in% colnames(res)) {
    res <- rSW2data::set_missing_soils_to_value(
      x = res,
      variable = "fragvol_r",
      value = 0,
      where = match.arg(replace_missing_fragvol_with_zero),
      horizon = "Horizon_No",
      verbose = verbose
    )
  }


  #--- Estimate soil bulk density if missing
  if (estimate_missing_bulkdensity) {
    is_missing_bd <- !is.finite(res[, "dbovendry_r"])

    if (any(is_missing_bd)) {
      res[is_missing_bd, "dbovendry_r"] <- rSW2data::estimate_bulkdensity(
        theta_saturated = res[is_missing_bd, "wsatiated_r"] / 100,
        gravel_volume = res[is_missing_bd, "fragvol_r"] / 100
      )

      if (verbose) {
        tmp <- is.finite(res[is_missing_bd, "dbovendry_r"])
        nm <- sum(!tmp)

        message(
          "Missing bulk density values estimated: n = ",
          sum(tmp),
          if (nm) paste0("; however, n = ", nm, " remain missing.")
        )
      }
    }
  }


  #--- Remove organic horizons
  res[, "organic"] <- is_NRCS_horizon_organic(res) %in% TRUE

  remove_organic_horizons <- match.arg(remove_organic_horizons)

  if (remove_organic_horizons %in% c("all", "at_surface")) {
    is_organic <- res[, "organic"]

    if (remove_organic_horizons == "all") {
      is_remove <- is_organic

      if (verbose && (n_remove <- sum(is_remove)) > 0) {
        message("Removed organic horizons: n = ", n_remove)
      }

    } else if (remove_organic_horizons == "at_surface") {
      #--- Organic horizon at the surface
      is_remove <- is_organic & res[, "Horizon_No"] == 1

      # Check whether more than one surface horizon is organic
      cokeys_w_osurf <- unique(res[is_remove, "COKEY"])
      ids_check <- which(res[["COKEY"]] %in% cokeys_w_osurf)

      if (length(ids_check) > 0) {
        tmp <- tapply(
          X = is_organic[ids_check],
          INDEX = res[ids_check, "COKEY"],
          FUN = function(x) {
            # First element corresponds to TRUE
            # because we only look at COKEYS with an organic surface horizon
            n <- rle(x)[["lengths"]][1]
            c(rep(TRUE, n), rep(FALSE, length(x) - n))
          },
          simplify = FALSE
        )

        is_remove[ids_check] <- unlist(tmp[match(cokeys_w_osurf, names(tmp))])
      }


      if (verbose && (n_remove <- sum(is_remove)) > 0) {
        n_oburied <- sum(is_organic & !is_remove)

        message(
          "Removed organic horizons at surface: n = ", n_remove,
          " for n = ", length(cokeys_w_osurf), " unique COKEYs",
          if (n_oburied > 0) {
            paste0("; n = ", n_oburied, " buried organic horizons remain.")
          } else "."
        )
      }
    }


    #--- Remove identified organic horizons

    if (any(is_remove)) {
      # Identify cokeys with horizons to be removed
      cokeys_affected <- unique(res[is_remove, "COKEY"])
      ids_affected <- which(res[["COKEY"]] %in% cokeys_affected)

      res_affected <- cbind(
        res[ids_affected, ],
        remove = is_remove[ids_affected]
      )

      # Recalculate horizon ranks and all depth values
      tmp <- by(
        data = res_affected,
        INDICES = res_affected[["COKEY"]],
        FUN = function(x) {
          ids_keep <- !x[["remove"]]

          xnew <- x[ids_keep, , drop = FALSE]
          n <- nrow(xnew)

          if (n > 0) {
            tmp <- rep(0, length(x[["remove"]]))
            tmp[x[["remove"]]] <-
              x[x[["remove"]], "hzdepb_r"] - x[x[["remove"]], "hzdept_r"]
            removed_total <- sum(tmp)
            removed_widths <- cumsum(tmp)[ids_keep]

            # Re-calculate horizon rank
            xnew[, "Horizon_No"] <- seq_len(n)

            # Re-calculate upper/lower horizon depth limits
            ids <- grep("hzdep", colnames(xnew))
            xnew[, ids] <- xnew[, ids] - removed_widths

            # Re-calculate depth restrictions
            ids <- grep("_depth", colnames(xnew))
            xnew[, ids] <- xnew[, ids] - removed_total

          } else {
            # We need at least one entry per COKEY
            # Copy values of first row of those columns that are fixed per
            # COKEY and set others to NA
            xnew <- x[1, , drop = FALSE]
            xnew[, setdiff(colnames(xnew), fx_per_cokey)] <- NA
          }

          xnew
        },
        simplify = FALSE
      )

      tmp <- do.call(rbind, tmp)

      # Put data back together
      res <- rbind(
        res[-ids_affected, ],
        tmp[, !grepl("remove", colnames(tmp))]
      )
    }
  }

  res[, "organic"] <- as.integer(res[, "organic"])


  #--- Calculate soil depth of profile (per COKEY)
  # and convert depth table to wide-format for output
  locs_table_depths <- calculate_soil_depth_NRCS(
    x = res,
    target_site_ids = locs_keys[, "cokey"],
    restrict_by_ec_or_ph = restrict_by_ec_or_ph,
    var_site_id = "COKEY",
    var_horizon = "Horizon_No",
    var_horizon_lower_depth = "hzdepb_r",
    var_restrictions =
      c("Horizon_depth", "RootZoneRestriction_depth", "Bedrock_depth"),
    var_soiltexture = c("sandtotal_r", "claytotal_r", "silttotal_r")
  )



  # Transfer final soil depth and (potentially adjusted depth_L1)
  ids <- match(res[["COKEY"]], rownames(locs_table_depths))
  res[, "SoilDepth_cm"] <- locs_table_depths[ids, "SoilDepth_cm"]
  res[, "N_horizons"] <- locs_table_depths[ids, "N_horizons"]

  is_shallowest <- res[, "Horizon_No"] == 1
  res[is_shallowest, "hzdepb_r"] <-
    locs_table_depths[ids[is_shallowest], "depth_L1"]


  #--- Last step: impute remaining missing values per COKEY/location
  # by shallow-depth value carried deeper (in analogy to LOCF)
  # but do not impute missing values in the shallowest horizon
  if (impute) {
    res <- rSW2data::impute_soils(
      x = res,
      var_site_id = "COKEY",
      var_horizon = "Horizon_No",
      var_values = var_output,
      verbose = verbose
    )
  }


  #--- Convert units & rounding
  # Convert % to fraction
  var_pct_to_fraction <- intersect(
    c("fragvol_r", var_stxt3),
    colnames(res)
  )
  res[, var_pct_to_fraction] <- res[, var_pct_to_fraction] / 100

  # Round texture
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

      var_others2 <- "fragvol_r"

    } else {
      var_others2 <- c("fragvol_r", var_stxt)
    }

    var_others2 <- intersect(var_others2, colnames(res))
    res[, var_others2] <- round(res[, var_others2], digits)
  }


  #--- Create texture table
  # Convert to wide format (one row for each point location)
  tmp_texture <- reshape2::acast(
    data = reshape2::melt(
      data = cbind(
        res[, c("Horizon_No", "COKEY", var_output)]
      ),
      id.vars = c("Horizon_No", "COKEY")
    ),
    formula = COKEY ~ Horizon_No + variable
  )

  ids <- match(locs_keys[, "cokey"], rownames(tmp_texture), nomatch = NA)
  locs_table_texture <- tmp_texture[ids, , drop = FALSE]

  colnames(locs_table_texture) <- sapply(
    X = strsplit(colnames(locs_table_texture), split = "_"),
    FUN = function(x) paste0(x[2], "_L", x[1])
  )



  #--- Attempt to replace nosoil-sites with STATSGO
  ids_h0 <- which(locs_table_depths[, "N_horizons"] == 0)
  if (
    length(ids_h0) > 0 &&
    !missing(x) &&
    method == "SSURGO_then_STATSGO"
  ) {

    # Call again for nosoil rows and extract from STATSGO instead of SSURGO
    res0 <- Recall(
      x = x[ids_h0, ],
      method = "STATSGO",
      remove_organic_horizons = remove_organic_horizons,
      replace_missing_fragvol_with_zero = replace_missing_fragvol_with_zero,
      estimate_missing_bulkdensity = estimate_missing_bulkdensity,
      restrict_by_ec_or_ph = restrict_by_ec_or_ph,
      impute = impute,
      digits = digits,
      chunk_size = chunk_size,
      progress_bar = progress_bar,
      verbose = verbose
    )

    # Check that we have no longer nosoils
    is_res0_good <-
      !is.na(res0[["table_depths"]][, "N_horizons"]) &
      res0[["table_depths"]][, "N_horizons"] > 0
    ids_h0_replace <- ids_h0[is_res0_good]

    # Update nosoil data with soils from STATSGO
    if (length(ids_h0_replace) > 0) {
      locs_keys[ids_h0_replace, ] <- res0[["table_keys"]][is_res0_good, ]

      ivars <- seq_len(
        min(ncol(locs_table_depths), ncol(res0[["table_depths"]]))
      )
      locs_table_depths[ids_h0_replace, ivars] <-
        res0[["table_depths"]][is_res0_good, ivars]

      ivars <- seq_len(
        min(ncol(locs_table_texture), ncol(res0[["table_texture"]]))
      )
      locs_table_texture[ids_h0_replace, ivars] <-
        res0[["table_texture"]][is_res0_good, ivars]
    }
  }


  #--- Return tables
  list(
    ref = create_reference_for_NRCS_SDA(),
    table_keys = locs_keys,
    table_depths = locs_table_depths,
    table_texture = locs_table_texture
  )
}
