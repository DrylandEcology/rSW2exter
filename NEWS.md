# rSW2exter v0.3.2-9000

# rSW2exter v0.3.1
* Functions that extract soil properties now return organic content as
  mass fraction
    * `extract_soils_NRCS_SDA()` now returns `"om_r"` as fraction `[0-1]`
       instead of percentage `0-100%`
    * `extract_soils_SOLUS100()` now returns `"soc"` (soil organic carbon)
       as fraction `[0-1]` instead of `g/kg 0-1000`
* `extract_soils_SOLUS100()` now returns correct units for `"gypsum"`.

# rSW2exter v0.3.0
* Functionality to download, query, and extract soils data from `SOLUS100`
    * `depth_profile_SOLUS100()`, `variables_SOLUS100()`, and
      `filenames_SOLUS100()` provide meta data.
    * `download_SOLUS100()` and `check_SOLUS100()` download and manage
      a local copy.
    * `extract_soil_SOLUS100()` (and bare-bones `fetch_soils_from_SOLUS100()`)
      extract soils data from a local copy.
* The `"raster"` package is now "suggested" (instead of "imported").


# rSW2exter v0.2.2
* `fetch_soils_from_NRCS_SDA()` gains the ability to inject queries with
  multi-variable parameters. It gains two new arguments
  (with backwards compatible default values):
    * `"bind_params"` that replaces the deprecated argument `"mukeys_unique"`
    * `"injection_format"` that identifies the format string used to
      bind/inject values in parametrized queries
* `fetch_soils_from_NRCS_SDA()` now builds `SQL` queries consistently with
  single quotes.


# rSW2exter v0.2.1
* `fetch_mukeys_spatially_NRCS_SDA()` now requires at least
  `"soilDB"` version `2.6.10` (and no longer supports `"sp"`).
* Linting updated to `lintr` >= `3.1`.


# rSW2exter v0.2.0
* `fetch_mukeys_spatially_NRCS_SDA()` now handles versions of `"soilDB"`
  from `2.5.7` to at least `2.6.14`.
* `extract_soils_NRCS_SDA()` queries include now `localphase`;
  this should improve the ability to identify a component of a soil map unit
  across `NRCS` data releases by a combination of
  `compname`, `comppct_r`, `localphase`.
* `extract_soils_NRCS_SDA()` gains argument `only_soilcomp` which excludes
  non-soil components, i.e., those that are not "Miscellaneous areas" and
  are not `"NOTCOM"` (not completed).
* Linting updated to `lintr` >= `3.0` and
  lint workflow switched from package tests to Github Action (#5).

# rSW2exter v0.1.0
Initial release
