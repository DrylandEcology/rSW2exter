# rSW2exter v0.2.1-9000
* `fetch_mukeys_spatially_NRCS_SDA()` now requires at least
  `"soilDB"` version `2.6.10` (and no longer supports `"sp"`).
* Linting updated to `lintr` >= 3.1


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
* Linting updated to `lintr` >= 3 and
  lint workflow switched from package tests to Github Action (#5).

# rSW2exter v0.1.0
Initial release
