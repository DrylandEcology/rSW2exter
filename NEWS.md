# rSW2exter v0.1.2-9000

# rSW2exter v0.1.1
* `fetch_mukeys_spatially_NRCS_SDA()` now handles versions of "soilDB"
   from `2.5.7` to at least `2.6.14`.
* `extract_soils_NRCS_SDA()` queries include now `localphase`;
   this should improve the ability to identify a component of a soil map unit
   across NRCS data releases by a combination of
   `compname`, `comppct_r`, `localphase`.
* `extract_soils_NRCS_SDA()` gains argument `only_soilcomp` which excludes
  non-soil components, i.e., those that are not "Miscellaneous areas" and
  are not "NOTCOM" (not completed).

# rSW2exter v0.1.0
Initial release