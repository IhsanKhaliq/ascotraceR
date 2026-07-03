## Resubmission

This is a resubmission (previous CRAN version: 0.0.2). In this version I have:

* Updated the corresponding/maintainer author's contact details.

* Fixed a bug in the internal `which_paddock_row()` helper, used by
  `trace_asco()` to place `primary_infection_foci` within the paddock. Its
  row-lookup formula assumed paddock rows were ordered with `x` varying
  fastest, which did not match how `data.table::CJ()` actually orders them.
  As a result, `primary_infection_foci` supplied as explicit, asymmetric
  `x`/`y` coordinates (rather than `"centre"` or a symmetric location) could
  be seeded at the transposed coordinate instead of the one requested. See
  NEWS.md for details.

* Expanded and hardened the unit test suite (seeded stochastic tests,
  tightened tolerances, added tests for previously untested functions).

* Minor documentation fixes (roxygen tag corrections, added `@examples` to
  several internal functions).

## R CMD check results

0 errors | 0 warnings | 0 notes
