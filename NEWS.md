# ascotraceR 0.1.0

## Bug fixes

* Fixed `which_paddock_row()`, used internally by `trace_asco()` to place
  `primary_infection_foci` in the paddock. Its row-lookup formula assumed
  `x` varied fastest through `paddock`'s rows, but `data.table::CJ()` (how
  `paddock` is built) actually varies `y` fastest. As a result,
  `primary_infection_foci` supplied as explicit, asymmetric `x`/`y`
  coordinates (i.e. anything other than `"centre"` or a symmetric location)
  could be seeded at the transposed coordinate instead of the one supplied.
  `which_paddock_row()` now looks up rows with a `data.table` join instead
  of a hand-rolled formula, so it no longer depends on assumptions about
  paddock row order. **This changes simulation output for runs that used
  non-`"centre"`, asymmetric `primary_infection_foci` coordinates -- results
  from prior versions with such inputs should be treated with caution and
  ideally re-run.**

## Documentation

* Added internal documentation for `newM_weather`, the package's internal
  test/example weather fixture, clarifying that it is not exported or
  user-accessible.

## Testing

* Substantially hardened the unit test suite: tests now actually exercise
  the functions they claim to test (e.g. `random_integer_from_real()`),
  stochastic tests are seeded and use tighter, statistically-justified
  tolerances instead of loose bounds, and `expect_is()` has been migrated
  to `expect_type()`/`expect_s3_class()` throughout.

# ascotraceR 0.0.2  

* Documentation fixes  
* update email addresses  
* bugfix date conversions in format_weather. Dates with no hours are now treated
as midnight hour for that date. lubridate(x, truncated = 3)

# ascotraceR 0.0.1.9000

## Minor changes

* Clarification on input arguments in `format_weather()`.

* Typo fixes and other enhancements in documentation.

* Add note in README pertaining to _Rcpp_ errors that may occur if an older version of _Rcpp_ is locally installed.

# ascotraceR 0.0.1

* Initial CRAN release.

* New R-package to simulate the spread of chickpea disease, Ascochyta blight.

# ascotraceR 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
