#' Formatted Newmarracarra weather data (internal test/example fixture)
#'
#' An `asco.weather` object (an extension of a \CRANpkg{data.table}, see
#' [format_weather()]) holding hourly weather observations for the
#' Newmarracarra, Western Australia research station for the 1998 chickpea
#' growing season. It is the same station data distributed with the package
#' as `inst/extdata/1998_Newmarracarra_weather_table.csv`, but already run
#' through [format_weather()] so that it is ready to pass directly to
#' [trace_asco()], [one_day()], [summarise_trace()] and [tidy_trace()].
#'
#' @section Internal only -- not exported: `newM_weather` is stored in
#'   `R/sysdata.rda`, which R lazy-loads into the package's internal
#'   namespace at build time but does **not** export. In practice this means:
#'   * it is **not** accessible to package users, either as
#'     `ascotraceR::newM_weather` or via `data(newM_weather)`, and no `?`
#'     help page is generated for it (hence `@noRd` below);
#'   * it **is** directly usable, unqualified, from within this package's own
#'     `testthat` test files (e.g. `test-trace_asco.R`, `test-one_day.R`,
#'     `test-summarise_trace.R`, `test-tidy_trace.R`), because
#'     `testthat::test_check()` evaluates package tests inside the package
#'     namespace, where internal objects are visible by name;
#'   * it is also usable, unqualified, from other internal package code, but
#'     is not currently referenced anywhere outside of `tests/testthat/`.
#'
#'   If this data ought to be usable by package users too (for example, so
#'   examples and vignettes don't need to re-derive it from the raw `.csv`
#'   file every time), it would need to be moved out of `R/sysdata.rda` and
#'   into a `data/newM_weather.rda` file (e.g. via
#'   `usethis::use_data(newM_weather)`), with `LazyData: true` set in
#'   `DESCRIPTION` and a normal (non-`@noRd`) roxygen documentation block so
#'   a public help page is generated.
#'
#' @format A `data.table` with 6,600 rows (hourly observations) and 16
#'   variables:
#'   \describe{
#'   \item{times}{Date-time of the observation, `POSIXct`.}
#'   \item{temp}{Air temperature (degrees Celsius).}
#'   \item{rain}{Rainfall in the hour (mm).}
#'   \item{ws}{Wind speed (km/h).}
#'   \item{wd}{Wind direction (compass degrees).}
#'   \item{wd_sd}{Standard deviation of wind direction (compass degrees).}
#'   \item{station}{Weather station identifying name.}
#'   \item{YYYY, MM, DD, hh, mm}{Year, month, day, hour and minute
#'   components of `times`.}
#'   \item{day}{Day of year (Julian day) that the observation falls on.}
#'   \item{hours_in_day}{Number of hourly observations recorded for `day`.
#'   *Not currently produced by the present version of [format_weather()];
#'   this looks like a column retained from an earlier version of the
#'   formatting code used when this fixture was generated. Worth confirming
#'   it's still needed by anything before relying on it.*}
#'   \item{wet_hours}{Number of hours in `day` with recorded rainfall.}
#'   \item{ws_sd}{Standard deviation of wind speed (km/h). *Same caveat as
#'   `hours_in_day` above -- not produced by the current
#'   [format_weather()].*}
#'   }
#'
#' @source Derived from `inst/extdata/1998_Newmarracarra_weather_table.csv`
#'   via [format_weather()]. There is no `data-raw/` script in this package
#'   version to regenerate it; if this data needs to be refreshed (e.g.
#'   after a change to [format_weather()]'s output schema), consider adding
#'   one so the provenance is reproducible rather than only living in the
#'   compiled `R/sysdata.rda`.
#'
#' @keywords internal
#' @noRd
"newM_weather"
