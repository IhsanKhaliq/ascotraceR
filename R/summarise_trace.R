#' Summarise a trace_asco output nested list
#'
#' Creates a paddock-level summary \CRANpkg{data.table} from the output of
#'  [trace_asco()] on a daily time-step.
#'
#' @param trace a nested list output from [trace_asco()]
#'
#' @return A \CRANpkg{data.table} summarising the model's output for a paddock
#'  on a daily time-step with the area under the disease progress curve
#'  (\acronym{AUDPC}) at the paddock level for the simulation's run with the
#'  following columns:
#'   \tabular{rl}{
#'   **i_day**: \tab Model iteration day (day) \cr
#'   **new_gp**: \tab New growing points on `i_day` (n) \cr
#'   **susceptible_gp**: \tab Susceptible growing points on `i_day` (n) \cr
#'   **exposed_gp**: \tab Exposed growing points on `i_day` (n) \cr
#'   **i_date**: \tab Calendar date corresponding to model's `i_day` \cr
#'   **day**: \tab Julian day or numeric day of year (day) \cr
#'   **cdd**: \tab Cumulative degree days (day) \cr
#'   **cwh**: \tab Cumulative wet hours (h) \cr
#'   **cr**: \tab Cumulative rainfall (mm) \cr
#'   **gp_standard**: \tab standard growing points assuming growth is not
#'           impeded by infection on `i_day` (n) \cr
#'   **AUDPC**: \tab Area under the disease progress curve (AUDPC) \cr}
#'
#' @seealso [trace_asco()], [tidy_trace()]
#'
#' @examplesIf interactive()
#' Newmarracarra <-
#'    read.csv(system.file("extdata",
#'             "1998_Newmarracarra_weather_table.csv", package = "ascotraceR"))
#' station_data <-
#'    system.file("extdata", "stat_dat.csv", package = "ascotraceR")
#'
#' weather_dat <- format_weather(
#'    x = Newmarracarra,
#'    POSIXct_time = "Local.Time",
#'    temp = "mean_daily_temp",
#'    ws = "ws",
#'    wd_sd = "wd_sd",
#'    rain = "rain_mm",
#'    wd = "wd",
#'    station = "Location",
#'    time_zone = "Australia/Perth",
#'    lonlat_file = station_data)
#'
#' traced <- trace_asco(
#'   weather = weather_dat,
#'   paddock_length = 100,
#'   paddock_width = 100,
#'   initial_infection = "1998-06-10",
#'   sowing_date = as.POSIXct("1998-06-09"),
#'   harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(100),
#'   time_zone = "Australia/Perth",
#'   primary_infection_foci = "centre")
#'
#' tidied <- summarise_trace(traced)
#' @export

summarise_trace <- function(trace) {

  i_day <- new_gp <- AUDPC <- `.` <- NULL

  summarised_trace <- tidy_trace(trace)

  new_gp <- summarised_trace[, .(new_gp = mean(new_gp)), by = i_day]
  susceptible_gp <-
    summarised_trace[, .(susceptible_gp = mean(susceptible_gp)), by = i_day]
  exposed_gp <-
    summarised_trace[, .(exposed_gp = mean(exposed_gp)), by = i_day]
  infectious_gp <-
    summarised_trace[, .(infectious_gp = mean(infectious_gp)), by = i_day]

  x <- unique(summarised_trace[, c("i_day",
                                   "i_date",
                                   "day",
                                   "cdd",
                                   "cwh",
                                   "cr",
                                   "gp_standard")])

  y <- list(new_gp, susceptible_gp, exposed_gp, infectious_gp, x)
  lapply(y, function(i) setkey(i, i_day))

  out <- Reduce(function(...) merge(..., all = TRUE), y)
  AUDPC <- .calculate_audpc(x = infectious_gp)
  return(out[, AUDPC := rep_len(AUDPC, .N)][])
}

#' Calculate the area under the disease progress curve (AUDPC)
#'
#' This function is used to return the AUDPC in the output of SEIR().  Not to be
#' used alone.
#'
#' @param x a `data.table` with the simulation day, `i_day` and
#'  infected growing points, `infectious_gp`
#'
#' @return  A `numeric` value as `double`.
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' x <- data.table::data.table(
#'   check.names = FALSE,
#' "i_day" = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
#'   "infectious_gp" = c(
#'     0.0016,
#'     0.002,
#'     0.002,
#'     0.0026,
#'     0.0026,
#'     0.0029,
#'     0.0029,
#'     0.0033,
#'     0.0036
#'   )
#' )
#' .calculate_audpc(x)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @references
#' Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D.
#' Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology in R:
#' Disease Progress over Time. *The Plant Health Instructor*.
#' DOI:[10.1094/PHI-A-2008-0129-02]https://doi.org/10.1094/PHI-A-2008-0129-02).
#'
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. The Study of Plant
#' Disease Epidemics. American Phytopathological Society, St. Paul, MN.
#' DOI:[10.1094/9780890545058](https://doi.org/10.1094/9780890545058).
#'
#' @keywords internal
#' @noRd

.calculate_audpc <- function(x) {
  n <- sum(NROW(x), -1)

  meanvec <- intvec <- vector(mode = "double", length = n)

  for (i in seq_len(n)) {
    j <- sum(i, 1)
    meanvec[i] <- mean(c(x$infectious_gp[i], x$infectious_gp[j]))
    intvec[i] <- sum(x$i_day[j], -x$i_day[i])
  }

  infprod <- meanvec * intvec

  return(sum(infprod))
}
