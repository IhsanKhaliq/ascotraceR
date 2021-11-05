#' Summarise a trace_asco output nested list
#'
#' Creates a paddock-level summary \CRANpkg{data.table} from the output of
#'  [trace_asco()] on a daily time-step.
#'
#' @param trace a nested list output from [trace_asco()]
#'
#' @return A \CRANpkg{data.table} summarising the model's output for a paddock
#'  on a daily time-step.
#' @export
#' @seealso [trace_asco()], [tidy_trace()]
#' @examples
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
#'
summarise_trace <- function(trace) {

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

  return(Reduce(function(...) merge(..., all = TRUE), y))
}
