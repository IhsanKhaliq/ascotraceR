#' Tidy up a trace_asco output nested list
#'
#' Creates a tidy \CRANpkg{data.table} from the output of [trace_asco()].
#'
#' @param trace a nested list output from [trace_asco()]
#'
#' @return a `data.table`
#' @export
#'
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
#' tidy_trace(traced)
tidy_trace <- function(trace) {

  i_date <- t(as.data.table(lapply(X = trace, `[[`, 2)))
  sub_trace <- setDT(map_df(trace, ~ unlist(.[3:9])))
  sub_trace[, i_date := lubridate::as_date(i_date)]

  infected_coords <- rbindlist(lapply(X = trace, `[[`, 10),
                               idcol = "i_day")
  paddock <- rbindlist(lapply(trace, `[[`, 1),
                       idcol = "i_day")
  infected <- rbindlist(lapply(trace, `[[`, 11),
                        idcol = "i_day")
  # create a col indicating whether and quadrat is infected or not
  infected[, infected := rep_len(TRUE, .N)]

  tidy_trace_dt <- paddock[sub_trace, on = c("i_day", "new_gp")]
  tidy_trace_dt <- infected[tidy_trace_dt, on = c("i_day", "x", "y")]

  # replace any `NA` values with `FALSE` in the "infected" col
  tidy_trace_dt[, "infected"][is.na(tidy_trace_dt[, "infected"])] <- FALSE

  setcolorder(tidy_trace_dt, c("i_day", "i_date", "day"))
  return(tidy_trace_dt)
}
