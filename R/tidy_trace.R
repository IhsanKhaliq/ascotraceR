#' Tidy up a trace_asco output nested list
#'
#' Creates a tidy \CRANpkg{data.table} from the output of [trace_asco()].
#'
#' @param trace a nested list output from [trace_asco()]
#'
#' @return A tidy \CRANpkg{data.table} of [trace_asco()] output.
#' @seealso [summarise_trace()], [trace_asco()]
#'
#' @examplesIf interactive()
#'
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
#'   paddock_length = 20,
#'   paddock_width = 20,
#'   initial_infection = "1998-06-10",
#'   sowing_date = as.POSIXct("1998-06-09"),
#'   harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(100),
#'   time_zone = "Australia/Perth",
#'   primary_infection_foci = "centre")
#'
#' tidied <- tidy_trace(traced)
#'
#' # take a look at the infectious growing points on day 102
#' library(ggplot2)
#' ggplot(data = subset(tidied, i_day == 102),
#'        aes(x = x, y = y, fill = infectious_gp)) +
#'   geom_tile()
#'
#' @export
#'
tidy_trace <- function(trace) {

  i_day <- new_gp <- NULL
  i_date <- t(setDT(lapply(X = trace, `[[`, 2)))
  sub_trace <- rbindlist(lapply(trace,function(x) {
    data.table(
      matrix(unlist(x[3:9]),
             nrow = 1,
             dimnames = list(NULL,
                             names(x[3:9]))
             )
      )
    }))
  sub_trace[, i_date := lubridate::as_date(i_date)]
  sub_trace[, new_gp := NULL] # this is a duplicated value
  setkey(sub_trace, i_day)

  paddock <- rbindlist(lapply(trace, `[[`, 1),
                       idcol = "i_day")
  setkey(paddock, i_day)

  tidy_trace_dt <- merge(x = paddock, y = sub_trace, all.x = TRUE)

  setcolorder(tidy_trace_dt, c("i_day", "i_date", "day"))
  return(tidy_trace_dt[])
}
