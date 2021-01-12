#' Simulates ascochyta spore dispersal for a single day increment
#'
#' @param i_date Date (YYYY-MM-DD) for which the dispersal function will iterate for each hour of the day
#' @param daily_vals `data.table` of model variable which have been calculated for days prior to the `i_date`
#' @param weather_dat `data.table` of weather observations which includes the query date `i_date`
#'
#' @return a `data.table` of values generated for the day `i_date`
#'
#'
#' @examples
one_day <- function(i_date,
                    daily_vals,
                    weather_dat) {

  # expand time to be hourly
  i_time <- rep(i_date, 24) + lubridate::dhours(0:23)

  # subset weather data by day
  weather_day <-
    weather_dat[times %in% i_time, ]

  # update daily_vals with the values from the current day
  daily_vals <-
    rbindlist(list(
      daily_vals,
      list(
        daily_vals[.N, cdd] +
          mean(weather_day[, temp]),
        daily_vals[.N, cwh] +
          weather_day[1, wet_hours],
        daily_vals[.N, cr] +
          sum(weather_day[, rain], na.rm = TRUE),
        as.POSIXct(i_date),
        lubridate::yday(i_date)
      )
    ))

  return(daily_vals)
}
