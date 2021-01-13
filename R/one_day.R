#' Simulates ascochyta spore dispersal for a single day increment
#'
#' @param i_date a character string or \class{Date} formated string indicating the
#'  iteration date of the model. Preferably in \acronym{ISO8601} format (YYYY-MM-DD),
#'  \emph{e.g.} \dQuote{2020-04-26}.
#'
#' @param daily_vals `data.table` of model variable which have been calculated for days prior to the `i_date`
#' @param weather_dat `data.table` of weather observations which includes the query date `i_date`
#'
#' @return a `data.table` of values generated for the day `i_date`
#'
#'
#' @examples
one_day <- function(i_date,
                    daily_vals,
                    weather_dat,
                    gp_rr) {

  # expand time to be hourly
  i_time <- rep(i_date, 24) + lubridate::dhours(0:23)

  # subset weather data by day
  weather_day <-
    weather_dat[times %in% i_time, ]

  # obatin summary weather for i_day
  i_mean_air_temp <- mean(weather_day[, temp])
  i_wet_hours <- weather_day[1, wet_hours]
  i_rainfall <- sum(weather_day[, rain], na.rm = TRUE)


  day_i_vals <-
    list(
      i = as.POSIXct(i_date),
      day = lubridate::yday(i_date),
      cdd = daily_vals[.N, cdd] +
        i_mean_air_temp,
      cwh = daily_vals[.N, cwh] +
        i_wet_hours,
      cr = daily_vals[.N, cr] +
        i_rainfall
    )

  day_i_vals[["gp"]] <-
    new_growing_points(current_growing_points = daily_vals[.N, gp],
                       growing_points_replication_rate,
                       max_growing_points,
                       mean_air_temp)


  # update daily_vals with the values from the current day
  daily_vals <-
    rbindlist(list(
      daily_vals,
      day_i_vals
    ))

  # Write code to iterate over each hour and the function `growth`
  # `growth` function should return a vector of length 24 rows for each hour.
  # each value should give the number of growing points at that hour in time
  crop_gps <-


  return(daily_vals)
}
