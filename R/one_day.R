#' Simulates ascochyta spore dispersal for a single day increment
#'
#' @param i_date a character string or \class{Date} formated string indicating the
#'  iteration date of the model. Preferably in \acronym{ISO8601} format (YYYY-MM-DD),
#'  \emph{e.g.} \dQuote{2020-04-26}.
#'
#' @param daily_vals `list` of model variables which have been calculated for days prior to the `i_date`
#' @param weather_dat `data.table` of weather observations which includes the query date `i_date`
#'
#' @return a `data.table` of values generated for the day `i_date`
#'
#'
#' @examples
one_day <- function(i_date,
                    daily_vals,
                    weather_dat,
                    gp_rr,
                    max_gp,
                    max_new_gp,
                    paddock,
                    spore_interception_parameter) {

  # expand time to be hourly
  i_time <- rep(i_date, 24) + lubridate::dhours(0:23)

  # subset weather data by day
  weather_day <-
    weather_dat[times %in% i_time, ]

  # obtain summary weather for i_day
  i_mean_air_temp <- mean(weather_day[, temp])
  i_wet_hours <- weather_day[1, wet_hours]
  i_rainfall <- sum(weather_day[, rain], na.rm = TRUE)

  # Start building a list of values for 'i'
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

  # Update Growing points for non-infected coords for time i
  i_new_gp <-
    calc_new_gp(current_growing_points = daily_vals[.N, gp_standard],
                       gp_rr = gp_rr,
                       max_gp = max_gp,
                       mean_air_temp = i_mean_air_temp)

  day_i_vals[["gp_standard"]] <-
    daily_vals[.N, gp_standard] + i_new_gp

  day_i_vals[["new_gp"]] <- i_new_gp


  # Update growing points for paddock coordinates
  if(i_wet_hours > 0){
    spread_spores(wet_hours = wet_hours,
                  weather_hourly = weather_day,
                  paddock = paddock,
                  max_gp =  max_gp,
                  max_new_gp = max_new_gp,
                  spore_interception_parameter = spore_interception_parameter)
    interception_probability()
  }




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
