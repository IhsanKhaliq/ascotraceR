#' Total daily rain
#'
#' 'daily_rain()' calculates total daily rain in degree celcius
#' @param wet_hours hours with rain
#' @param rain_in_hour total amount of rain in an hour
#' @noRd
#'
daily_rain <- function(wet_hours, data_line) {
  total_rain = 0
  for (hour in seq_len(wet_hours)) {
    rain_column = hour + 4
    rain_in_hour = weather_data[data_line, rain_column]
    total_rain = total_rain + rain_in_hour
  }
  return(total_rain)
}
