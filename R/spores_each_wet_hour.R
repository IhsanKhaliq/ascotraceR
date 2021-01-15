#' Calculates number of conidia dispersed during each wet hour
#'
#' 'spores_each_wet_hour()' calcutates number of conidia dispersed during each
#' wet hour when rainfall threshold is reached
#'
#' @param h hour in the day which the spores for each wet hour are calculated
#' @param weather_hourly hourly weather for only the day for which the spores are being estimated
#' @param infective_elements_list ??
#' @keywords internal
#' @noRd


spores_each_wet_hour <- function(h,
                                 weather_hourly,
                                 infective_elements_list) {

  newly_infected_list <- vector(mode = "list")

  # obtain weather data for hour_i
  rain_in_hour <- weather_hourly[h, "rain"]
  average_wind_speed_in_hour <- weather_hourly[h, "ws"]
  wind_direction_in_hour = weather_hourly[h, "wd"]
  stdev_wind_direction_in_hour = weather_hourly[h, "wd_sd"]

  for (i_source in 1:length(infective_elements_list)) {
    newly_infected_list <- c(
      newly_infected_list,
      spores_from_1_element(
        infective_elements_list[[walrus]],
        rain_in_hour,
        wind_direction_in_hour,
        average_wind_speed_in_hour,
        stdev_wind_direction_in_hour
      )
    )
  }
  return(newly_infected_list)
}
