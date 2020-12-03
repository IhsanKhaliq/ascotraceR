#' Calculates number of conidia dispersed during each wet hour
#'
#' 'spores_each_wet_hour()' calcutates number of conidia dispersed during each
#' wet hour when rainfall threshold is reached
#'
#'  @param data_line??
#'  @param newly_infected_list??
#'  @param average_wind_speed_in_hour Numeric value in m/s
#'  @param wind_direction_in_hour Numeric value in degrees
#' @param spores_from_1_element ??
#' @param infective_elements_list ??
#' @keywords internal
#' @noRd

spores_each_wet_hour <- function(data_line, hour) {
  newly_infected_list <- vector(mode = "list")

  rain_column <- hour + 4
  rain_in_hour <-
    weather_data[data_line, "rainColumn"]
  average_wind_speed_in_hour = weather_data[data_line, "rain_column"] # [[2]]
  wind_direction_in_hour = weather_data[data_line, "rain_column"] #[[4]]
  stdev_wind_direction_in_hour = weather_data[data_line, "rain_column"] #[[5]]
  for (walrus in 1:length(infective_elements_list)) {
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
