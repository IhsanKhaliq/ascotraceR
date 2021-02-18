#' Calculates number of conidia dispersed during each wet hour
#'
#' 'spores_each_wet_hour()' calculates number of conidia dispersed during each
#' wet hour when rainfall threshold is reached
#'
#' @param h hour in the day which the spores for each wet hour are calculated
#' @param weather_hourly data.table of hourly weather for only the day for which the spores are being estimated
#' @param paddock data.table of paddock coordinates detailing the growing points and infections at each location
#' @param max_interception_probability double with length of one
#' @param spore_interception_parameter double with length of one
#' @keywords internal
#' @noRd


spores_each_wet_hour <- function(h,
                                 weather_hourly,
                                 paddock,
                                 max_interception_probability,
                                 spore_interception_parameter) {

  # obtain weather data for hour_i

  rain_in_hour <- weather_hourly[h, rain]
  average_wind_speed_in_hour <- weather_hourly[h, ws]
  wind_direction_in_hour = weather_hourly[h, wd]
  stdev_wind_direction_in_hour = weather_hourly[h, wd_sd]

  # set a new vector of infected coordinates
  # newly_infected_list <- vector(length = sum(paddock$infected_gp))

  # this could be changed to an apply functions
  # for (i_source in which(paddock$infected)) {
  #   newly_infected_list <- c(
  #     newly_infected_list,
  #     spores_from_1_element(
  #       source_address = paddock[i_source, c("x", "y")],
  #       rain_in_hour = rain_in_hour,
  #       wind_direction_in_hour = wind_direction_in_hour,
  #       average_wind_speed_in_hour = average_wind_speed_in_hour,
  #       stdev_wind_direction_in_hour = stdev_wind_direction_in_hour
  #     )
  #   )
  # }

  paddock_infected <- paddock[infected_gp > 0,]

  newly_infected_dt <-
    rbindlist(
    apply(paddock_infected, 1, spores_from_1_element,
          max_interception_probability = max_interception_probability,
          wind_direction_in_hour = wind_direction_in_hour,
          average_wind_speed_in_hour = average_wind_speed_in_hour,
          stdev_wind_direction_in_hour = stdev_wind_direction_in_hour,
          paddock = paddock)
    )

  newly_infected_dt$spores_per_packet <-
    successful_infections(
      spore_targets = newly_infected_dt,
      paddock = paddock,
      spore_interception_parameter = spore_interception_parameter,
      max_interception_probability = max_interception_probability
    )

  # filter only successful interceptions inside the paddock
  newly_infected_dt <-
    newly_infected_dt[spores_per_packet > 0 &
                        x >= min(paddock[, x]) &
                        x <= max(paddock[, x]) &
                        y >= min(paddock[, y]) &
                        y <= max(paddock[, y]) , ]


  return(newly_infected_dt)
}
