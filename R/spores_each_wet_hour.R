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
#' @param spores_per_gp_per_wet_hour Number of spores produced per sporulating growing point each wet hour.
#'   Also known as the 'spore_rate'. Value is dependent on the susceptibility of the host genotype.
#' @keywords internal
#' @noRd


spores_each_wet_hour <- function(h,
                                 weather_hourly,
                                 paddock,
                                 max_interception_probability,
                                 spore_interception_parameter,
                                 spores_per_gp_per_wet_hour) {
  rain <-
    ws <-
    wd <- wd_sd <- sporulating_gp <- spores_per_packet <- x <- y <-
    NULL

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

  paddock_infective <- paddock[sporulating_gp > 0,]

  if(nrow(paddock_infective)== 0){
    stop("Can't detect any infection, please check sum(paddock$sporulating_gp > 0) is >= 1")
  }

  newly_infected_dt <-
    future.apply::future_apply(
      X = paddock_infective,
      MARGIN =  1,
      future.seed = TRUE,
      FUN =  spores_from_1_element,
      spores_per_gp_per_wet_hour = spores_per_gp_per_wet_hour,
      max_interception_probability = max_interception_probability,
      wind_direction_in_hour = wind_direction_in_hour,
      average_wind_speed_in_hour = average_wind_speed_in_hour,
      stdev_wind_direction_in_hour = stdev_wind_direction_in_hour,
      paddock = paddock
    )

  if(is.null(newly_infected_dt)){
    return(data.table(x = numeric(),
                      y = numeric(),
                      spores_per_packet = numeric()))
  }else{
    newly_infected_dt <- rbindlist(newly_infected_dt)
  }


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
