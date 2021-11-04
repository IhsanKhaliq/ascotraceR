#' Calculates number of conidia dispersed during each wet hour
#'
#' Calculates number of conidia dispersed during each wet hour when rainfall
#' threshold is reached or exceeded.
#'
#' @param h Hour in the day which the spores for each wet hour are calculated
#' @param weather_hourly [data.table] of hourly weather for only the day for
#'   which the spores are being estimated
#' @param paddock A data.table of paddock coordinates detailing the growing
#'   points and infections at each location
#' @param spore_interception_parameter A double with length of one; the maximum
#'   number of susceptible growing are 5000/350 = 14.2. The highest probability
#'   of a spore landing on the area of these 14 susceptible growing points is
#'   `0.00006 * 14.2 (i.e. 0.00006 * (max_gp_lim/max_new_gp))`. However, as the
#'   crop is always changing we need to calculate the actual probability of
#'   interception depending on the density of the crop canopy for that given
#'   time.
#' @param max_interception_probability DoubleS with length of one; Estimated
#'   using the `spore_interception_parameter`, see function
#'   `interception_probability()`
#' @param spores_per_gp_per_wet_hour Number of spores produced per sporulating
#'   growing point each wet hour. Also known as the `spore_rate`. Value is
#'   dependent on the susceptibility of the host genotype.
#' @keywords internal
#' @noRd


spores_each_wet_hour <- function(h,
                                 weather_hourly,
                                 paddock,
                                 spore_interception_parameter,
                                 max_interception_probability,
                                 spores_per_gp_per_wet_hour) {
  rain <- ws <- wd <- wd_sd <- infectious_gp <- spores_per_packet <- x <- y <-
    NULL

  # obtain weather data for hour_i

  rain_in_hour <- weather_hourly[h, rain]
  average_wind_speed_in_hour <- weather_hourly[h, ws]
  wind_direction_in_hour <-  weather_hourly[h, wd]
  stdev_wind_direction_in_hour <-  weather_hourly[h, wd_sd]

  # get data.table of infected coordinates
  paddock_infective <- paddock[infectious_gp > 0, ]

  if (nrow(paddock_infective) == 0) {
    stop(
      call. = FALSE,
      "Can't detect any infection, please check that",
      "`sum(paddock$infectious_gp > 0)` is >= 1")
  }

  exposed_dt <-
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

  if (is.null(exposed_dt)) {
    return(data.table(
      x = numeric(),
      y = numeric(),
      spores_per_packet = numeric()
    ))
  } else{
    exposed_dt <- rbindlist(exposed_dt)
  }


  exposed_dt$spores_per_packet <-
    successful_infections(
      spore_targets = exposed_dt,
      paddock = paddock,
      spore_interception_parameter = spore_interception_parameter,
      max_interception_probability = max_interception_probability
    )

  # filter only successful interceptions inside the paddock
  exposed_dt <-
    exposed_dt[spores_per_packet > 0 &
                        x >= min(paddock[, x]) &
                        x <= max(paddock[, x]) &
                        y >= min(paddock[, y]) &
                        y <= max(paddock[, y]) ,]


  return(exposed_dt)
}
