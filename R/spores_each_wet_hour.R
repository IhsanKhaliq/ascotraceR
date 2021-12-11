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
#' @param splash_cauchy_parameter A parameter used in the Cauchy distribution
#'  and describes the median distance spores travel due to rain splashes.
#'  Defaults to `0.5`.
#' @param wind_cauchy_multiplier A scaling parameter to estimate a Cauchy
#'  distribution which resembles the possible distances a conidium travels due
#'  to wind dispersal. Defaults to `0.015`.
#' @param rainfall_multiplier logical values will turn on or off rainfall
#'  multiplier default method. The default method increases the number of spores
#'  spread per growing point if the rainfall in the spore spread event hour is
#'  greater than one. Numeric values will scale the number of spores spread per
#'  growing point against the volume of rainfall in the hour. Defaults to
#'  `FALSE`.
#' @keywords internal
#' @noRd

spores_each_wet_hour <- function(h,
                                 weather_hourly,
                                 paddock,
                                 spore_interception_parameter,
                                 max_interception_probability,
                                 spores_per_gp_per_wet_hour,
                                 splash_cauchy_parameter = 0.5,
                                 wind_cauchy_multiplier = 0.015,
                                 rainfall_multiplier = FALSE) {
  rain <- ws <- wd <- wd_sd <- infectious_gp <- spores_per_packet <- x <- y <-
    NULL

  # obtain weather data for hour_i

  rain_in_hour <- weather_hourly[h, rain]
  average_wind_speed_in_hour <- weather_hourly[h, ws]
  wind_direction_in_hour <-  weather_hourly[h, wd]
  stdev_wind_direction_in_hour <-  weather_hourly[h, wd_sd]
  spores_per_gp_per_wet_hour <- fifelse(rainfall_multiplier == FALSE |
                                          rain_in_hour <= 1,
                                        spores_per_gp_per_wet_hour,
                                        spores_per_gp_per_wet_hour * (
                                          as.numeric(rainfall_multiplier) *
                                            rain_in_hour))

  # get data.table of inoculum coordinates
  paddock_infective <- paddock[infectious_gp > 0 &
                                 stubble_lesions > 0, ]

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
      splash_cauchy_parameter = splash_cauchy_parameter,
      wind_cauchy_multiplier = wind_cauchy_multiplier,
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
