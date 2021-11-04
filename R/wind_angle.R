#' Estimates Bearing Conidia Dispersed by Wind Driven Rain
#'
#' '(wind_angle)' samples from normal distribution to estimate the bearing of of
#' Ascochyta rabiei conidia dispersed by wind driven rain
#'
#' @param mean_wind_direction A numeric vector representing mean wind direction
#'   at a particular time interval
#' @param stdev_wind_direction Refer to standard deviation of wind_direction at
#'   a particular time interval
#' @param PSPH A numeric vector, estimated from `.estimate_spore_discharge()`
#'
#' @return A numeric vector giving information on the angle component of conidia
#'   dispersed by wind driven rain.
#'
#' @examples
#' wind_angle(10, 2) # returns a single estimate
#' wind_angle(10, 2, PSPH = 10) # returns 10 estimates
#' wind_angle(15, 2, PSPH = c(5, 5)) # returns 10 estimates
#'
#' @keywords internal
#' @noRd
#'
wind_angle <-
  function(average_wind_direction_in_fifteen_minutes,
           stdev_wind_direction_fifteen_minutes,
           PSPH = 1) {

    w_angle <-
      lapply(X = PSPH,
             FUN = stats::rnorm,
             mean = average_wind_direction_in_fifteen_minutes,
             sd = stdev_wind_direction_fifteen_minutes)

    w_angle <- unlist(w_angle)

    # if rnorm() approximates beyond 360 or lower than 0 correct for this
    w_angle <- lapply(w_angle, function(w_a) {
      if (w_a < 0) {
        w_a <- w_a + 360
      }
      if (w_a >= 360) {
        w_a <- w_a - 360
      }
      return((w_a))
    })
    return(unlist(w_angle))
  }
