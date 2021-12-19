#' Wind distance dispersal parameter
#'
#' `wind_distance()` Determines distance conidia disperse, in metres, by wind
#' driven rain Conidia are assumed to spread from the centre of each subunit.
#' The destination subunit, where conidia land, could be the same subunit or
#' another subunit within or outside the paddock.
#'
#' @param average_wind_speed_in_fifteen_minutes A *numeric* value in m/s
#' @param wind_cauchy_multiplier A scaling parameter to estimate a Cauchy
#'  distribution which resembles the possible distances a conidium travels due
#'  to wind dispersal. Defaults to `0.015`.
#' @param PSPH A numeric vector estimated from '.estimate_spore_discharge()'
#' @return Numerical vector, which returns distance conidia dispersed by wind
#'   driven rain from the source of infection
#' @examples
#' wind_distance(10) # returns a single estimate
#' wind_distance(10, PSPH = 10) # returns 10 estimates
#' wind_distance(15, PSPH = c(5, 5)) # returns 10 estimates
#' @keywords internal
#' @noRd
wind_distance <-
  function(average_wind_speed_in_fifteen_minutes,
           wind_cauchy_multiplier = 0.015,
           PSPH = 1) {
    w_d <- lapply(PSPH, function(peakSPH) {
      abs(
        stats::rcauchy(
          n = peakSPH,
          location = 0,
          scale = wind_cauchy_multiplier * average_wind_speed_in_fifteen_minutes
        )
      )
    })
    return(unlist(w_d))
  }
