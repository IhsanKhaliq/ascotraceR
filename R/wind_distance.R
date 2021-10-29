#' Wind distance dispersal parameter
#'
#' `wind_distance()` determines distance conidia disperse, in metres, by wind
#' Conidia are assumed to spread from the centre of each subunit.
#' The destination subunit, where conidia land, could be the same subunit or
#'  another subunit within or outside the paddock.
#' If conidia land outside the paddock, the model can simulate the spread
#'  outside the paddock when the wrapping option is chosen.
#' When the wrapping option is not chosen, the model simulate disease spread
#'  within the paddock
#'
#' @param average_wind_speed_in_fifteen_minutes A *numeric* value in m/s
#' @param wind_cauchy_multiplier A Cauchy scaling parameter
#' @param PSPH A numeric vector estimated from '.estimate_spore_discharge()'
#' @return numerical vector, which returns distance conidia dispersed by wind
#'  from the source of infection
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
