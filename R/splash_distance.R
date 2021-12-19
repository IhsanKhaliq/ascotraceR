#' Splash Distance Dispersal Parameter
#'
#' `wind_distance()` Determines distance conidia travel, in metres, via a rain
#' splash
#'
#' @param splash_cauchy_parameter A Cauchy scaling parameter
#' @param PSPH A numeric vector estimated from '.estimate_spore_discharge()'
#' @return numerical vector, which returns distance conidia dispersed by a rain
#'   splash from the source of infection
#' @examples
#' wind_distance(10) # returns a single estimate
#' wind_distance(10, PSPH = 10) # returns 10 estimates
#' wind_distance(15, PSPH = c(5, 5)) # returns 10 estimates
#' @keywords internal
#' @noRd


splash_distance <-
   function(splash_cauchy_parameter = 0.015,
            PSPH = 1) {
      s_d <- lapply(PSPH, function(peakSPH) {
         abs(stats::rcauchy(
            n = peakSPH,
            location = 0,
            scale = splash_cauchy_parameter
         ))
      })
      return(unlist(s_d))
   }
