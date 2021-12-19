#' Estimates Bearing of Conidia Dispersed by a Rain Splash
#'
#' (splash_angle)' gives the angle of dispersal due to rain splash, which is a
#' random number with uniform probability from 1-360 degree
#' @param min is the lowest angle of dispersion
#' @param max is the highest angle of dispersion
#' @keywords internal
#' @noRd

splash_angle <- function() {
  stats::runif(n = 1,
               min =  1,
               max = 360)
}
