#' Describes the Growth of Chickpea
#'
#' 'new_growing_points()' describes the growth of chickpea in terms of development of growing
#' points. Upon germination, each germinated seed produces one growing point that multiplies at a rate
#'that is a function of temperature and is limited by proximity to a maximum growing point density.
#'
#' @param current_growing_points Are the current number of growing points at an iteration period
#' @param gp_rr Chickpea growing points (meristems) replication rate as a
#'  proportion of one per degree day.
#' @param max_gp Maximum number of chickpea growing points (meristems) allowed
#'  per square meter.
#' @param mean_air_temp Is average daily temperature in Celcius
#' @return Integer giving the number of new growing points formed that day
#' @example
#' new_growing_points(current_growing_points = 500, mean_air_temp = 20, gp_rr = 0.0065, max_gp = 15000)
#'
new_growing_points <-
  function(current_growing_points,
           gp_rr,
           max_gp,
           mean_air_temp) {

    current_growing_points *
      growing_points_replication_rate *
      mean_air_temp *
      (1 - current_growing_points / max_gp)
  }
