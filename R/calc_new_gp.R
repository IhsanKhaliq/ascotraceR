#' Describes the Growth of Chickpea.
#'
#' `calc_new_gp()` Describes the growth of chickpea in terms of development of
#' growing points. Upon germination, each germinated seed produces one growing
#' point that multiplies at a rate that is a function of temperature and is
#' limited by proximity to a maximum growing point density.
#'
#' @param current_growing_points Are the current number of growing points at an
#'   iteration period
#' @param gp_rr Is the rate of increase of chickpea growing points in an
#'   iteration period of one day
#' @param max_gp Are the maximum number of chickpea growing points
#' @param mean_air_temp Is average daily temperature in Celsius
#' @return Integer giving the number of new growing points formed in an
#'   iteration period of one day
#'
#' @keywords internal
#' @noRd
calc_new_gp <-
  function(current_growing_points,
           gp_rr,
           max_gp,
           mean_air_temp) {

    # Check values are not lower than 0
    sapply(current_growing_points, function(cgp){
      if(cgp < 0){
        stop(call. = FALSE,
             "'current_growing_points' (value = ",cgp,") can't be < 0",
             sep = "")
      }
    })

    # calculate number of new growing points in 24 hours
    current_growing_points *
      gp_rr *
      mean_air_temp *
      (1 - current_growing_points / max_gp)
  }
