#' Updates the number of uninfective (health) growing points
#'
#' 'update_ref_uninfective_growing_points()' updates the number of
#' growing points that are not infected by A. rabiei
#'
#' @param mean_air_temp is average air temperature in celcius
#' @param new_growing_points are the number of new growing points formed per day
#' @param ref_new_growing_points are reference new growing points
#'
#' @keywords internal
#' @noRd
calc_new_noninfectived_gp <-
  function(current_growing_points,
           gp_rr,
           max_growing_points,
           mean_air_temp,
           new_gp) {

  new_gp <-
    current_growing_points *
    gp_rr *
    mean_air_temp *
    (1 - current_growing_points / max_growing_points)

  return(ref_uninfective_growing_points + new_gp)
}
#Probably remove this function replaced by new_growing_points
