#' Updates the number of uninfective (health) growing points
#'
#' 'update_ref_uninfective_growing_points()' updates the number of
#' growing points that are not infected by A. rabiei
#'
#' @param mean_air_temp is average air temperature in celcius
#' @param new_growing_points are the number of new growing points formed per day
#' @param ref_new_growing_points are reference new growing points
#' @keywords internal
#' @noRd
update_ref_uninfective_growing_points <- function (mean_air_temp) {
  ref_new_growing_points <-
    new_growing_points(ref_uninfective_growing_points, mean_air_temp)
  return(ref_uninfective_growing_points + ref_new_growing_points)
}
