#' Describes growth of chickpea
#'
#' 'growth()' determines growth of chickpea in terms of development of growing points
#' that is a function of temperature and is limited by the proximity of maximum
#' growing points limit for a chickpea cultivars
#' @param mean_air_temp is average air temperature in celcius
#' @param update_ref_uninfective_growing_points updated healthy growing points?
#' @param update_growing_points_all_infective_elements updated infected growing points?
#' @keywords internal
#' @noRd
growth <- function(mean_air_temp) {
  update_ref_uninfective_growing_points[mean_air_temp]
  update_growing_points_all_infective_elements[mean_air_temp]
}
