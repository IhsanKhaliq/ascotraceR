#' The parameter which controls the relationship between uninfected growing points per area
#' and probability of a spore causing an infection
#'
#' 'spore_interception_parameter()' gives an estimate of spores not spores not producing
#' infections
#'
#' @param max_growing_points_limit Are the maximum number of growing points chickpea can achieve
#' @param max_new_growing_points_limit A *numeric* number giving number of new growing points
#'  @keywords internal
#'  @noRd
spore_interception_parameter <- function() {
  0.00006 * max_growing_points_limit / max_new_growing_points_limit
}
