#' Calculate spore interception parameter
#'
#' The parameter which controls the relationship between uninfected growing points per area
#' and probability of a spore causing an infection.
#' The spore_interception_parameter gives an estimate of spores not producing
#' infections
#'
#' @param spore_interception_multiplier multiplier to determine spore interception parameter.
#'  Defaults to \code{0.00006}
#' @param max_growing_points_limit Are the maximum number of growing points chickpea can achieve
#' @param max_new_growing_points_limit A *numeric* number giving number of new growing points
#'  @keywords internal
#'  @noRd
intercept_spores <- function(spore_interception_multiplier = 0.00006,
                             max_growing_points_limit,
                             max_new_growing_points_limit) {
  spore_interception_multiplier * max_growing_points_limit /
    max_new_growing_points_limit
}
