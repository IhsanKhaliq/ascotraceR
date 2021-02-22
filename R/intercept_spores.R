#' Calculate spore interception parameter
#'
#' The parameter which controls the relationship between uninfected growing points per area
#' and probability of a spore causing an infection.
#' The spore_interception_parameter gives an estimate of spores not producing
#' infections
#'
#' @param spore_interception_multiplier multiplier to determine spore interception parameter.
#'  Defaults to \code{0.00006}
#' @param max_growing_points_limit Are the maximum number of growing points per square metre.
#'  Defaults to \code{15000}
#' @param max_new_growing_points_limit A *numeric* number indicating number of new growing points
#' developed per square metre in an iteration period of one day.  Defaults to \code{350}
#'  @keywords internal
#'  @noRd
intercept_spores <- function(spore_interception_multiplier = 0.00006,
                             max_growing_points_limit,
                             max_new_growing_points_limit) {
  spore_interception_multiplier * max_growing_points_limit /
    max_new_growing_points_limit
}
