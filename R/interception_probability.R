#' Probability of Interception of Conidia

#' 'interception_probability()' estimates the probability of conidia not landing
#' on suceptible growing points
#'
#' @param target_density is the density of suceptible growing points
#' @param k is a dimensionless parameter
#' @example
#' interception_probability(3000,5)
#'
#' @keywords internal
#' @noRd



interception_probability <- function(target_density, k) {
  1 - exp(-k * target_density)
}
