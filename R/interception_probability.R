#' Probability of Interception of Conidia
#'
#' `interception_probability()` estimates the probability of conidia not landing
#' on susceptible growing points
#'
#' @param target_density is the density of susceptible growing points
#' @param k is a dimensionless parameter
#' @examples
#' # a moderate susceptible growing point density
#' interception_probability(target_density = 5 * 40,
#'                          k = 0.00006 * (15000 / 350))
#'
#' # a very high density approaches a probability of 1
#' interception_probability(3000, 5)
#'
#' @keywords internal
#' @noRd
interception_probability <- function(target_density, k) {
  1 - exp(-k * target_density)
}
