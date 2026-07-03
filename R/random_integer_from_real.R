#' Rounds up or down real numbers depending on the values
#'
#' @param r a `numeric` real number to stochastically round to an integer.
#'   The fractional part of `r` is used as the probability of rounding up.
#' @return an `integer`
#' @examples
#' set.seed(42)
#' # ~90% chance of rounding up to 6, ~10% chance of rounding down to 5
#' random_integer_from_real(5.9)
#' @keywords internal
#' @noRd

random_integer_from_real <- function(r) {
  fraction <- r %% 1
  r_stand <- stats::runif(1)
  fcase(
    fraction == 0,
    as.integer(r),
    r_stand < fraction,
    as.integer(ceiling(r)),
    r_stand >= fraction,
    as.integer(floor(r))
  )
}
