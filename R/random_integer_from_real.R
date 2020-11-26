#' To be deleted after discussion
#'
#' @keywords internal
#' @noRd

random_integer_from_real <- function (r) {
  fraction <- r %% 1
  integer(ifelse(
    fraction == 0,
    return(r),
    ifelse(runif(1) < fraction,
           ceiling[r],
           floor(r))
  ))
}
