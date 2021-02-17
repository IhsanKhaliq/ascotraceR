#' To be deleted after discussion
#'
#' @keywords internal
#' @noRd

random_integer_from_real <- function (r) {
  fraction <- r %% 1
  r_stand <- runif(1)
  fcase(fraction == 0, return(as.integer(r)),
        r_stand < fraction, return(as.integer(ceiling(r))),
        r_stand >= fraction, return(as.integer(floor(r)))
  )
}
