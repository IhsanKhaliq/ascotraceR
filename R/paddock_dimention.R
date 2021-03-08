#' calculates paddock dimensions in the model, not in the paddock
#'
#' 'paddock_dimention()' determines paddock dimensions in metres in the model,
#' not in paddock
#' @param paddock_width is the width of the paddock in metres.
#' Defaults value \code{200}.
#' @param paddock_length is the length of the paddock in metres.
#' Defaults value \code{200}.
#' @param summary_unit_width is the width of an observation quadrat in metres. Defaults value \code{80}
#' @param summary_unit_length is the length of an observation quadrat in metres.
#' Defaults value \code{80}
#' @return the dimension of a paddock in metres
#' @keywords internal
#' @noRd
paddock_dimentions <- function() {
  c(
    ceiling(paddock_width / summary_unit_width),
    ceiling(paddock_length / summary_unit_length)
  )
}
