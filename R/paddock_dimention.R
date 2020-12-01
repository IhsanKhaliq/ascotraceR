#' calculates paddock unit dimensions
#'
#' 'paddock_dimention()' determines paddock dimensions in metres in the model, not in paddock
#' @param paddock_width is the width of the paddock in metres
#' @param paddock_length is the length of the paddock in metres
#' @param summary_unit_width is the width of the observation quadrat in metres
#' @param summary_unit_length is the length of the observation quadrat in metres
#' @return the dimension of a paddock in metres
#' @keywords internal
#' @noRd
paddock_dimentions <- function() {
  c(
    ceiling(paddock_width / summary_unit_width),
    ceiling(paddock_length / summary_unit_length)
  )
}
