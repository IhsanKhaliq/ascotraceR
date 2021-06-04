#' Calculates paddock dimensions in the model, not in the paddock
#'
#' Determines paddock dimensions in metres in the model not in paddock.
#' @param paddock_width is the width of the paddock in metres.
#' Defaults value `200`.
#' @param paddock_length is the length of the paddock in metres.
#' Defaults value `200`.
#' @param summary_unit_width is the width of an observation quadrat in metres.
#'  Defaults value `80`.
#' @param summary_unit_length is the length of an observation quadrat in metres.
#' Defaults value `80`
#' @return A numeric value of the dimension of a paddock in metres
#' @keywords internal
#' @noRd
paddock_dimensions <-
  function(paddock_width = 200,
           summary_unit_width = 80,
           paddock_length = 200,
           summary_unit_length = 80) {
    c(
      ceiling(paddock_width / summary_unit_width),
      ceiling(paddock_length / summary_unit_length)
    )
  }
