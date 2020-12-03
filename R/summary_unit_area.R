#' Calculates area of a summary unit
#'
#' 'summary_unit_area()' calculates area of a summary unit i.e., an observation
#' quadrat or paddock in metres
#' @param  summary_unit_width width in metres
#' @param summary_unit_length length in metres
#' @keywords internal
#' @noRd

summary_unit_area <-
  function() {
    summary_unit_width * summary_unit_length
  }
