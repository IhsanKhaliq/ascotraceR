#' Calculates area of a summary unit
#'
#' 'summary_unit_area()' calculates area of a summary unit i.e., an observation
#' quadra in metres
#' @param  summary_unit_width width in metres. Defaults value `1`.
#' @param summary_unit_length length in metres. Defaults value `1`.
#' @keywords internal
#' @noRd

summary_unit_area <-
  function(summary_unit_width, summary_unit_length) {
    summary_unit_width * summary_unit_length
  }
