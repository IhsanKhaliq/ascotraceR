#' Area of a summary unit (observation quadrat or paddock)
#'
#' 'per_area()' gives information on the length of a summary unit (observation quadrate or paddock)
#'
#' @param summary_unit_area paddock or an observation quadrat area in metres
#' @param per_summary_unit each summary unit
#' @return length of a summary unit in metres
#' @keywords internal
#' @noRd
per_area <- function(per_summary_unit) {
  per_summary_unit * summary_unit_area
}
