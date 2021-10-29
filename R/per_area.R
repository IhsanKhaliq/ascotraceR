#' Area of a summary unit (an observation quadrat or a paddock)
#'
#' 'per_area()' gives information on the area of a summary unit (an observation
#' quadrat or a paddock) in metres
#'
#' @param summary_unit_area a paddock or an observation quadrat area in metres
#' @param per_summary_unit each summary unit
#' @return length of a summary unit in metres
#' @keywords internal
#' @noRd
per_area <- function(per_summary_unit) {
  per_summary_unit * summary_unit_area
}
