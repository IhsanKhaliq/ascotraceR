#' Estimates distance conidia travelled from the origin of dispersal
#'
#' 'address_from_edge_distance()' gives information on the possible destination
#' where conidia may land when dispersed by wind driven rain or rain splash
#'
#' @param summary_unit_width is the width of the observation quadrat/cell, which is equal to 1 square metre
#' @param summary_unit_length is the length of the observation quadrat/cell, which is equal to 1 square metre
#' @param location[1] represents origin of dispersal
#' @param location[2] represents the destination where conidia land
#' @return distance in metres
#'
#' @keywords internal
#' @noRd


address_from_edge_distance <- function (location) {
  c(1 + floor(location[1] / summary_unit_width),
    1 + floor(location[2] / summary_unit_length))
}


