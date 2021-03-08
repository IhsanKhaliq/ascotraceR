#' Estimates distance conidia travelled from the origin of dispersal
#'
#' 'address_from_edge_distance()' gives information on the possible destination
#' where conidia may land when dispersed by wind driven rain or rain splash
#'
#' @param summary_unit_width an *integer* value is the width of the observation quadrat/cell,
#' which is equal to 1 square metre
#' @param summary_unit_length an *integer* representing the length of the observation
#' quadrat/cell,which is equal to 1 square metre
#' @param location[1] an *integer* value representing origin of dispersal
#' @param location[2] an *integer* representing the destination where conidia land
#' @return distance *integer* value in metres
#'
#' @keywords internal
#' @noRd


address_from_edge_distance <- function (location) {
  c(1 + floor(location[1] / summary_unit_width),
    1 + floor(location[2] / summary_unit_length))
}


