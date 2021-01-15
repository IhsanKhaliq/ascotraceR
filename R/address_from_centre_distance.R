#' To be deleted after discussion
#'
#' @keywords internal
#' @noRd

address_from_centre_distance <-
  function(offset_distance, start_address) {
    start_address[1] +
      floor(0.5 + offset_distance[1])
    start_address[2] +
      floor(0.5 + offset_distance[2])
  }
