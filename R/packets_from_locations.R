#' To be deleted after discussion
#'
#' @keywords internal function
#'
#' @noRd

packets_from_locations <- function(location_list) {
  for (index in seq_along(location_list)) {
    packet_list <- dplyr::bind_rows(packet_list,
                                    c(address_from_edge_distance(location_list[[index]]), 1))
  }

  return(wrap_addresses(packet_list))
}
