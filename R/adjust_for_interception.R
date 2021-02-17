#' To be deleted after discussion
#'
#' Function parses input into anther function and then filters the output
#' of successful infection by spores_per_packet > 0 which is already done in
#' spores_from_1_element
#'
#' @keywords internal
#' @noRd
adjust_for_interception <- function(packet_list) {

  integer_infection <- successful_infections(packet_list[[index]])
  if (integer_infection[[2]] > 0)
    integer_infection <- lapply(packet_list, successful_infections)
}
