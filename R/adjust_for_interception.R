#' To be deleted after discussion
#'
#' @keywords internal
#' @noRd
adjust_for_interception <- function(packet_list) {
  integer_infection = successful_infections[packet_list[[index]]]
  if (integer_infection[[2]] > 0)
    integer_infection <- lapply(packet_list, successful_infections)
}
