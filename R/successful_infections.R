#' Indicates infections leading to disease or lesions development
#'
#' 'successful_infectoins()' determines successful infections. That is, infections
#'  that have developed into visible lesions
#'
#' @param spore_packet indicates??
#' @param spores_in_packet indictes??
#' @param susceptible_growing_points are growing points suceptible to disease
#' @param paddock_new_growing_points indicate??
#' @param ref_new_growing_points are the number of new growing points
#' @return successful infections
#' @keywords internal
#' @noRd
successful_infections <- function (spore_packet) {
  address <- spore_packet[1] %>%
    spores_in_packet <-  spore_packet[2] %>%
      susceptible_growing_points <-
        paddock_new_growing_points[address[1] %% address[2]]
      if (susceptible_growing_points < -0.5) {
        susceptible_growing_points <- ref_new_growing_points
      } else{
        NULL
      }

      spores_in_packet <-
        random_integer_from_real(
          spores_in_packet * interception_probability(
            5 * susceptible_growing_points,
            spore_interception_parameter
          ) /
            max_interception_probability
        )
      return(list(address, spores_in_packet))
}
