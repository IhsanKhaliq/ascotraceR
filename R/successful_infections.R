#' Indicates infections leading to disease or lesions development
#'
#' 'successful_infections()' determines successful infections. That is, infections
#'  that have developed into visible lesions
#'
#' @param spore_targets a data.table with variables x, y and spores_per_packet
#'  (formally name spore_packet)
#' @param spores_in_packet indictes??
#' @param susceptible_growing_points are growing points susceptible to disease
#' @param paddock_new_growing_points indicate??
#' @param ref_new_growing_points are the number of new growing points
#' @return successful infections
#' @keywords internal
#' @noRd
successful_infections <- function (spore_targets) {

  address <- c(spore_targets["x"],
               spore_targets["y"])

  spores_in_packet <-  spore_targets["spores_per_packet"]

  susceptible_growing_points <-
        paddock[x == spore_targets["x"] &
                  y == spore_targets["y"], new_gp]


  if (susceptible_growing_points < -0.5) {
    susceptible_growing_points <- ref_new_growing_points
  } else{
    NULL
  }

  spores_in_packet <-
    random_integer_from_real(
      spores_in_packet * interception_probability(5 * susceptible_growing_points,
                                                  spore_interception_parameter) /
        max_interception_probability
    )
  return(list(address, spores_in_packet))
}
