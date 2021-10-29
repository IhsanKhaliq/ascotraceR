#' Indicates infections leading to disease or lesions development
#'
#' [successful_infections()] determines successful infections. That is,
#' infections that have developed into visible lesions
#'
#' @param spore_targets a data.table with variables x, y and spores_per_packet
#'   (formally name spore_packet)
#' @param paddock data.table containing all parameters for each 1 x 1 coordinate
#'   in the paddock
#' @param spore_interception_parameter a function of the maximum growing points
#'   limit (usually 15000 for lupin) and the maximum new growing points rate
#'   i.e. - 0.00006 * (max_gp_lim/max_new_gp)
#' @param max_interception_probability are the number of new growing points
#' @return a vector of spores_per_packet each referring to a row in
#'   spore_targets
#' @keywords internal
#' @noRd
successful_infections <- function(spore_targets,
                                  paddock,
                                  spore_interception_parameter,
                                  max_interception_probability) {
  x <-
    y <-
    new_gp <-
    summary_unit_width <- summary_unit_length <- new_gp <- NULL

  if ((is.data.table(spore_targets) |
       is.data.frame(spore_targets)) == FALSE) {
    stop("argument 'spore_targets' should be a data.table input not ",
         class(spore_targets))
  }

  suc_inf <-
    apply(spore_targets, 1, function(sp_tar) {
      address <- c(sp_tar["x"],
                   sp_tar["y"])

      spores_in_packet <-  sp_tar["spores_per_packet"]

      susceptible_growing_points <-
        paddock[x == sp_tar["x"] &
                  y == sp_tar["y"], new_gp]

      spores_in_packet <-
        random_integer_from_real(
          spores_in_packet *
            interception_probability(
              5 * susceptible_growing_points,
              spore_interception_parameter
            ) /
            max_interception_probability
        )
      return(spores_in_packet)
    })

  return(unlist(suc_inf))

}
