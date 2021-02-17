#' Indicates infections leading to disease or lesions development
#'
#' 'successful_infections()' determines successful infections. That is, infections
#'  that have developed into visible lesions
#'
#' @param spore_targets a data.table with variables x, y and spores_per_packet
#'  (formally name spore_packet)
#' @param paddock data.table containing all parameters for each 1 x 1 coordinate in the paddock
#' @param spore_interception_parameter
#' @param max_interception_probability are the number of new growing points
#' @return a vector of spores_per_packet each referring to a row in spore_targets
#' @keywords internal
#' @noRd
successful_infections <- function (spore_targets,
                                   paddock,
                                   spore_interception_parameter,
                                   max_interception_probability) {
  suc_inf <-
    apply(spore_targets,1,function(sp_tar){

      address <- c(sp_tar["x"],
                   sp_tar["y"])

      spores_in_packet <-  sp_tar["spores_per_packet"]

      susceptible_growing_points <-
        paddock[x == sp_tar["x"] &
                  y == sp_tar["y"], new_gp]

      spores_in_packet <-
        random_integer_from_real(
          spores_in_packet *
            interception_probability(5 * susceptible_growing_points,
                                     spore_interception_parameter) /
            max_interception_probability
        )
      return(spores_in_packet)
    })

  return(c(suc_inf))

}
