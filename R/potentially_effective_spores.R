#' Estimate the potentially infective spores
#'
#' @param spores_per_gp_per_wet_hour estimated number of spores produced in one
#'  hour from a single chickpea growing point
#' @param max_interception_probability double with length of one; Estimates the
#'  maximum interception probability using the `spore_interception_parameter`,
#'  see function `interception_probability()`
#' @param paddock_infected_gp integer value giving number of sporulating growing
#'  points per paddock
#'
#' @return a double providing the potentially infective spores at the paddock_infected_gp coordinates
#' @noRd
#' @examples
#' potentially_effective_spores(spores_per_gp_per_wet_hour = 0.22,  # default parameter of the model
#'   max_interception_probability = 1,
#'   paddock_infected_gp = 100) # number of infected growing points at coordinates
potentially_effective_spores <- function(spores_per_gp_per_wet_hour,
                                         max_interception_probability,
                                         paddock_infected_gp) {


  if (paddock_infected_gp <= 0) {
    return(0)
  } else{
    expected_effective_spores <-
      spores_per_gp_per_wet_hour *
      max_interception_probability *
      paddock_infected_gp

    if(expected_effective_spores < 0) {
      stop("Function 'potentially_effective_spores() returning negative values\n
           Check parameters spores_per_gp_per_wet_hour and max_interception_probability")
    }else{

    p_e_s <-
      stats::rpois(1, expected_effective_spores)

    return(p_e_s)
    }
  }
}
