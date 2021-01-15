#' Estimate the potentially effective spore
#'
#' @param infected_source_address vector of length two giving the paddock coordinates with ascochyta infection
#' @param sporesPerInfectiveGPPerWetHour estimated number of spores produced in one hour from a single chickpea growing point
#' @param max_interception_probability maximum interception probability
#' @param paddock_infected_gp
#'
#' @return a double providing the potentially infective spores at the input coordinates
#'
#' @examples
potentially_effective_spores <- function(sporesPerInfectiveGPPerWetHour,
                                         max_interception_probability,
                                         paddock_infected_gp) {
  if (paddock_infected_gp <= 0) {
    return(0)
  } else{
    expected_effective_spores <-
      sporesPerInfectiveGPPerWetHour *
      max_interception_probability *
      paddock_infected_gp

    p_e_s <-
      rpois(1, expected_effective_spores)

    return(p_e_s)
  }

}
