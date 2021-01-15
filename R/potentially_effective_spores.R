potentially_effective_spores <- function(infected_source_address,
                                         sporesPerInfectiveGPPerWetHour,
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
