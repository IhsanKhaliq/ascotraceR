#' Determines??
#'
#' 'spores_from_1_element()' calculates conidia dispersed from??
#' @param source_address where conidia dispersal originates
#' @param potentially_effective_spores conidia with the ability to cause infection
#' @param spore_aggregation_limit ??
#' @param spores_per_packet ??
#' @param spore_packets ??
#' @param width_distance ??
#' @param length_distance ??
#' @param target_address where conidia land
#' @param new_infections new lesions
#' @param adjus_for_interception ??
#' @keywords internal
#' @noRd
spores_from_1_element <-
  function(source_address,
           rain_in_hour,
           wind_direction_in_hour,
           average_wind_speed_in_hour,
           stdev_wind_direction_in_hour) {
    spore_packets <- potentially_effective_spores[source_address]
    if (spore_packets > spore_aggregation_limit) {
      spores_per_packet <- spore_packets / spore_aggregation_limit
      spore_packets <- spore_aggregation_limit
    } else{
      spores_per_packet = 1
    }
    for (n in spore_packets) {
      wind_d <- wind_distance(average_wind_speed_in_hour)
      wind_a <-
        wind_angle(wind_direction_in_hour, stdev_wind_direction_in_hour)
      splash_d <- splash_distance(rain_cauchy_parameter)
      splash_a <- splash_angle

      width_distance <-
        wind_d * cos(wind_a * degree) +
        splash_d * cos(splash_a * degree)

      length_distance <-
        wind_d * sin(wind_a * degree) +
        splash_d * sin(splash_a * degree)

      target_address <-
        address_from_centre_distance(c(width_distance, length_distance),
                                     source_address)

      data.table::rbindlist(new_infections, c(target_address, spores_per_packet)) # needs double checking
    }

    new_infections <- wrap_addresses(new_infections)
    return(adjust_for_interception(new_infections))

  }
