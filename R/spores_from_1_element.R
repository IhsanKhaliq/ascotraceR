#' Infective spores per unit area
#'
#' Returns the number of `spore_packets` as a proportion of the spore aggregation limit (spores_per_packet).
#' A `spore_packets` being the number of spores dispersed per growing point which is capable of causing
#' infection on a uninfected growing point.
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
#' @param spore_aggregation_limit When spores/summary unit (n) is <= this value n spores
#'  are produced as individuals.  When greater they are produced in sporeAggregationLimit
#'  groups of sporeAggregationLimit spores  default: \code{1000}
#' @param rain_cauchy_parameter parameter used in the cauchy distribution used in
#'  determining the spread of spores due to rain splashes. default: \code{0.5}
#' @param paddock data.table of x and y coordinates; provides the dimensions of the poaddock
#'  so function only returns target_coordinates in the paddock area.
#' @keywords internal
#' @noRd
spores_from_1_element <-
  function(paddock_source,
           sporesPerInfectiveGPPerWetHour = 0.15,
           max_interception_probability,
           wind_direction_in_hour,
           average_wind_speed_in_hour,
           stdev_wind_direction_in_hour,
           spore_aggregation_limit = 1000,
           rain_cauchy_parameter = 0.5,
           paddock
           ) {

    # this might be able to be calculated at the spread_spores level, and If statement should come first
    # given that it is if == 0
    spore_packets <- potentially_effective_spores(sporesPerInfectiveGPPerWetHour = sporesPerInfectiveGPPerWetHour,
                                                  max_interception_probability = max_interception_probability,
                                                  paddock_source["sporulating_gp"])

    degree <- round(pi,6)/180


    if (spore_packets > spore_aggregation_limit) {
      spores_per_packet <- spore_packets / spore_aggregation_limit
      spore_packets <- spore_aggregation_limit
    } else{
      spores_per_packet = 1
    }

    if(spore_packets == 0){return(NULL)}

    # this for loop needs improvement so it is not growing a data.table
    target_coordinates <-
      lapply(seq_len(spore_packets), function(x) {
        wind_d <- wind_distance(average_wind_speed_in_hour)
        wind_a <-
          wind_angle(wind_direction_in_hour, stdev_wind_direction_in_hour)
        splash_d <- splash_distance(rain_cauchy_parameter)
        splash_a <- splash_angle()

        # Check this is correct use of cos and sin from blackspot package
        width_distance <-
          wind_d * cos(wind_a * degree) +
          splash_d * cos(splash_a * degree)

        length_distance <-
          wind_d * sin(wind_a * degree) +
          splash_d * sin(splash_a * degree)

        target_address <-
          address_from_centre_distance(c(width_distance, length_distance),
                                       paddock_source[c("x", "y")])
        return(target_address)

        #data.table::rbindlist(new_infections, c(target_address, spores_per_packet)) # needs double checking
      })


    new_infections <- data.table::rbindlist(target_coordinates)
    new_infections$spores_per_packet <- spores_per_packet

    new_infections <- new_infections[x >= min(paddock[, x]) &
                                       x <= max(paddock[, x]) &
                                       y >= min(paddock[, y]) &
                                       y <= max(paddock[, y]) , ]

    return(new_infections)
# I think adjust_for_interception should be moved up to the
# spread_spores function as it depends on other paddock parameters
    #return(adjust_for_interception(new_infections))

  }
