#' Infective spores per unit area
#'
#' Returns the number of `spore_packets` as a proportion of the spore
#'  aggregation limit (spores_per_packet). `spore_packets` being the number of
#'  spores dispersed per growing point which is capable of causing infection on
#'  an uninfected growing point. `spores_from_1_element()` calculates conidia
#'  dispersed from??
#' @param paddock_source data.table of coordinates which contains sporulating
#'  growing points and the one element from which conidia dispersal originates.
#' @param spores_per_gp_per_wet_hour The 'spore rate' or conidia with the ability
#'  to cause infection
#' @param max_interception_probability double with length of one; Estimated using
#'  the `spore_interception_parameter`, see function `interception_probability()`
#' @param wind_direction_in_hour wind_direction
#' @param average_wind_speed_in_hour avg wind dir
#' @param stdev_wind_direction_in_hour std wind dir
#' @param spore_aggregation_limit When spores/summary unit (n) is <= this value
#'  n spores are produced as individuals.  When greater they are produced in
#'  sporeAggregationLimit groups of sporeAggregationLimit spores  default:
#'  `1000`
#' @param rain_cauchy_parameter parameter used in the cauchy distribution and
#'  describes the median distance of spore travel due to rain splashes. default:
#'   `0.5`
#' @param paddock data.table of x and y coordinates; provides the dimensions of
#'  the paddock so function only returns target_coordinates in the paddock area.
#' @keywords internal
#' @noRd
spores_from_1_element <-
  function(paddock_source,
           spores_per_gp_per_wet_hour = 0.15,
           max_interception_probability,
           wind_direction_in_hour,
           average_wind_speed_in_hour,
           stdev_wind_direction_in_hour,
           spore_aggregation_limit = 1000,
           rain_cauchy_parameter = 0.5,
           paddock) {
    x <- y <- NULL

    # this might be able to be calculated at the spread_spores level, and If statement should come first
    # given that it is if == 0
    spore_packets <-
      potentially_effective_spores(
        spores_per_gp_per_wet_hour = spores_per_gp_per_wet_hour,
        max_interception_probability = max_interception_probability,
        paddock_source["sporulating_gp"]
      )

    degree <- 0.01745


    if (spore_packets > spore_aggregation_limit) {
      spores_per_packet <- spore_packets / spore_aggregation_limit
      spore_packets <- spore_aggregation_limit
    } else{
      spores_per_packet = 1
    }

    if (spore_packets == 0) {
      return(NULL)
    }

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
                                       y <= max(paddock[, y]) ,]

    return(new_infections)
    # I think adjust_for_interception should be moved up to the
    # spread_spores function as it depends on other paddock parameters
    #return(adjust_for_interception(new_infections))

  }
