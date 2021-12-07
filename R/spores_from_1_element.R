#' Infective spores per unit area
#'
#' Returns the number of `spore_packets` as a proportion of the spore
#' aggregation limit (spores_per_packet). `spore_packets` being the number of
#' spores dispersed per growing point which is capable of causing infection on
#' an uninfected growing point. `spores_from_1_element()` calculates conidia
#' dispersed from pycnidia
#' @param paddock_source A data.table of coordinates which contains sporulating
#'   growing points and the one element from which conidia dispersal originates.
#' @param spores_per_gp_per_wet_hour The 'spore rate' or conidial production
#'   rate per growing point during each wet hour
#' @param max_interception_probability Doubles with length of one; Estimated
#'   using the `spore_interception_parameter`, see function
#'   `interception_probability()`
#' @param wind_direction_in_hour Wind_direction
#' @param average_wind_speed_in_hour Avg wind speed
#' @param stdev_wind_direction_in_hour Std wind dir
#' @param spore_aggregation_limit When spores/summary unit (n) is <= this value
#'   n spores are produced as individuals. When greater they are produced in
#'   sporeAggregationLimit groups of sporeAggregationLimit spores. Defaults to
#'   `1000`.
#' @param splash_cauchy_parameter A parameter used in the Cauchy distribution
#'  and describes the median distance spores travel due to rain splashes.
#'  Defaults to `0.5`.
#' @param wind_cauchy_multiplier A scaling parameter to estimate a Cauchy
#'  distribution which resembles the possible distances a conidium travels due
#'  to wind dispersal. Defaults to `0.015`.
#' @param paddock A data.table of x and y coordinates; provides the dimensions
#'  of the paddock so function only returns `target_coordinates` in the paddock
#'  area.
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
           splash_cauchy_parameter = 0.5,
           wind_cauchy_multiplier = 0.015,
           paddock) {
    x <- y <- NULL

    # this might be able to be calculated at the spread_spores level, and `If`
    # statement should come first given that it is if == 0
    spore_packets <-
      potentially_effective_spores(
        spores_per_gp_per_wet_hour = spores_per_gp_per_wet_hour,
        max_interception_probability = max_interception_probability,
        paddock_source["infectious_gp"]
      )

    degree <- 0.01745

    if (spore_packets > spore_aggregation_limit) {
      spores_per_packet <- spore_packets / spore_aggregation_limit
      spore_packets <- spore_aggregation_limit
    } else{
      spores_per_packet <- 1
    }

    if (spore_packets == 0) {
      return(NULL)
    }

    # this for loop needs improvement so it is not growing a data.table
    target_coordinates <-
      lapply(seq_len(spore_packets), function(x) {
        wind_d <- wind_distance(average_wind_speed_in_hour,
                                wind_cauchy_multiplier)
        wind_a <-
          wind_angle(wind_direction_in_hour, stdev_wind_direction_in_hour)
        splash_d <- splash_distance(splash_cauchy_parameter)
        splash_a <- splash_angle()

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
      })


    new_infections <- data.table::rbindlist(target_coordinates)
    new_infections$spores_per_packet <- spores_per_packet

    new_infections <- new_infections[x >= min(paddock[, x]) &
                                       x <= max(paddock[, x]) &
                                       y >= min(paddock[, y]) &
                                       y <= max(paddock[, y]) ,]

    return(new_infections)
  }
