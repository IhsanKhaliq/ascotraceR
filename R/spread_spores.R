spread_spores <-
  function(wet_hours,
           paddock,
           max_gp,
           max_new_gp,
           spore_interception_parameter,
           weather_hourly) {

    max_interception_probability <-
      interception_probability(target_density = 5 * max(paddock$new_gp),
                               k = spore_interception_parameter)
    # k = intercept_spores(
    #   spore_interception_multiplier = spore_interception_multiplier,
    #   max_growing_points_limit = max_gp,
    #   max_new_growing_points_limit = max_new_gp
    # ))

    # filter weather data to just wet hours
    wet_hour_weather <-
      weather_hourly[rain >= 0.2,]

    newly_infected_list <-
      lapply(
        seq_len(nrow(wet_hour_weather)),
        FUN = spores_each_wet_hour,
        weather_hourly = weather_day,
        paddock = paddock,
        max_interception_probability = max_interception_probability,
        spore_interception_parameter = spore_interception_parameter
      )


      newlyInfectedList <- c(
        newlyInfectedList,
        spores_each_wet_hour(
          h = hour,
          weather_hourly = weather_hourly,
          paddock = paddock,
          max_interception_probability = max_interception_probability
        )
      )
    }

  }
