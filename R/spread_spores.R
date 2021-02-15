spread_spores <-
  function(wet_hours,
           paddock,
           max_gp,
           max_new_gp,
           spore_interception_parameter,
           weather_hourly) {

    max_interception_probability <-
      interception_probability(
        target_density = 5 * max(paddock$new_gp),
        k = spore_interception_parameter
        # k = intercept_spores(
        #   spore_interception_multiplier = spore_interception_multiplier,
        #   max_growing_points_limit = max_gp,
        #   max_new_growing_points_limit = max_new_gp
        # )
      )

    for(hour in which(weather_hourly[["rain"]] >= 0.2)){
      newlyInfectedList <- c(newlyInfectedList,
                             spores_each_wet_hour(h = hour,
                                                  weather_hourly = weather_hourly,
                                                  max_interception_probability = max_interception_probability))
    }

  }
