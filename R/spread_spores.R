spread_spores <-
  function(wet_hours,
           paddock,
           max_gp = 15000,
           max_new_gp = 350,
           spore_interception_multiplier = 6e-05,
           weather_hourly) {

    max_interception_probability <-
      interception_probability(
        target_density = 5 * max(paddock$new_gp),
        k = intercept_spores(
          spore_interception_multiplier = spore_interception_multiplier,
          max_growing_points_limit = max_gp,
          max_new_growing_points_limit = max_new_gp
        )
      )

    for(hour in 1:wet_hours){
      newlyInfectedList <- c(newlyInfectedList,
                             spores_each_wet_hour())
    }

  }
