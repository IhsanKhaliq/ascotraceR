# this function has been made redunant as it just passes everything betweeen different levels
# and does not do anything really important
spread_spores <-
  function(wet_hours,
           paddock,
           max_gp,
           max_new_gp,
           spore_interception_parameter,
           weather_hourly) {


    # k = intercept_spores(
    #   spore_interception_parameter = spore_interception_parameter,
    #   max_growing_points_limit = max_gp,
    #   max_new_growing_points_limit = max_new_gp
    # ))

    # filter weather data to just wet hours
    wet_hour_weather <-
      weather_hourly[rain >= 0.2,]



    }

