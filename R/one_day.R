#' Simulates Ascochyta spore dispersal for a single day increment
#'
#' @param i_date a character string or \class{Date} formatted string indicating an
#'  iteration date of the model. Preferably in \acronym{ISO8601} format (YYYY-MM-DD),
#'  \emph{e.g.} \dQuote{2020-04-26}.
#'
#' @param daily_vals `list` of model variables which have been calculated for days prior to the `i_date`
#' @param weather_dat `data.table` of weather observations which includes the query date `i_date`
#'
#' @return a `data.table` of values generated for the day `i_date`
#'
#'
#' @examples
one_day <- function(i_date,
                    day,
                    daily_vals,
                    weather_dat,
                    gp_rr,
                    max_gp,
                    max_new_gp,
                    spore_interception_parameter) {

  # expand time to be hourly
  i_time <- rep(i_date, 24) + lubridate::dhours(0:23)

  # subset weather data by day
  weather_day <-
    weather_dat[times %in% i_time, ]

  # obtain summary weather for i_day
  i_mean_air_temp <- mean(weather_day[, temp])
  i_wet_hours <- weather_day[1, wet_hours]
  i_rainfall <- sum(weather_day[, rain], na.rm = TRUE)

  # Start building a list of values for 'i'
  # NOTE: I may add this to after `make_some_infective`
  daily_vals[["cdd"]] <- daily_vals[["cdd"]] + i_mean_air_temp
  daily_vals[["cwh"]] <- daily_vals[["cwh"]] + i_wet_hours
  daily_vals[["cr"]] <- daily_vals[["cr"]] + i_rainfall


  max_interception_probability <-
    interception_probability(target_density = 5 * max(daily_vals[["paddock"]][,new_gp]),
                             k = spore_interception_parameter)


# Spread spores and infect plants
  # Update growing points for paddock coordinates
  if(i_wet_hours > 2){

    newly_infected_dt <-
      rbindlist(
        lapply(
          seq_len(nrow(weather_day[rain >= 0.1, ])),
          FUN = spores_each_wet_hour,
          weather_hourly = weather_day[rain >= 0.2, ],
          paddock = daily_vals[["paddock"]],
          max_interception_probability = max_interception_probability,
          spore_interception_parameter = spore_interception_parameter
        )
      )
    newly_infected_dt[, cdd_at_infection := daily_vals[["cdd"]]]

    # #aggregate infections
    # newly_infected_dt <- newly_infected_dt[ , .N, by = .(x,y, cdd_at_infection)]
    # setnames(
    #   newly_infected_list,
    #   old = c("x", "y", "cdd_at_infection", "N"),
    #   new = c("x", "y", "cdd_at_infection", "spores_per_packet")
    # )

    daily_vals[["newly_infected"]] <- rbind(daily_vals[["newly_infected"]],
                                            newly_infected_dt)

    daily_vals <- make_some_infective(daily_vals = daily_vals,
                                      latent_period = 200)
  }




# Grow Plants
  # this code represents mathematica function `growth`; `updateRefUninfectiveGrowingPoints`

  # `updateGrowingPointsAllInfectiveElements`
  # Update Growing points for non-infected coords for time i
  daily_vals[["new_gp"]] <-
    calc_new_gp(current_growing_points = daily_vals[["gp_standard"]],
                gp_rr = gp_rr,
                max_gp = max_gp,
                mean_air_temp = i_mean_air_temp)

  daily_vals[["gp_standard"]] <-
    daily_vals[["gp_standard"]] + daily_vals[["new_gp"]]

  # this might be quicker if there was no fifelse statement
  daily_values[["paddock"]][, new_gp := fifelse(noninfected_gp == 0, 0,
                                                calc_new_gp(current_growing_points = noninfected_gp,
                                                            gp_rr = gp_rr,
                                                            max_gp = max_gp,
                                                            mean_air_temp = i_mean_air_temp)
                                                            )]

  daily_values[["paddock"]][, noninfected_gp := noninfected_gp + new_gp]






  # Write code to iterate over each hour and the function `growth`
  # `growth` function should return a vector of length 24 rows for each hour.
  # each value should give the number of growing points at that hour in time
  crop_gps <-


  return(daily_vals)
}
