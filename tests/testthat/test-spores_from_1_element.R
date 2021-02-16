context("spores produced from a spatial unit with at least one infected growing point")

library(data.table)

# import weather and filter to a single day with rain
# load formatted weather data `newM_weather`
load("R/sysdata.rda")

#filter to day
w_dat <- newM_weather[times == as.POSIXct("1998-09-02 02:00:00", tz = "Australia/Perth"),]


# makePaddock equivalent
paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))

primary_infection_foci <-
  paddock[x == 50 &
            y == 50,
          c("x", "y")]

# define paddock variables at time 1
paddock[, new_gp := 40] # Change in the number of growing points since last iteration
paddock[, noninfected_gp := 40] #
paddock[, infected_gp := fifelse(x == primary_infection_foci[1,x] &
                                   y == primary_infection_foci[1,y], 1,
                                 0)] # Initialise column of infected growing points

paddock_infected <- paddock[infected_gp > 0,]

apply(paddock_infected, 1, spores_from_1_element,
                      sporesPerInfectiveGPPerWetHour = 0.15, # default parameter for the function
                      max_interception_probability = 1, # parameter in potentially_effective_spores defined in spread_spores
                      wind_direction_in_hour = w_dat[1,wd],
                      average_wind_speed_in_hour = w_dat[1,ws],
                      stdev_wind_direction_in_hour = w_dat[1,wd_sd],
                      spore_aggregation_limit = 1000,
                      rain_cauchy_parameter = 0.5
                      )
