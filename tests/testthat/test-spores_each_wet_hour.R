context("spores spread each day triggered by each wet hour")

# load formatted weather data `newM_weather`
day <- fread("tests/testthat/formatted_weather_one_day.csv")
#day <- fread("formatted_weather_one_day.csv")

# makePaddock equivalent
paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))

primary_infection_foci <-
    c(50,50)

# define paddock variables at time 1
paddock[, new_gp := 40] # Change in the number of growing points since last iteration
paddock[, noninfected_gp := 40] #
paddock[, infected_gp := fifelse(x == primary_infection_foci[1] &
                                   y == primary_infection_foci[2], 1,
                                 0)] # Initialise column of infected growing points

spore_interception_parameter <-
  0.00006 * (15000 / 350)


spores_each_wet_hour(
  h = 1,
  weather_hourly = day,
  paddock = paddock,
  max_interception_probability = 1,
  spore_interception_parameter = spore_interception_parameter
)
