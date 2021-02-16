
# load formatted weather data `newM_weather`
load("R/sysdata.rda")

# randomly choose a day to get weather
day <- sample(unique(newM_weather[newM_weather$wet_hours > 0,times]), size =1)

# makePaddock equivalent
paddock <- as.data.table(expand.grid(x = 1:paddock_width,
                                     y = 1:paddock_length))

primary_infection_foci <-
    paddock[as.integer(round(paddock_width / 2)),
            as.integer(round(paddock_length / 2))]

# define paddock variables at time 1
paddock[, new_gp := seeding_rate] # Change in the number of growing points since last iteration
paddock[, noninfected_gp := seeding_rate] #
paddock[, infected_gp := fifelse(x == primary_infection_foci[1] &
                                   y == primary_infection_foci[2], 1,
                                 0)] # Initialise column of infected growing points

w_dat <- newM_weather[times %in% seq(as.POSIXct("1998-09-02 00:00:00", tz = "Australia/Perth"),
                                     as.POSIXct("1998-09-02 23:00:00", tz = "Australia/Perth"),
                                     by ="hours"),]


paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))

paddock[,new_gp := 40]
paddock[,noninfected_gp := 40]
paddock[,infected_gp := NA] # Needs to be updated!!!!


spores_each_wet_hour(
  h = 1,
  weather_hourly = w_dat,
  paddock = paddock,
  max_interception_probability = 1
)
