
# load formatted weather data `newM_weather`
load("R/sysdata.rda")

# randomly choose a day to get weather
day <- sample(unique(newM_weather[newM_weather$wet_hours > 0,times]), size =1)

# I have no idea why this is not working
# newM_weather[times == day,]
# which(newM_weather$times == day)

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
