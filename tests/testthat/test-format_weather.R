context(
   "`format_weather()` wrangles weather station data to be suitable for input'
   into `trace_asco`"
)

# identify lon lat from file ---------------------------------------------------
test_that("`format_weather()` is able to identify the correct lat and lon values
          from file",
          {
             set.seed(27)
             # create data.frame of station coordinates
             write.csv(data.frame(
                station = c("69061","16095"),
                lon = c(114.8627,114.2627),
                lat = c(-28.5990, -28.1)
             ),
             file = file.path(tempdir(), "stat_coord.csv"))

             dat_minutes <- 10080 # equal to, 7 * 24 * 60

             weather_station_data <- data.table(
                Local.Time.YYYY = rep(2020, dat_minutes),
                Local.Time.MM = rep(6, dat_minutes),
                Local.Time.DD = rep(10:16, each = 24 * 60),
                Local.Time.HH24 = rep(1:24, each = 60, times = 7),
                Local.Time.MI = rep(0:59, times = 7 * 24),
                Precipitation.since.last.observation.in.mm = round(abs(rnorm(
                   dat_minutes, mean = 0, sd = 0.2
                )), 1),
                Temperature.C = (sin(seq(0,(6.285)*7, length.out = dat_minutes)+4)+1) * 20,
                Wind.speed.in.km.h = abs(rnorm(
                   dat_minutes, mean = 5, sd = 10
                )),
                Wind.direction.in.degrees.true = runif(n = dat_minutes,
                                                       min = 0, max = 359),
                Station.Number = "69061"
             )

             weather_dt <- format_weather(
                x = weather_station_data,
                YYYY = "Local.Time.YYYY",
                MM = "Local.Time.MM",
                DD = "Local.Time.DD",
                hh = "Local.Time.HH24",
                mm = "Local.Time.MI",
                temp = "Temperature.C",
                rain = "Precipitation.since.last.observation.in.mm",
                ws = "Wind.speed.in.km.h",
                wd = "Wind.direction.in.degrees.true",
                station = "Station.Number",
                lonlat_file = file.path(tempdir(), "stat_coord.csv"),
                time_zone = "Australia/Perth"

             )

             expect_is(weather_dt, "asco.weather")
             expect_equal(
                names(weather_dt),
                c(
                   "times",
                   "temp",
                   "rain",
                   "ws",
                   "wd",
                   "wd_sd",
                   "lon",
                   "lat",
                   "station",
                   "YYYY",
                   "MM",
                   "DD",
                   "hh",
                   "mm"
                )
             )
             expect_equal(dim(weather_dt), c(dat_minutes/60, 14))
             expect_true(anyNA(weather_dt$lon) == FALSE)
             expect_true(anyNA(weather_dt$lat) == FALSE)
             expect_true(weather_dt[,unique(lon)] == 114.8627)
             expect_equal(unique(weather_dt$lat), -28.5990)
             expect_is(weather_dt$times, "POSIXct")
             expect_equal(as.character(min(weather_dt$times)),
                          "2020-06-10 01:00:00")
             expect_equal(as.character(max(weather_dt$times)), "2020-06-17")
             expect_equal(round(min(weather_dt$rain), 0), 7)
             expect_equal(round(max(weather_dt$rain), 1), 12.4)
             expect_equal(round(min(weather_dt$ws), 1), 6.5)
             expect_equal(round(max(weather_dt$ws), 2), 10.99)
             expect_equal(round(min(weather_dt$wd), 0), 1)
             expect_equal(round(max(weather_dt$wd), 1), 358.3)
             expect_equal(round(min(weather_dt$wd_sd), 0), 82)
             expect_equal(round(max(weather_dt$wd_sd), 0), 195)
             expect_equal(mean(weather_dt$YYYY), 2020)
             expect_equal(mean(weather_dt$MM), 6)
             expect_equal(min(weather_dt$DD), 10)
             expect_equal(max(weather_dt$DD), 16)
             expect_equal(min(weather_dt$hh), 1)
             expect_equal(max(weather_dt$hh), 24)
             expect_equal(mean(weather_dt$mm), 0)

             unlink(file.path(tempdir(), "stat_coord.csv"))
          })

# Test that it is able to import supplied data suitable for `ascotraceR` model

# read in data associated with the ascotraceR model
newmarra_raw <-
   fread(system.file("extdata", "1998_Newmarracarra_weather_table.csv", package = "ascotraceR"))

test_that("format_weather() can format Newmarracarra weather data",{
expect_silent(
   newM <- format_weather(
      x = newmarra_raw,
      POSIXct_time = "Local.Time",
      time_zone = "Australia/Perth",
      temp = "mean_daily_temp",
      rain = "rain_mm",
      ws = "ws",
      wd = "wd",
      wd_sd = "wd_sd",
      station = "Location",
      lat = NA,
      lon = NA
   )
)
})

dat_no_location <- format_weather(
   x = newmarra_raw,
   POSIXct_time = "Local.Time",
   time_zone = "Australia/Perth",
   temp = "mean_daily_temp",
   rain = "rain_mm",
   ws = "ws",
   wd = "wd",
   wd_sd = "wd_sd",
   station = "Location",
   lat = NA,
   lon = NA
)

test_that("dat_no_location contains the correct output",{

   expect_false("lat" %in% colnames(dat_no_location))
   expect_false("lon" %in% colnames(dat_no_location))
   expect_true(all(
      c(
         "times",
         "temp",
         "rain",
         "ws",
         "wd",
         "wd_sd",
         "station",
         "YYYY",
         "MM",
         "DD",
         "hh",
         "mm"
      ) %in% colnames(dat_no_location)
   ))
})







# when more than one station is supplied, lapply is used -----------------------

# test_that("`format_weather()` handles multiple stations", {
#    scaddan <-
#       system.file("extdata", "scaddan_weather.csv", package = "blackspot")
#    naddacs <-
#       system.file("extdata", "naddacs_weather.csv", package = "blackspot")
#
#    weather_file_list <- list(scaddan, naddacs)
#    weather_station_data <-
#       lapply(X = weather_file_list, FUN = read.csv)
#
#    weather_station_data <- do.call("rbind", weather_station_data)
#
#    weather_dat <- format_weather(
#       x = weather_station_data,
#       POSIXct_time = "Local.Time",
#       ws = "meanWindSpeeds",
#       wd_sd = "stdDevWindDirections",
#       rain = "Rainfall",
#       wd = "meanWindDirections",
#       lon = "Station.Longitude",
#       lat = "Station.Latitude",
#       station = "StationID",
#       time_zone = "Australia/Perth"
#    )
#
#    expect_is(weather_dat, "blackspot.weather")
#    expect_equal(
#       names(weather_dat),
#       c(
#          "times",
#          "rain",
#          "ws",
#          "wd",
#          "wd_sd",
#          "lon",
#          "lat",
#          "station",
#          "YYYY",
#          "MM",
#          "DD",
#          "hh",
#          "mm"
#       )
#    )
#    expect_equal(dim(weather_dat), c(8784, 13))
#    expect_true(anyNA(weather_dat$lon) == FALSE)
#    expect_true(anyNA(weather_dat$lat) == FALSE)
#    expect_equal(round(unique(weather_dat$lon), 1), c(135.7, 135.9))
#    expect_equal(round(unique(weather_dat$lat), 1), c(-33.1, -33.3))
# })

# identify lon lat from cols ---------------------------------------------------
test_that("`format_weather()` works when lat lon are in data", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Local.Time.MI = rep(0:59, times = 7 * 24),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   weather_dt <- format_weather(
      x = weather_station_data,
      YYYY = "Local.Time.YYYY",
      MM = "Local.Time.MM",
      DD = "Local.Time.DD",
      hh = "Local.Time.HH24",
      mm = "Local.Time.MI",
      rain = "Precipitation.since.last.observation.in.mm",
      ws = "Wind.speed.in.km.h",
      wd = "Wind.direction.in.degrees.true",
      station = "Station.Number",
      lon = "lon",
      lat = "lat",
      time_zone = "UTC"
   )


 expect_is(weather_dt, "asco.weather")
 expect_is(weather_dt, "data.table")

   expect_named(
      weather_dt,
      c(
         "times",
         "temp",
         "rain",
         "ws",
         "wd",
         "wd_sd",
         "lon",
         "lat",
         "station",
         "YYYY",
         "MM",
         "DD",
         "hh",
         "mm"
      )
   )
   expect_equal(dim(weather_dt), c(168, 14))
   expect_is(weather_dt$times, "POSIXct")
   expect_true(anyNA(weather_dt$times) == FALSE)
   expect_true(max(weather_dt$wd, na.rm = TRUE) < 360)
   expect_true(min(weather_dt$wd, na.rm = TRUE) > 0)
   expect_true(lubridate::tz(weather_dt$times) == "UTC")
})

# stop if `x` is not a data.frame object ---------------------------------------
test_that("`format_weather()` stops if `x` is not a data.frame object", {
   expect_error(
      weather_dt <- format_weather(
         x = list(),
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lon = "lon",
         lat = "lat",
         r = "eyre",
         POSIXct_time = "times"
      ),
      regexp = "`x` must be provided as a `data.frame` object*"
   )
})

# stop if time isn't given in any col ------------------------------------------
test_that("`format_weather()` stops if time cols are not provided", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Local.Time.MI = rep(0:59, times = 7 * 24),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   weather_dt <- format_weather(
      x = weather_station_data,
      YYYY = "Local.Time.YYYY",
      MM = "Local.Time.MM",
      DD = "Local.Time.DD",
      hh = "Local.Time.HH24",
      mm = "Local.Time.MI",
      rain = "Precipitation.since.last.observation.in.mm",
      ws = "Wind.speed.in.km.h",
      wd = "Wind.direction.in.degrees.true",
      station = "Station.Number",
      lon = "lon",
      lat = "lat",
      time_zone = "Australia/Adelaide"
   )

   expect_error(
      weather_dt <- format_weather(
         x = weather_station_data,
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lon = "lon",
         lat = "lat",
         r = "eyre"
      ),
      regexp = "You must provide time values either as a*"
   )
})

# # stop if lonlat file lacks proper field names ---------------------------------
test_that("`format_weather() stops if lonlat input lacks proper names", {
   # create a dummy .csv with misnamed cols
   write.csv(data.frame(
      stats = c("69061", "16096"),
      long = c(134.2734, 135.7243),
      lat = c(-33.52662, -33.26625)
   ),
   file = file.path(tempdir(), "stat_coord.csv"))

   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Local.Time.MI = rep(0:59, times = 7 * 24),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   # while ascotraceR does not use rasters lets create one for testing
   eyre_temp <- raster::raster(nrows=180, ncols=360,
                               xmn=-135.7, xmx=135.9, ymn=-33.4, ymx=-33.2)

   expect_error(
      weather_dt <- format_weather(
         x = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         mm = "Local.Time.MI",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lonlat_file = file.path(tempdir(), "stat_coord.csv"),
         r = eyre_temp
      ),
      regexp = "The csv file of weather station coordinates *"
   )
   unlink(file.path(tempdir(), "stat_coord.csv"))
})

# stop if no lonlat info provided ----------------------------------------------
test_that("`format_weather() stops if lonlat input lacks proper names", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Local.Time.MI = rep(0:59, times = 7 * 24),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   expect_error(
      weather_dt <- format_weather(
         x = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         mm = "Local.Time.MI",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         r = eyre
      ),
      regexp = "You must provide lonlat values for the weather *"
   )
   unlink(file.path(tempdir(), "stat_coord.csv"))
})


# fill missing mm --------------------------------------------------------------
test_that("`format_weather() creates a `mm` column if not provided", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   expect_named(
      weather_dt <- format_weather(
         x = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lat = "lat",
         lon = "lon",
         time_zone = "Australia/Adelaide"
      ),
      c(
         "times",
         "temp",
         "rain",
         "ws",
         "wd",
         "wd_sd",
         "lon",
         "lat",
         "station",
         "YYYY",
         "MM",
         "DD",
         "hh",
         "mm"
      )
   )
})

# fill create YYYY, MM, DD hhmm cols from POSIXct ------------------------------
test_that("`format_weather() creates a YYYY MM DD... cols", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      Ptime = seq(ISOdate(2000, 1, 1), by = "1 min", length.out = dat_minutes),
      lon = 135.7243,
      lat = -33.26625,
      time_zone = c("Australia/Adelaide", "Australia/Brisbane")
   )

   # while ascotraceR does not use rasters lets create one for testing
   eyre_temp <- raster::raster(nrows=180, ncols=360,
                               xmn=-135.7, xmx=135.9, ymn=-33.4, ymx=-33.2)

   expect_named(
      weather_dt <- format_weather(
         x = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lat = "lat",
         lon = "lon",
         POSIXct_time = "Ptime",
         r = eyre_temp
      ),
      c(
         "times",
         "temp",
         "rain",
         "ws",
         "wd",
         "wd_sd",
         "lon",
         "lat",
         "station",
         "YYYY",
         "MM",
         "DD",
         "hh",
         "mm"
      )
   )
})

# stop if `wd_sd` is missing or cannot be calculated ---------------------------
test_that("`format_weather() stops if `wd_sd` is not available", {
   weather_station_data <- data.table(
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         24, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(24, mean = 5, sd = 10)),
      Wind.direction.in.degrees.true =
         runif(n = 24, min = 0, max = 359),
      Station.Number = "16096",
      Ptime = seq(ISOdate(2000, 1, 1), by = "1 hour", length.out = 24),
      lon = 135.7243,
      lat = -33.26625
   )

   expect_error(
      weather_dt <- format_weather(
         x = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lat = "lat",
         lon = "lon",
         POSIXct_time = "Ptime",
         time_zone = "UTC"
      ),
      regexp = "`format_weather*"
   )
})

# stop if no raster, `r` or `time_zone` provided --------------------------------
test_that("`format_weather() stops if `time_zone` cannot be determined", {
   weather_station_data <- data.table(
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         24, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(24, mean = 5, sd = 10)),
      Wind.direction.in.degrees.true =
         runif(n = 24, min = 0, max = 359),
      Station.Number = "16096",
      Ptime = seq(ISOdate(2000, 1, 1), by = "1 hour", length.out = 24),
      lon = 135.7243,
      lat = -33.26625
   )

   expect_error(
      weather_dt <- format_weather(
         x = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lat = "lat",
         lon = "lon",
         POSIXct_time = "Ptime"
      ),
      regexp =  "Please ensure that either a raster object for the area of * "
   )
})


test_that("format_weather detects impossible times", {
   raw_weather <- data.table(
      Year = rep(2020, 14 * 24 * 60),
      Month = rep(6, 14 * 24 * 60),
      Day = rep(rep(1:7, each = 24 * 60), 2),
      Hour = rep(rep(0:23, each = 60), 14),
      Minute = rep(1:60, 14 * 24),
      WindSpeed = abs(rnorm(14 * 24 * 60, 1, 3)),
      WindDirectionDegrees = round(runif(14 * 24 * 60, 0, 359)),
      Rainfall = floor(abs(rnorm(14 * 24 * 60, 0, 1))),
      stationID = rep(c("12345", "54321"), each = 7 * 24 * 60),
      StationLongitude = rep(c(134.123, 136.312), each = 7 * 24 * 60),
      StationLatitude = rep(c(-32.321,-33.123), each = 7 * 24 * 60)
   )



   expect_warning(
      expect_error(
         format_weather(
            x = raw_weather,
            YYYY = "Year",
            MM = "Month",
            DD = "Day",
            hh = "Hour",
            mm = "Minute",
            ws = "WindSpeed",
            rain = "Rainfall",
            wd = "WindDirectionDegrees",
            lon = "StationLongitude",
            lat = "StationLatitude",
            station = "stationID",
            time_zone = "UTC"
         ),
         regexp = "Time records contain NA values or impossible time*"
      )
   )

}
)
