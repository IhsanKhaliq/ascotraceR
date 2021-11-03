# First weather data needs to be imported and formatted with `format_weather`

Newmarracarra <-
   read.csv(system.file("extdata",
            "1998_Newmarracarra_weather_table.csv", package = "ascotraceR"))
station_data <-
   system.file("extdata", "stat_dat.csv", package = "ascotraceR")

weather_dat <- format_weather(
   x = Newmarracarra,
   POSIXct_time = "Local.Time",
   temp = "mean_daily_temp",
   ws = "ws",
   wd_sd = "wd_sd",
   rain = "rain_mm",
   wd = "wd",
   station = "Location",
   time_zone = "Australia/Perth",
   lonlat_file = station_data)

dat <- trace_asco(
  weather = weather_dat,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-06-10",
  sowing_date = as.POSIXct("1998-06-09"),
  harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(100),
  time_zone = "Australia/Perth",
  primary_infection_foci = "centre")

test1 <- tracer_plot(dat, 5, tiles = "infectious_gp")

test_that("expected output is returned", {
  expect_is(test1, "ggplot")
  expect_false(any(is.na(test1)))
  expect_error(
    scale_colour_continuous(
      type = function()
        "abc"
    ),
    "could not find function \"scale_colour_continuous\""
  )
  expect_error(
    scale_fill_continuous(type = geom_point),
    "could not find function \"scale_fill_continuous\""
  )
  expect_error(
    scale_fill_binned(type = scale_fill_brewer),
    "could not find function \"scale_fill_binned\""
  )

})

