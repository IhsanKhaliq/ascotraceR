context("trace_asco is the main function that runs the ascochyta model")

# read in example raw data
newmarra_raw <- read.csv(file =
                          system.file("extdata", "1998_Newmarracarra_weather_table.csv", package = "Ascotracer"))

weather_dat <- format_weather(
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

test_that("trace_asco runs without error" , {
  expect_silent(

    ta1 <- trace_asco(
      weather = weather_dat,
      paddock_length = 100,
      paddock_width = 100,
      epidemic_start = "1998-03-10",
      sowing_date = as.POSIXct("1998-03-09"),
      harvest_date = as.POSIXct("1998-03-12"),
      time_zone = "Australia/Perth"
    )

  )
})


# rerun the first test to make output available as an object to
#  test in subsequent tests
ta1 <- trace_asco(
  weather = weather_dat,
  paddock_length = 100,
  paddock_width = 100,
  sowing_date = "1998-03-09",
  sowing_date = "1998-03-10"
  )


test_that("trace_asco returns an xy data.frame", {
  expect_is(ta1, "data.frame")
  expect_true(all(c("x", "y") %in% colnames(ta1)))
})

test_that("trace_asco returns the correct data.frame dimensions", {
  expect_equal(nrow(ta1), 10000)
  expect_equal(ncol(ta1), 2)
})

# Test for stop error is triggered
test_that("trace_asco stops if epidemic_start is earlier than sowing_start",{
  ta2 <- trace_asco(
    weather = weather_dat,
    paddock_length = 100,
    paddock_width = 100,
    sowing_date = "1998-03-09",
    harvest_date = "1998-03-09"
  )
})

