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
  epidemic_start = "1998-03-10",
  sowing_date = as.POSIXct("1998-03-09"),
  harvest_date = as.POSIXct("1998-03-12"),
  time_zone = "Australia/Perth"
)

# comment out as testing with different output for now
# test_that("trace_asco returns an xy data.frame", {
#   expect_is(ta1, "data.frame")
#   expect_true(all(c("x", "y") %in% colnames(ta1)))
# })
#
# test_that("trace_asco returns the correct data.frame dimensions", {
#   expect_equal(nrow(ta1), 10000)
#   expect_equal(ncol(ta1), 2)
# })


test_that("trace_asco daily_vals returns an daily vals data.frame", {
  expect_is(ta1, "data.frame")
  expect_true(all(c("cdd", "cwh", "cr", "i", "day") %in% colnames(ta1)))
})

test_that("trace_asco daily_vals returns the correct data.frame dimensions", {
  expect_equal(nrow(ta1), 4)
  expect_equal(ncol(ta1), 5)
})

test_that("trace_asco daily_vals the following contents", {
  expect_equal(ta1$cdd, c(0,28,53,71))
  expect_equal(ta1$cwh, c(0,0,0,5))
  expect_equal(ta1$cr, c(0,0,0,5.33))
  expect_equal(ta1$i[1], as.POSIXct("1998-03-09", tz = "Australia/Perth"))
  expect_equal(ta1$day, c(68,69,70,71))
  expect_equal(ncol(ta1), 5)
})


# Test for stop error is triggered
test_that("trace_asco stops if epidemic_start is earlier than sowing_start",{
  expect_error(
  ta1 <- trace_asco(
    weather = weather_dat,
    paddock_length = 100,
    paddock_width = 100,
    epidemic_start = "1998-03-09",
    sowing_date = as.POSIXct("1998-03-09"),
    harvest_date = as.POSIXct("1998-03-12"),
    time_zone = "Australia/Perth"
  ))
})

