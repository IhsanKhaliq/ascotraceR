context("Simulate disease progression for a single day increment")

# initialise daily_vals
daily_vals_dt <- data.table::data.table(
  cdd = 0, # cumulative degree days
  cwh = 0, # cumulative wet hours
  cr = 0,  # cumulative rainfall
  i = as.POSIXct("1998-05-12", tz = "Australia/Perth"),   # day of the simulation (iterator)
  day = 0  # day of the year
)


od_t1 <- one_day(i_date = as.POSIXct("1998-05-12", tz = "Australia/Perth"),
                 daily_vals = daily_vals_dt,
                 weather_dat = newM_weather)

test_that("one_day returns a data.table", {
  expect_is(od_t1, "data.table")
})
