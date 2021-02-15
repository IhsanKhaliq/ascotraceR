context("Simulate disease progression for a single day increment")

# initialise daily_vals
daily_vals_dt <- data.table::data.table(
  cdd = 0, # cumulative degree days
  cwh = 0, # cumulative wet hours
  cr = 0,  # cumulative rainfall
  i = as.POSIXct("1998-05-12", tz = "Australia/Perth"),   # day of the simulation (iterator)
  day = 0  # day of the year
)

od_t1_i_date <-as.POSIXct("1998-05-12", tz = "Australia/Perth")
od_t1 <- one_day(i_date = od_t1_i_date,
                 daily_vals = daily_vals_dt,
                 weather_dat = newM_weather)

test_that("one_day returns a data.table", {
  expect_is(od_t1, "data.table")
})

test_that("one_day daily_vals returns an daily vals data.frame", {
  expect_is(ta1, "data.frame")
  expect_true(all(c("cdd", "cwh", "cr", "i", "day") %in% colnames(od_t1)))
})

test_that("trace_asco daily_vals returns the correct data.frame dimensions", {
  expect_equal(nrow(od_t1), 2)
  expect_equal(ncol(od_t1), 5)
})

test_that("trace_asco daily_vals the following contents", {
  expect_equal(od_t1$cdd, c(0,18))
  expect_equal(od_t1$cwh, c(0,15))
  expect_equal(od_t1$cr, c(0,78.85))
  expect_equal(od_t1$i[2], od_t1_i_date)
  expect_equal(od_t1$day, c(0, 132))
})
