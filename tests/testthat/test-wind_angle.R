# `wind_angle()` draws bearing(s) from Normal(mean_direction, sd_direction)
# and then corrects any value that falls outside [0, 360) back into range.
# Setting `stdev_wind_direction_fifteen_minutes = 0` makes `rnorm()`
# deterministic (it always returns the mean), which lets us test the
# wrap-around correction logic exactly, with no randomness involved.

test_that("wind_angle wraps a negative angle back into [0, 360)", {
  expect_equal(wind_angle(-10, 0), 350)
  expect_equal(wind_angle(-0.5, 0), 359.5)
})

test_that("wind_angle wraps an angle >= 360 back into [0, 360)", {
  expect_equal(wind_angle(370, 0), 10)
  expect_equal(wind_angle(360, 0), 0)
})

test_that("wind_angle leaves an in-range angle unchanged", {
  expect_equal(wind_angle(180, 0), 180)
  expect_equal(wind_angle(0, 0), 0)
})

test_that("wind_angle returns a number between 0 and 360", {
  set.seed(99)
  w_a <- wind_angle(1, 30, 1)
  w_a10 <- wind_angle(1, 30, 10)

  expect_true(all(w_a >= 0 & w_a < 360))
  expect_true(all(w_a10 >= 0 & w_a10 < 360))
})

test_that("wind_angle returns the correct number of elements", {
  set.seed(99)
  expect_length(wind_angle(1, 30, 1), 1)
  expect_length(wind_angle(1, 30, 10), 10)
})

test_that("wind_angle returns the correct class", {
  set.seed(99)
  w_a <- wind_angle(1, 30, 1)
  expect_type(w_a, "double")
  expect_true(is.vector(w_a))
})

test_that("wind_angle is centred on the supplied mean direction", {
  set.seed(99)
  angles <- wind_angle(average_wind_direction_in_fifteen_minutes = 180,
                       stdev_wind_direction_fifteen_minutes = 10,
                       PSPH = 1e4)
  expect_true(all(angles >= 0 & angles < 360))
  # with a mean of 180 and sd of 10, values are far enough from the 0/360
  # wrap boundary that the sample mean should still closely match the mean
  expect_equal(mean(angles), 180, tolerance = 1)
})
