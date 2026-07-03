# `wind_distance()` draws from a half-Cauchy distribution scaled by
# `wind_cauchy_multiplier * average_wind_speed_in_fifteen_minutes`. As with
# `splash_distance()`, the median of a half-Cauchy distribution is exactly
# equal to its scale parameter, which gives an analytic reference value to
# test the wind-speed scaling against instead of a hard-coded RNG draw
# compared with an overly loose tolerance.

test_that("wind_distance returns a numeric vector of length 1 by default", {
  set.seed(25)
  w_dist <- wind_distance(5)
  expect_type(w_dist, "double")
  expect_length(w_dist, 1)
  expect_true(w_dist >= 0)
})

test_that("wind_distance can return a vector of 10 numbers", {
  set.seed(25)
  w_dist10 <- wind_distance(average_wind_speed_in_fifteen_minutes = 5,
                            PSPH = 10)
  expect_type(w_dist10, "double")
  expect_length(w_dist10, 10)
  expect_true(all(w_dist10 >= 0))
})

test_that("wind_distance recycles across a vector of PSPH groups", {
  set.seed(25)
  w_dist_grouped <- wind_distance(average_wind_speed_in_fifteen_minutes = 15,
                                  PSPH = c(5, 5))
  expect_length(w_dist_grouped, 10)
})

test_that("wind speed scales the dispersal distance's median as expected", {
  set.seed(2026)
  slow_wind <- wind_distance(average_wind_speed_in_fifteen_minutes = 5,
                             PSPH = 1e4)
  # scale = wind_cauchy_multiplier (0.015) * wind speed (5) = 0.075
  expect_equal(median(slow_wind), 0.015 * 5, tolerance = 0.02)

  fast_wind <- wind_distance(average_wind_speed_in_fifteen_minutes = 50,
                             PSPH = 1e4)
  # scale = 0.015 * 50 = 0.75
  expect_equal(median(fast_wind), 0.015 * 50, tolerance = 0.08)
})
