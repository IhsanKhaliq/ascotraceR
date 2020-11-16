context("Returns an rnorm wind angle from a mean windangle and sd(wind angle)")

set.seed(25)

w_dist10 <-wind_distance(average_wind_speed_in_fifteen_minutes = 5,
                         PSPH = 10)

test_that("wind_distance returns a number", {
  w_dist <- wind_distance(5)
  expect_is(w_dist, "numeric")
  expect_equal(w_dist, 1.853252, tolerance = 0.000001)
  expect_length(w_dist, 1)

})

test_that("wind_distance can return multiple numbers", {
  w_dist10 <-wind_distance(average_wind_speed_in_fifteen_minutes = 5,
                           PSPH = 10)
  expect_is(w_dist10, "numeric")
  expect_length(w_dist10, 10)

})
