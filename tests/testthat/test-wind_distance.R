context("Returns an rcachy of wind distance from a mean wind speed")

set.seed(25)

test_that("wind_distance returns a numberic vector of length 1", {
  w_dist <- wind_distance(5)
  expect_is(w_dist, "numeric")
  expect_equal(w_dist, 1.583252, tolerance = 1.31)
  expect_length(w_dist, 1)

})

test_that("wind_distance can return a vector of 10 numbers", {
  w_dist10 <-wind_distance(average_wind_speed_in_fifteen_minutes = 5,
                           PSPH = 10)
  expect_is(w_dist10, "numeric")
  expect_length(w_dist10, 10)

})
