context("calculates the number of new growing points from an existing number of growing points")

# Test function if current growing points is zero
test1 <- calc_new_gp(
  current_growing_points = 0,
  gp_rr = 0.0065,
  max_gp = 15000,
  mean_air_temp = 24
)

# Test function if current growing points is a negative
test2 <- calc_new_gp(
  current_growing_points = -1,
  gp_rr = 0.0065,
  max_gp = 15000,
  mean_air_temp = 24
)

# Test function if current growing points is 40, the model default
test3 <- calc_new_gp(
  current_growing_points = 40,
  gp_rr = 0.0065,
  max_gp = 15000,
  mean_air_temp = 24
)

test_that("calc_new_gp returns a number",{
  expect_equal(test1, 0)
  expect_equal(round(test2,6), -0.15601)
  expect_equal(round(test3,6), 6.22336)
})
