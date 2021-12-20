set.seed(123)

# Test function if current growing points is zero
test1 <- calc_new_gp(
  current_growing_points = rep(0, 10000),
  gp_rr = 0.0065,
  max_gp = 15000,
  mean_air_temp = 24
)

# Test function if current growing points is a negative
test_that("calc_new_gp errors with inputs lower than 0",{
  expect_error(
    calc_new_gp(
      current_growing_points = -1,
      gp_rr = 0.0065,
      max_gp = 15000,
      mean_air_temp = 24),
    label = "'current_growing_points' (value = -1) can't be < 0")
})


# Test function if current growing points is 40, the model default
test3 <- calc_new_gp(
  current_growing_points = 40,
  gp_rr = 0.0065,
  max_gp = 15000,
  mean_air_temp = 24
)

test_that("calc_new_gp returns a number", {
  expect_equal(unique(test1), 0)
  expect_equal(round(test3, 6), 6)
})
