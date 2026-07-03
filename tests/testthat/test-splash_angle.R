# `splash_angle()` draws a single bearing from Uniform(1, 360).

test_that("splash_angle returns a single numeric value in [1, 360]", {
  set.seed(42)
  sa1 <- splash_angle()
  expect_type(sa1, "double")
  expect_length(sa1, 1)
  expect_true(sa1 >= 1 && sa1 <= 360)
})

test_that("splash_angle samples uniformly across its full range", {
  set.seed(42)
  angles <- replicate(1e4, splash_angle())
  expect_true(all(angles >= 1 & angles <= 360))
  # the mean of Uniform(1, 360) should be close to the midpoint, 180.5
  expect_equal(mean(angles), 180.5, tolerance = 5)
})
