# `splash_distance()` draws from a half-Cauchy distribution (the absolute
# value of a Cauchy(0, scale) draw, since the function wraps `rcauchy()` in
# `abs()`). A useful analytic property of the half-Cauchy distribution is
# that its *median* is exactly equal to its scale parameter
# (P(|X| <= scale) == 0.5 for X ~ Cauchy(0, scale)). That gives us a
# reference value to test the dispersal scaling against without needing to
# hard-code specific RNG draws, which would be fragile across R/OS versions.

test_that("splash_distance returns a numeric vector of length 1 by default", {
  set.seed(25)
  s_dist <- splash_distance(5)
  expect_type(s_dist, "double")
  expect_length(s_dist, 1)
  expect_true(s_dist >= 0)
})

test_that("splash_distance can return a vector of 10 numbers", {
  set.seed(25)
  s_dist10 <- splash_distance(PSPH = 10)
  expect_type(s_dist10, "double")
  expect_length(s_dist10, 10)
  expect_true(all(s_dist10 >= 0))
})

test_that("splash_distance recycles across a vector of PSPH groups", {
  set.seed(25)
  s_dist_grouped <- splash_distance(PSPH = c(5, 5))
  expect_type(s_dist_grouped, "double")
  expect_length(s_dist_grouped, 10)
  expect_true(all(s_dist_grouped >= 0))
})

test_that("splash_distance is always non-negative, even across many draws", {
  set.seed(25)
  many <- splash_distance(splash_cauchy_parameter = 0.015, PSPH = 1e4)
  expect_true(all(many >= 0))
})

test_that("the cauchy scale parameter controls the dispersal distance's median", {
  set.seed(2026)
  default_scale <- splash_distance(splash_cauchy_parameter = 0.015, PSPH = 1e4)
  expect_equal(median(default_scale), 0.015, tolerance = 0.005)

  wider_scale <- splash_distance(splash_cauchy_parameter = 1, PSPH = 1e4)
  expect_equal(median(wider_scale), 1, tolerance = 0.08)
})
