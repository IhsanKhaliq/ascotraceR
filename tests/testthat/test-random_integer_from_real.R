# `random_integer_from_real()` stochastically rounds a real number up or
# down so that, over many draws, the long-run proportion of "round up"
# outcomes approximates the fractional part of the input (e.g. 5.9 should
# round up to 6 approximately 90% of the time, and down to 5 approximately
# 10% of the time). These tests exercise both the deterministic branch
# (exact integers, where no rounding decision is needed) and the stochastic
# branches, using a fixed seed and a large number of draws so the empirical
# proportion reliably converges to the true fraction without being flaky.

test_that("an exact integer value is returned unchanged, as an integer", {
  expect_equal(random_integer_from_real(5), 5L)
  expect_equal(random_integer_from_real(0), 0L)
  expect_equal(random_integer_from_real(-3), -3L)
  expect_type(random_integer_from_real(5), "integer")
  expect_length(random_integer_from_real(5), 1)
})

test_that("a fractional value always rounds to floor() or ceiling()", {
  set.seed(1)
  draws <- vapply(rep(5.5, 1000), random_integer_from_real, integer(1))
  expect_type(draws, "integer")
  expect_true(all(draws %in% c(5L, 6L)))
  expect_length(random_integer_from_real(5.5), 1)
})

test_that("rounding direction is biased by the size of the fraction", {
  set.seed(20260703)

  # fraction = 0.9 -> ~90% of draws should round UP to 6
  high_fraction <- vapply(rep(5.9, 1e4), random_integer_from_real, integer(1))
  expect_equal(mean(high_fraction == 6L), 0.9, tolerance = 0.02)

  # fraction = 0.1 -> ~10% of draws should round UP to 6
  low_fraction <- vapply(rep(5.1, 1e4), random_integer_from_real, integer(1))
  expect_equal(mean(low_fraction == 6L), 0.1, tolerance = 0.02)

  # fraction = 0.5 -> ~50/50 split between 5 and 6
  mid_fraction <- vapply(rep(5.5, 1e4), random_integer_from_real, integer(1))
  expect_equal(mean(mid_fraction == 6L), 0.5, tolerance = 0.03)
})

test_that("rounding also works correctly for negative real numbers", {
  set.seed(20260703)

  # -5.9 %% 1 == 0.1 in R, so this should behave like the 0.1 fraction case
  # above but shifted to the -6/-5 boundary
  draws <- vapply(rep(-5.9, 1e4), random_integer_from_real, integer(1))
  expect_true(all(draws %in% c(-6L, -5L)))
  expect_equal(mean(draws == -5L), 0.1, tolerance = 0.02)
})
