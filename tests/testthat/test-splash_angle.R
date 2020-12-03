context("Splashed dispersed conidia returns an rnorm splash angle")

sa1 <- splash_angle()


test_that("sa1 length is equal to 1", {
  expect_equal(length(sa1), 1)
})

test_that("splash angle returns a number", {
  expect_is(sa1, "numeric")
})

test_that("splash angle is between 1 and 360", {
  expect_true(sa1 >= 1)
  expect_true(sa1 <= 360)
})

