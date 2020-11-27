context("The angle of splashed dispersed conidia")

sa1 <- splash_angle()

test_that("splash angle returns a number", {
  expect_is(sa1, "numeric")
})

test_that("splash angle is between 1 and 360", {
  expect_true(sa1 >= 1)
  expect_true(sa1 <= 360)
})
