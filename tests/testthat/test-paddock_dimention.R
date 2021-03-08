context("paddock_dimention")

test_1 <-  ceiling(200 / 80)
ceiling(200 / 80)

test_that("correct parameters values are returned", {
  expect_equal(test_1, 3)
  expect_length(test_1, 1)
  expect_type(test_1, "double")
  expect_is(as.integer(test_1), "integer")
})
