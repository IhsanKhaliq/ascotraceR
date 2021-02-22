context("per_area")

test_1 <- 1*(1* 20*20)


test_that("correct parameter values are returned", {
  expect_equal(test_1, 400)
  expect_length(test_1, 1)
  expect_type(test_1, "double")
  expect_is(as.integer(test_1), "integer")
})
