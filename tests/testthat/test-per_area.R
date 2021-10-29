
test_1 <- list(1* 20*20)


test_that("correct parameter values are returned", {
  expect_equivalent(test_1, 400)
  expect_length(test_1, 1)
  expect_is(as.integer(test_1), "integer")
})
