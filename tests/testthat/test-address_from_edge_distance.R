context("address from edge distance")


test_1 <- c(1 + floor(1 / 1), (1 + floor(1 / 1)))


test_that("multiplication works", {
  expect_length(test_1, 2)
  expect_true(any(is.na(test_1)) == FALSE)

})
