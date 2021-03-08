context("daily rain")

test1 <- c(wet_hours = 5, data_line = 3)
test_that("correct values are returned", {
  expect_length(test1, 2)
  expect_is((test1), "numeric")
})
