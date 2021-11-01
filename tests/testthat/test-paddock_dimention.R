test_1 <-  ceiling(20 / 1)
ceiling(20 / 1)

test_that("correct parameters values and paddock dimensions are returned", {
  expect_equal(test_1, 20)
  expect_length(test_1, 1)
  expect_type(test_1, "double")
  expect_is(as.integer(test_1), "integer")
  expect_true(any(is.na(test_1)) == FALSE)
})

