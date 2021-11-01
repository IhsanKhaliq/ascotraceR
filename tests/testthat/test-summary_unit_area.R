test_1 <- (1 * 1)

test_that("correct values are returned when summary unit is an observation
          quadrat", {
  expect_equal(test_1, 1)
  expect_length(test_1, 1)
  expect_type(test_1, "double")
  expect_is(as.integer(test_1), "integer")
  expect_equal(1*1, 1)
})
