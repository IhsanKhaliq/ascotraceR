
# this doesn't test any functionality from this package that I can tell? -as
# it needs to be rewritten to actually test the paddock_dimension(), this just
# tests `ceiling()`

test_1 <-  ceiling(200 / 80)
ceiling(200 / 80)

test_that("correct parameters values are returned", {
  expect_equal(test_1, 3)
  expect_length(test_1, 1)
  expect_type(test_1, "double")
  expect_is(as.integer(test_1), "integer")
})
