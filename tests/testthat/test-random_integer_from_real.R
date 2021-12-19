r <- 0.8


test_that("function returns correct values and class type", {
  expect_equal(r %% 1, 0.8)
  expect_equal(ceiling(r), 1)
  expect_length(r, 1)
  expect_type(floor(r), "double")
  expect_type(ceiling(r), "double")
  expect_true(r < 1)
  expect_is(r, "numeric")
})
