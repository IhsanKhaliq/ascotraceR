
set.seed(25)

test_that("splash_distance returns a numeric vector of length 1", {
  s_dist <- splash_distance(5)
  expect_is(s_dist, "numeric")
  expect_length(s_dist, 1)

})

test_that("splash_distance can return a vector of 10 numbers", {
  s_dist10 <- splash_distance(PSPH = 10)
  expect_is(s_dist10, "numeric")
  expect_length(s_dist10, 10)

})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
