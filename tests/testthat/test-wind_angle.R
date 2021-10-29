
w_a <- wind_angle(1, 30, 1)
w_a10 <- wind_angle(1, 30, 10)

test_that("wind_angle returns a number between 0 and 360", {
  # test that function returns a number below 360
  expect_true(w_a < 360)

  # test that function returns a number above 0
  expect_true(w_a >= 0)
})


test_that("wind_angle returns the correct number of elements", {
  # test that the length of w_a == 1
  expect_equal(length(w_a), 1)
  # test that the length of w_a10 == 10
  expect_equal(length(w_a10), 10)
})


test_that("wind_angle returns the correct class", {
  # test the class of output
  expect_is(w_a, "numeric")
  expect_true(is.vector(w_a))
})
