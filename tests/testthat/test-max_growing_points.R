context ("maximum growing points")


max_gp <- 15000 * (1 - exp(-0.138629 * 40))

test_that("correct numbers are returned", {
  expect_length(max_gp, 1)
  expect_is(max_gp, "numeric")
  expect_equivalent(max_gp, 14941.4052278536)
})
