context("Test if the spore interception parameter is calculated approprietly")

# defines some input parameters
max_growing_points <- 15000 * (1 - exp(-0.138629 * 40))

test_defaults <- intercept_spores(max_growing_points_limit = 15000,
                                  max_new_growing_points_limit = 350)

test_that("default values return the correct number",{
  test_defaults <- intercept_spores(max_growing_points_limit = max_growing_points,
                                    max_new_growing_points_limit = 350)

  expect_equal(round(test_defaults,6), 0.002561)
  expect_length(test_defaults,1)
  expect_is(test_defaults, "numeric")

})
