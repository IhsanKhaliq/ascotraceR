context("Test if the spore interception parameter is calculated approprietly")

test_defaults <- intercept_spores(max_growing_points_limit = 15000,
                                  max_new_growing_points_limit = 350)

test_that("default values return the correct number",{
  test_defaults <- intercept_spores(max_growing_points_limit = 15000,
                                    max_new_growing_points_limit = 350)

  expect_equal(test_defaults, 0.002571429)
  expect_length(test_defaults,1)
  expect_is(test_defaults, "numeric")

})
