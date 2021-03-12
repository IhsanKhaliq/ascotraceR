context("Function is use to calculate the max_interception_probability")

# Define parameters after model defaults
plant_density <-
  5 * max(15000)

spore_interception_parameter <-
  0.00006 * (15000 / 350)

test1 <-
  interception_probability(target_density = plant_density,
                           k = spore_interception_parameter)

test2 <-
  interception_probability(1e12,0.00005)

test_that("function returns the correct probabilities", {
  expect_equal(test1, 1)

})

test_that("correct 'type' and length is returned", {
  expect_type(test1, "double")
  expect_length(test1, 1)
})


test_that("test2 contains correct output",{
  expect_equal(test2,1)

})
