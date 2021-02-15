context("Function is use to calculate the max_interception_probability")

# Define parameters after model defaults
plant_density <-
  5 * max(15000)

spore_interception_parameter <-
  0.00006 * (15000 / 350)

test1 <-
  sapply()
  interception_probability(target_density = plant_density,
                           k = spore_interception_parameter)

test_that("function returns the correct probabilities", {
  expect_equal(test1, 1)
})
