context("function updates the number of growing points in a paddock")

paddock <- expand.grid(x = 1:40,
                       y = 1:40)

paddock$noninfected_gp <- sample(c(-1,0), nrow(paddock), replace = TRUE)

GPs <- update_paddock_gp(
  paddock_coords = paddock,
  mean_air_temp = 24,
  gp_rr = 0.0065,
  max_gp = 15000
)

test_that("the correct length vector is returned", {
  expect_length(GPs, 40*40)
})
