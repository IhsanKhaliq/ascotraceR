
test1 <-
  potentially_effective_spores(
    spores_per_gp_per_wet_hour = 0.22,
    # default parameter of the model
    max_interception_probability = 1,
    # calculated in spread_spores and almost always 1
    paddock_infected_gp = 0
  ) # number of infected growing points at coordinates

test_that("test1 returns zero", {
  expect_type(test1, "double")
  expect_equal(test1, 0)
  expect_length(test1, 1)
})

test2 <-
  potentially_effective_spores(
    spores_per_gp_per_wet_hour = 0.22,
    # default parameter of the model
    max_interception_probability = 1,
    # calculated in spread_spores and almost always 1
    paddock_infected_gp = 1
  ) # number of infected growing points at coordinates

test_that("test2 returns in range of 0 -2", {
  expect_type(test2, "integer")
  expect_lt(test2, 6)
  expect_gte(test2, 0)
  expect_length(test2, 1)
})

test3 <- sapply(rep(1, 1e5), function(x) {
  potentially_effective_spores(
    spores_per_gp_per_wet_hour = 0.22,
    max_interception_probability = 1,
    paddock_infected_gp = x
  )
})

test_that("test3 returns in range of 100000 iterations", {
  expect_type(test3, "integer")
  expect_true(all(unique(test3) %in% 0:6))
  expect_gt(mean(test3), 0.21)
  expect_lt(mean(test3), 0.222)
})



test4 <- sapply(rep(40, 1e5), function(x) {
  potentially_effective_spores(
    spores_per_gp_per_wet_hour = 0.22,
    max_interception_probability = 1,
    paddock_infected_gp = x
  )
})

test_that("test3 returns in range of 100000 iterations", {
  expect_type(test4, "integer")
  expect_true(all(unique(test3) %in% 0:18))
  expect_gt(mean(test4), 8.5)
  expect_lt(mean(test4), 9)

})

test_that("test5 returns an error", {
  expect_error(
    potentially_effective_spores(
      spores_per_gp_per_wet_hour = 0.22,
      max_interception_probability = -1,
      paddock_infected_gp = 1
    )
  )

})
