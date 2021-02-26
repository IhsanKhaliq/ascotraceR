context("successfully intercepted dispersed spores")

# for manual testing
#spore_dat <- fread("tests/testthat/test-data_successful_infections.csv")

# for manual automatic check() testing
spore_dat <- fread("test-data_successful_infections.csv")
seeding_rate <- 40

# makePaddock equivalent
paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))

primary_infection_foci <-
  c(50,50)

# define paddock variables at time 1
paddock[, c(
  "new_gp", # Change in the number of growing points since last iteration
  "noninfected_gp",
  "infected_gp",
  "sporilating_gp", # replacing InfectiveElementList
  "cdd_at_infection"
) :=
  list(
    seeding_rate,
    fifelse(x == primary_infection_foci[1] &
              y == primary_infection_foci[2], seeding_rate - 1,
            seeding_rate),
    0,
    fifelse(x == primary_infection_foci[1] &
              y == primary_infection_foci[2], 1,
            0),
    0
  )]



spore_interception_parameter <-
  0.00006 * (15000 / 350)

set.seed(666)



test1 <- successful_infections(spore_targets = data.frame(x = 50,
                                                          y = 50,
                                                          spores_per_packet = 1),
                               paddock = paddock,
                               spore_interception_parameter = spore_interception_parameter,
                               max_interception_probability = 1)

test_that("test1 provides correct output",{
  expect_is(test1, "integer")
  expect_true(is.vector(test1))
  expect_length(test1, 1)
  expect_equal(sum(test1),0)
  expect_equal(max(test1), 0)
  expect_equal(min(test1), 0)
  expect_false(any(is.na(test1)))
})


test2 <- successful_infections(spore_targets = spore_dat,
                               paddock = paddock,
                               spore_interception_parameter = spore_interception_parameter,
                               max_interception_probability = 1)

test_that("test2 provides correct output",{
  expect_is(test2, "integer")
  expect_true(is.vector(test2))
  expect_length(test2, nrow(spore_dat))
  expect_equal(sum(test2),12)
  expect_equal(max(test2), 1)
  expect_equal(min(test2), 0)
  expect_false(any(is.na(test2)))
})

test_that("successful_infections returns an error with incorrect input",{
  expect_error(
    test3 <- successful_infections(spore_targets = as.list(spore_dat),
                                   paddock = paddock,
                                   spore_interception_parameter = spore_interception_parameter,
                                   max_interception_probability = 1)
  )

})
