context("successfully intercepted dispersed spores")

# for manual testing
spore_dat <- fread("tests/testthat/test-data_successful_infections.csv")

# for manual automatic check() testing
# spore_dat <- fread("test-data_successful_infections.csv")


# makePaddock equivalent
paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))

primary_infection_foci <-
  paddock[x == 50 &
            y == 50,
          c("x", "y")]

# define paddock variables at time 1
paddock[, new_gp := 40] # Change in the number of growing points since last iteration
paddock[, noninfected_gp := 40] #
paddock[, infected_gp := fifelse(x == primary_infection_foci[1,x] &
                                   y == primary_infection_foci[1,y], 1,
                                 0)] # Initialise column of infected growing points

spore_interception_parameter <-
  0.00006 * (15000 / 350)

set.seed(666)

test1 <- successful_infections(spore_targets = spore_dat,
                               paddock = paddock,
                               spore_interception_parameter = spore_interception_parameter,
                               max_interception_probability = 1)

test_that("test1 provides correct output",{
  expect_is(test1, "integer")
  expect_true(is.vector(test1))
  expect_length(test1, nrow(spore_dat))
  expect_equal(sum(test1),12)
  expect_equal(max(test1), 1)
  expect_equal(min(test1), 0)
  expect_false(any(is.na(test1)))
})
