
# import weather and filter to a single day with rain
load(test_path("formatted_weather_one_day.rda"))

# makePaddock equivalent
# create data and parameters
seeding_rate <- 40
paddock <- data.table(CJ(x = 1:100,
                         y = 1:100))
paddock[, c("new_gp",
            "susceptible_gp",
            "infected_gp",
            "infectious_gp",
            "cdd_at_infection") :=
          list(
            seeding_rate,
            fifelse(x == 50 &
                      y == 50, seeding_rate - 1,
                    seeding_rate),
            0,
            fifelse(x == 50 &
                      y == 50, 1,
                    0),
            0
          )]


paddock_infected <- paddock[infectious_gp > 0, ]

set.seed(667)

test1 <- apply(
  X = paddock_infected,
  MARGIN = 1,
  FUN = spores_from_1_element,
  spores_per_gp_per_wet_hour = 0.15,
  # default parameter for the function
  max_interception_probability = 1,
  # parameter in potentially_effective_spores defined in spread_spores
  wind_direction_in_hour = w_dat[1, wd],
  average_wind_speed_in_hour = w_dat[1, ws],
  stdev_wind_direction_in_hour = w_dat[1, wd_sd],
  spore_aggregation_limit = 1000,
  splash_cauchy_parameter = 0.5,
  paddock = paddock
)

test_that("test1 returns NULL", {
  expect_null(test1)
})

set.seed(7)

test2 <- apply(
  X = paddock_infected,
  MARGIN = 1,
  FUN = spores_from_1_element,
  spores_per_gp_per_wet_hour = 0.15,
  # default parameter for the function
  max_interception_probability = 1,
  # parameter in potentially_effective_spores defined in spread_spores
  wind_direction_in_hour = w_dat[1, wd],
  average_wind_speed_in_hour = w_dat[1, ws],
  stdev_wind_direction_in_hour = w_dat[1, wd_sd],
  spore_aggregation_limit = 1000,
  splash_cauchy_parameter = 0.5,
  paddock = paddock
)

test_that("test2 is data.table of 1 row", {
  expect_is(test2, "list")
  expect_s3_class(test2[[1]], "data.table")
  expect_equal(nrow(test2[[1]]), 1)
  expect_equal(colnames(test2[[1]]), c("x", "y", "spores_per_packet"))
  expect_equal(test2[[1]][1, x], 50)
  expect_equal(test2[[1]][1, y], 49)
  expect_equal(test2[[1]][1, spores_per_packet], 1)
})


set.seed(66)

test3 <- apply(
  X = paddock_infected,
  MARGIN = 1,
  FUN = spores_from_1_element,
  spores_per_gp_per_wet_hour = 0.15,
  # default parameter for the function
  max_interception_probability = 1,
  # parameter in potentially_effective_spores defined in spread_spores
  wind_direction_in_hour = w_dat[1, wd],
  average_wind_speed_in_hour = w_dat[1, ws],
  stdev_wind_direction_in_hour = w_dat[1, wd_sd],
  spore_aggregation_limit = 1000,
  splash_cauchy_parameter = 0.5,
  paddock = paddock
)

test_that("test3 is data.table of 1 row", {
  expect_is(test3, "list")
  expect_s3_class(test3[[1]], "data.table")
  expect_equal(nrow(test3[[1]]), 2)
  expect_equal(colnames(test3[[1]]), c("x", "y", "spores_per_packet"))
  expect_equal(test3[[1]][, x], c(49, 50))
  expect_equal(test3[[1]][, y], c(51, 50))
  expect_equal(test3[[1]][, spores_per_packet], c(1, 1))
})


set.seed(666)

#seleck 20 coordinates to randomly allocate infectious_gps
vec_R1 <- sample(1:nrow(paddock), size = 20, replace = FALSE)

# filter paddock to only the infected coordinates
paddock_infected <- paddock[vec_R1, ]

# give infected coordinates infectious_gp of between 1:20
paddock_infected[, infectious_gp := sample(1:20, size = 20, replace = FALSE)]

# use new infected data in model
test4 <- apply(
  X = paddock_infected,
  MARGIN = 1,
  FUN = spores_from_1_element,
  spores_per_gp_per_wet_hour = 0.15,
  # default parameter for the function
  max_interception_probability = 1,
  # parameter in potentially_effective_spores defined in spread_spores
  wind_direction_in_hour = w_dat[1, wd],
  average_wind_speed_in_hour = w_dat[1, ws],
  stdev_wind_direction_in_hour = w_dat[1, wd_sd],
  spore_aggregation_limit = 1000,
  splash_cauchy_parameter = 0.5,
  paddock = paddock
)

test_that("test4 is a list of data.tables", {
  expect_is(test4, "list")
  expect_length(test4, 20)
  expect_s3_class(test4[[2]], "data.table")
  expect_equal(nrow(test4[[2]]), 3)
  expect_silent(dt_test4 <- rbindlist(test4))
  expect_equal(colnames(dt_test4), c("x", "y", "spores_per_packet"))
  expect_is(dt_test4[, x], "integer")
  expect_is(dt_test4[, y], "integer")
  expect_is(dt_test4[, spores_per_packet], "numeric") # it would be better if this was integer
  expect_false(any(is.na(dt_test4)))
})
