
# load formatted weather data
load(test_path("formatted_weather_one_day.rda"))

# makePaddock equivalent
paddock <- CJ(x = 1:100,
              y = 1:100)

primary_infection_foci <- c(50, 50)
seeding_rate <- 40

# define paddock variables at time 1
paddock[, c(
  "new_gp", # Change in the number of growing points since last iteration
  "susceptible_gp",
  "exposed_gp",
  "infectious_gp" # replacing InfectiveElementList
) :=
  list(
    seeding_rate,
    fifelse(
      x == primary_infection_foci[1] &
        y == primary_infection_foci[2],
      seeding_rate - 1,
      seeding_rate
    ),
    0,
    fifelse(x == primary_infection_foci[1] &
              y == primary_infection_foci[2], 1,
            0)
  )]

spore_interception_parameter <- 0.00006 * (15000 / 350)

####  write some tests
# test 1 - single inputs tested 100 times
set.seed(666)

for (repeat1000 in 1:100) {
  test1 <- spores_each_wet_hour(
    h = 1,
    weather_hourly = w_dat,
    paddock = paddock,
    max_interception_probability = 1,
    spore_interception_parameter = spore_interception_parameter,
    spores_per_gp_per_wet_hour = 0.22
  )
}

test_that("test1 returns expected output", {
  expect_s3_class(test1, "data.table")
  expect_equal(nrow(test1), 0)
  expect_equal(colnames(test1), c("x", "y", "spores_per_packet"))
  expect_equal(test1[1, x], NA_real_)
  expect_equal(test1[1, y], NA_real_)
  expect_equal(test1[1, spores_per_packet], NA_real_)
  set.seed(7)
  expect_silent(
    test1 <- spores_each_wet_hour(
      h = 1,
      weather_hourly = w_dat,
      paddock = paddock,
      max_interception_probability = 1,
      spore_interception_parameter = spore_interception_parameter,
      spores_per_gp_per_wet_hour = 0.22
    )
  )
  expect_s3_class(test1, "data.table")
  expect_equal(nrow(test1), 1)
  expect_equal(colnames(test1), c("x", "y", "spores_per_packet"))
  expect_equal(test1[1, x], 51)
  expect_equal(test1[1, y], 50)
  expect_equal(test1[1, spores_per_packet], 1)
  expect_is(test1[,x], "integer")
  expect_is(test1[,y], "integer")
  expect_is(test1[,spores_per_packet], "integer")
})


# add more than one sporulating growing point
paddock[, infectious_gp := fifelse(x >= 53 &
                                   x <= 57 &
                                   y >= 53 &
                                   y <= 57, 5,
                                 0)] # Initialise column of infected growing points


test2 <- spores_each_wet_hour(
  h = 1,
  weather_hourly = w_dat,
  paddock = paddock,
  max_interception_probability = 1,
  spore_interception_parameter = spore_interception_parameter,
  spores_per_gp_per_wet_hour = 0.22
)

test_that("test2 returns expected output", {
  expect_s3_class(test2, "data.table")
  expect_equal(nrow(test2), 15)
  expect_equal(colnames(test2), c("x", "y", "spores_per_packet"))
  expect_equal(test2[1, x], 53)
  expect_equal(test2[1, y], 54)
  expect_equal(test2[1, spores_per_packet], 1)
  expect_is(test2[, x], "integer")
  expect_is(test2[, y], "integer")
  expect_is(test2[, spores_per_packet], "integer")
})

test3 <- lapply(seq_len(w_dat[1,wet_hours]),
               FUN = spores_each_wet_hour,
               weather_hourly = w_dat,
               paddock = paddock,
               max_interception_probability = 1,
               spore_interception_parameter = spore_interception_parameter,
               spores_per_gp_per_wet_hour = 0.22)


test_that("test3 with lapply returns expected output", {
  expect_is(test3, "list")
  expect_length(test3, 7)
  expect_silent(test3 <- rbindlist(test3))
  expect_s3_class(test3, "data.table")
  expect_equal(nrow(test3), 59)
  expect_equal(colnames(test3), c("x", "y", "spores_per_packet"))
  expect_equal(test3[1, x], 53)
  expect_equal(test3[1, y], 56)
  expect_equal(max(test3[, spores_per_packet]), 1)
  expect_is(test3[, x], "integer")
  expect_is(test3[, y], "integer")
  expect_is(test3[, spores_per_packet], "integer")
  expect_false(any(is.na(test3)))
})
