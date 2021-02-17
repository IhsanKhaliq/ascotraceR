context("spores spread each day triggered by each wet hour")

load_all()

# load formatted weather data `newM_weather`
weather_day <- fread("tests/testthat/formatted_weather_one_day.csv")

#weather_day <- fread("formatted_weather_one_day.csv")

# makePaddock equivalent
paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))

primary_infection_foci <-
    c(50,50)

# define paddock variables at time 1
paddock[, new_gp := 40] # Change in the number of growing points since last iteration
paddock[, noninfected_gp := 40] #
paddock[, infected_gp := fifelse(x >= 53 &
                                   x <= 57 &
                                   y >= 53 &
                                   y <= 57, 5,
                                 0)] # Initialise column of infected growing points

spore_interception_parameter <-
  0.00006 * (15000 / 350)

# write some tests
set.seed(666)
test1 <- spores_each_wet_hour(
  h = 1,
  weather_hourly = weather_day,
  paddock = paddock,
  max_interception_probability = 1,
  spore_interception_parameter = spore_interception_parameter
)

test_that("test1 returns expected output",{
  expect_is(test1, "data.table")
  expect_equal(nrow(test1), 12)
  expect_equal(colnames(test1), c("x", "y", "spores_per_packet"))
  expect_equal(test1[1, x], 54)
  expect_equal(test1[1, y], 53)
  expect_equal(test1[1, spores_per_packet], 1)
  expect_is(test1[,x], "integer")
  expect_is(test1[,y], "integer")
  expect_is(test1[,spores_per_packet], "integer")

})

test2 <- lapply(seq_len(weather_day[1,wet_hours]),
               FUN = spores_each_wet_hour,
               weather_hourly = weather_day,
               paddock = paddock,
               max_interception_probability = 1,
               spore_interception_parameter = spore_interception_parameter)


test_that("test2 with lapply returns expected output",{
  expect_is(test2, "list")
  expect_length(test2, 7)
  expect_silent(test2 <- rbindlist(test2))
  expect_is(test2, "data.table")
  expect_equal(nrow(test2), 39)
  expect_equal(colnames(test2), c("x", "y", "spores_per_packet"))
  expect_equal(test2[1, x], 57)
  expect_equal(test2[1, y], 53)
  expect_equal(max(test2[, spores_per_packet]), 1)
  expect_is(test2[,x], "integer")
  expect_is(test2[,y], "integer")
  expect_is(test2[,spores_per_packet], "integer")
  expect_false(any(is.na(test2)))
})


