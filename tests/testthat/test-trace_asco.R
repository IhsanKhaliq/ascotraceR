
sowing_date <- as.POSIXct("1998-05-09", tz = "Australia/Perth")
harvest_date <- as.POSIXct("1998-05-12", tz = "Australia/Perth")

# Test running for 3 days
test1 <- trace_asco(
  weather = newM_weather,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-05-10",
  sowing_date = as.POSIXct("1998-05-09"),
  harvest_date = as.POSIXct("1998-05-12"),
  time_zone = "Australia/Perth",
  # weather file is in Perth timezone
  primary_infection_foci = "centre"
)

test_that("days have updated after 5 increments", {
  expect_equal(sapply(test1, function(x) {
    as.character(x[["i_date"]])
  }),
  as.character(
    seq(
      from = sowing_date,
      to = harvest_date + lubridate::ddays(1),
      by = "days"
    )
  ))
  expect_length(test1, 5)
  expect_length(test1[[1]], 11)
  expect_equal(
    colnames(test1[[5]][["paddock"]]),
    c(
      "x",
      "y",
      "new_gp",
      "susceptible_gp",
      "exposed_gp",
      "infectious_gp"
    )
  )
  expect_equal(test1[[5]][["day"]], lubridate::yday(harvest_date) + 1)
  expect_equal(test1[[5]][["i_day"]], 5)
  expect_equal(test1[[5]][["cwh"]], newM_weather[times >
                                                   sowing_date +
                                                   lubridate::dminutes(1) &
                                                   times <= harvest_date +
                                                   lubridate::dhours(23),
                                                 sum(!is.na(rain))])
  expect_equal(test1[[5]][["cdd"]], newM_weather[times > sowing_date +
                                                   lubridate::dminutes(1) &
                                                   times <= harvest_date +
                                                   lubridate::dhours(23),
                                                 mean(temp),
                                                 by = day][, sum(V1)])
  Ninf_coord <- lapply(test1, function(L1) {
    nrow(L1[["infected_coords"]])
  })
  # is infected coordinates updated after initial infection
  expect_equal(unlist(Ninf_coord), c(0, 1, 1, 1, 1))

  Ninf_pad <- lapply(test1, function(L1) {
    nrow(L1[["paddock"]][infectious_gp > 0])
  })
  # is infected coordinates updated after initial infection
  expect_equal(unlist(Ninf_pad), c(0, 1, 1, 1, 1))
})


set.seed(667)
# test more intensity start
test1.1 <- trace_asco(
  weather = newM_weather,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-05-10",
  sowing_date = as.POSIXct("1998-05-09"),
  harvest_date = as.POSIXct("1998-05-12"),
  time_zone = "Australia/Perth",
  # weather file is in Perth timezone
  primary_infection_foci = "centre",
  primary_infection_intensity = 40
)

test_that("intense primary_infection_foci lead to more infections", {
  expect_equal(sapply(test1.1, function(x) {
    as.character(x[["i_date"]])
  }), as.character(
    seq(
      from = sowing_date,
      to = harvest_date + lubridate::ddays(1),
      by = "days"
    )
  ))
  expect_length(test1.1, 5)
  expect_length(test1.1[[1]], 11)
  expect_equal(test1.1[[5]][["exposed_gps"]][, .N], 2)
  expect_equal(test1.1[[5]][["paddock"]][infectious_gp > 0, infectious_gp], 40)
  expect_length(test1.1[[5]][["paddock"]][infectious_gp > 0, infectious_gp], 1)
  expect_equal(test1.1[[5]][["exposed_gps"]][spores_per_packet  >
                                               0, spores_per_packet], rep(1, 2))
  expect_equal(test1.1[[5]][["exposed_gps"]][, unique(cdd_at_infection)], 87)
})

# test running for 14 days
# this will test that the infection intensifies with more days and
#  that newly infected gp are moved to sporilating gp after the latent period
test2 <- trace_asco(
  weather = newM_weather,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-03-10",
  sowing_date = as.POSIXct("1998-03-09"),
  harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(14),
  time_zone = "Australia/Perth",
  primary_infection_foci = "centre"
)

test_that("intense primary_infection_foci lead to more infections", {
  expect_equal(sapply(test1.1, function(x) {
    as.character(x[["i_date"]])
  }), as.character(
    seq(
      from = sowing_date,
      to = harvest_date + lubridate::ddays(1),
      by = "days"
    )
  ))
  expect_length(test2, 16)
  expect_length(test2[[1]], 11)
  expect_equal(test2[[5]][["exposed_gps"]][, .N], 0)
  expect_equal(test2[[5]][["paddock"]][infectious_gp > 0, infectious_gp], 1)
  expect_length(test2[[5]][["paddock"]][infectious_gp > 0, infectious_gp], 1)
  expect_equal(test2[[5]][["exposed_gps"]][spores_per_packet  >
                                             0, spores_per_packet], numeric())

})

# test running for 28 days
# this will test that the infection intensifies with more days and
#  that newly infected gp are moved to sporilating gp after the latent period
test3 <- trace_asco(
  weather = newM_weather,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-03-10",
  sowing_date = as.POSIXct("1998-03-09"),
  harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(28),
  time_zone = "Australia/Perth",
  primary_infection_foci = "centre"
)


test_that("test3 returns some sporulating gps", {
  expect_equal(test3[[30]][["paddock"]][, sum(infectious_gp)], 1)
  expect_length(test3, 30)
  expect_length(test3[[1]], 11)
})


# test running for 28 days with multiple (10) random start locations
pdk <- as.data.table(CJ(
  x = 1:100,
  y = 1:100,
  load = 3
))
qry <- pdk[sample(1:nrow(pdk), 10), ]

test3 <- trace_asco(
  weather = newM_weather,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-03-10",
  sowing_date = as.POSIXct("1998-03-09"),
  harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(28),
  time_zone = "Australia/Perth",
  primary_infection_foci = qry
)

# test3[[30]] # look at values on the 30th day
#  tracer_plot(test3,30)

test_that("test3 returns some sporulating gps", {
  expect_equal(test3[[30]][["paddock"]][, sum(infectious_gp)], 31)
  expect_length(test3, 30)
  expect_length(test3[[1]], 11)
  expect_true(all(test3[[30]][["exposed_gps"]][, unique(cdd_at_infection)] >
                    test3[[30]][["cdd"]] - 200))

})

test_that("returns an error when initial infection is before sowing date", {
  expect_error(
    trace_asco(
      weather = newM_weather,
      paddock_length = 100,
      paddock_width = 100,
      initial_infection = as.POSIXct("1998-03-08"),
      sowing_date = as.POSIXct("1998-03-09"),
      harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(28),
      time_zone = "Australia/Perth",
      primary_infection_foci = qry
    )
  )
})

test_that("returns an error with invalid date formats", {
  expect_error(
    trace_asco(
      weather = newM_weather,
      paddock_length = 100,
      paddock_width = 100,
      initial_infection = "01-03-98",
      sowing_date = "01-03-98",
      harvest_date = "05-03-98",
      time_zone = "Australia/Perth",
      primary_infection_foci = qry
    )
  )
})

test_that("returns an error when primary infection intensity exceeds gp
          density",
          {
            expect_error(
              trace_asco(
                weather = newM_weather,
                paddock_length = 100,
                paddock_width = 100,
                initial_infection = as.POSIXct("1998-03-8"),
                sowing_date = as.POSIXct("1998-03-09"),
                harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(28),
                time_zone = "Australia/Perth",
                primary_infection_foci = qry,
                primary_infection_intensity = 50,
                seeding_rate = 40
              )
            )
          })

test_that("primary_infection_foci can accept an numeric input of 2", {
  expect_silent(
    test6 <- trace_asco(
      weather = newM_weather,
      paddock_length = 100,
      paddock_width = 100,
      initial_infection = "1998-03-10",
      sowing_date = as.POSIXct("1998-03-09"),
      harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(28),
      time_zone = "Australia/Perth",
      primary_infection_foci = c(1, 53)
    )
  )
})


test_that("primary_infection_foci input is a unrecognicsed character error",
          {
            expect_error(
              label = "primary_infection_foci input not recognised",
              trace_asco(
                weather = newM_weather,
                paddock_length = 100,
                paddock_width = 100,
                initial_infection = as.POSIXct("1998-03-10"),
                sowing_date = as.POSIXct("1998-03-09"),
                harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(28),
                time_zone = "Australia/Perth",
                primary_infection_foci = "qry"
              )
            )
          })

# Test for stop error is triggered
test_that("trace_asco stops if initial_infection is earlier than sowing_start",{
  expect_error(
  ta1 <- trace_asco(
    weather = newM_weather,
    paddock_length = 100,
    paddock_width = 100,
    initial_infection = "1998-03-09",
    sowing_date = as.POSIXct("1998-03-09"),
    harvest_date = as.POSIXct("1998-03-12"),
    time_zone = "Australia/Perth"
  ),regexp = "The `initial_infection` occurs on or before `sowing_date`.Please use an `initial_infection` date which occurs after `crop_sowing`.")
})


# trace_asco stops on error for non formatted weather data
test_that("trace_asco stops on error for non formatted weather data",{
  newM_weather2 <- copy(newM_weather)
  class(newM_weather2) <- c("data.table", "data.frame")
  expect_error(
    ta1 <- trace_asco(
      weather = newM_weather2,
      paddock_length = 100,
      paddock_width = 100,
      initial_infection = "1998-03-10",
      sowing_date = as.POSIXct("1998-03-09"),
      harvest_date = as.POSIXct("1998-03-12"),
      time_zone = "Australia/Perth"
    ), regexp = "'weather' must be class \"asco.weather\"")
})

