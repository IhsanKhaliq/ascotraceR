
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
  expect_equal(test1.1[[5]][["exposed_gps"]][, .N], 6)
  expect_equal(test1.1[[5]][["paddock"]][infectious_gp > 0, infectious_gp], 40)
  expect_length(test1.1[[5]][["paddock"]][infectious_gp > 0, infectious_gp], 1)
  expect_equal(test1.1[[5]][["exposed_gps"]][spores_per_packet  >
                                               0, spores_per_packet], rep(1, 6))
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
pdk <- as.data.table(expand.grid(
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
  expect_equal(test3[[30]][["paddock"]][, sum(infectious_gp)], 33)
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



# library(future)
# future::plan("multisession")
# qry <- data.table(x = c(15,30),
#                   y = c(4,4),
#                   load = 40)
#
# test7 <- trace_asco(
#   weather = newM_weather,
#   paddock_length = 8,
#   paddock_width = 45,
#   initial_infection = "1998-06-18",
#   sowing_date = as.POSIXct("1998-05-20"),
#   harvest_date = as.POSIXct("1998-08-04"),
#   time_zone = "Australia/Perth",
#   primary_infection_foci = qry)
#


# Example discussed 2/6/2021
#   s_date <- as.POSIXct("1998-05-20")
#   h_date <- as.POSIXct("1998-08-04")
#
#   example <- trace_asco(
#   weather = #yourWeather######,
#   paddock_length = 8,
#   paddock_width = 45,
#   initial_infection = s_date + lubridate::ddays(14),
#   sowing_date = s_date,
#   harvest_date = h_date,
#   time_zone = "Australia/Brisbane",
#   primary_infection_foci = "centre",
#   seeding_rate = 40,
#   latent_period_cdd = 150
# )
# tracer_plot(dat = example,
#             day = 82)
# write.csv(test1[[5]]$paddock, "example.csv", row.names = FALSE)


# future::plan("sequential")

# # Test for stop error is triggered
# test_that("trace_asco stops if initial_infection is earlier than sowing_start",{
#   expect_error(
#   ta1 <- trace_asco(
#     weather = weather_dat,
#     paddock_length = 100,
#     paddock_width = 100,
#     initial_infection = "1998-03-09",
#     sowing_date = as.POSIXct("1998-03-09"),
#     harvest_date = as.POSIXct("1998-03-12"),
#     time_zone = "Australia/Perth"
#   ))
# })
#



# The following tests require time and computing power and are not included in regular testing
#
# # # test running for 100 days
# test4 <- trace_asco(
#   weather = newM_weather,
#   paddock_length = 100,
#   paddock_width = 100,
#   initial_infection = "1998-05-10",
#   sowing_date = as.POSIXct("1998-05-09"),
#   harvest_date = as.POSIXct("1998-05-09") + lubridate::ddays(80),
#   time_zone = "Australia/Perth",
#   primary_infection_foci = qry,
#   primary_infection_intensity = 40
# )
# test4[[80]] # look at values on the 80th day
#
# test_that("test4 returns some sporulating gps",{
#   expect_equal(test4[[80]][["paddock"]][,sum(infectious_gp)], 15592)
#   expect_true(all(test4[[80]][["exposed_gps"]][,unique(cdd_at_infection)] > test4[[80]][["cdd"]] - 200))
#
# })
#
# tracer_plot(test4, 82)





#
# test5 <- trace_asco(
#   weather = newM_weather,
#   paddock_length = 20,
#   paddock_width = 20,
#   initial_infection = "1998-05-10",
#   sowing_date = as.POSIXct("1998-05-09"),
#   harvest_date = as.POSIXct("1998-05-09") + lubridate::ddays(100),
#   time_zone = "Australia/Perth",
#   primary_infection_foci = "centre",
#   primary_infection_intensity = 40
#
# )
# beepr::beep(3)
# tracer_plot(test5,72, tiles = "infectious_gp")
# tracer_plot(test5,72, tiles = "susceptible_gp")
# tracer_plot(test5,72, tiles = "percent_gp_sporulating")
# test5[[72]]
#
# sapply(test5, function(x){as.character(x[["cdd"]])})
#

#

# Sowing of chick pea normally occurs between
#   Mid May to Mid June



#
# # test running for 100 days
# test5 <- trace_asco(
#   weather = newM_weather,
#   paddock_length = 75,
#   paddock_width = 75,
#   initial_infection = "1998-06-10",
#   sowing_date = as.POSIXct("1998-06-09"),
#   harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(175),
#   time_zone = "Australia/Perth",
#   primary_infection_foci = "centre"
# )
# test5[[102]] # look at values on the 102nd day
#
# test_that("test4 returns some sporulating gps from june",{
#   expect_equal(test4[[102]][["paddock"]][,sum(infectious_gp)], 5)
#
#
# })
#
#
#
# test_that("trace_asco daily_vals returns an daily vals data.frame", {
#   expect_is(ta1, "data.frame")
#   expect_true(all(c("cdd", "cwh", "cr", "i", "day") %in% colnames(ta1)))
# })
#
# test_that("trace_asco daily_vals returns the correct data.frame dimensions", {
#   expect_equal(nrow(ta1), 4)
#   expect_equal(ncol(ta1), 5)
# })
#
# test_that("trace_asco daily_vals the following contents", {
#   expect_equal(ta1$cdd, c(0,28,53,71))
#   expect_equal(ta1$cwh, c(0,0,0,5))
#   expect_equal(ta1$cr, c(0,0,0,5.33))
#   expect_equal(ta1$i[1], as.POSIXct("1998-03-09", tz = "Australia/Perth"))
#   expect_equal(ta1$day, c(68,69,70,71))
#   expect_equal(ncol(ta1), 5)
# })
#
#
# # Test for stop error is triggered
# test_that("trace_asco stops if initial_infection is earlier than sowing_start",{
#   expect_error(
#   ta1 <- trace_asco(
#     weather = weather_dat,
#     paddock_length = 100,
#     paddock_width = 100,
#     initial_infection = "1998-03-09",
#     sowing_date = as.POSIXct("1998-03-09"),
#     harvest_date = as.POSIXct("1998-03-12"),
#     time_zone = "Australia/Perth"
#   ))
# })
#
# example mocked up on 5/8/2021 for ihsan
# ta1 <- trace_asco(
#       weather = weather_dat,
#       paddock_length = 20,
#       paddock_width = 20,
#       initial_infection = as.POSIXct("1998-06-04") + lubridate::ddays(30),
#       sowing_date = as.POSIXct("1998-06-09"),
#       harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(135) ,
#       time_zone = "Australia/Perth",
#       seeding_rate = 40,
#       gp_rr = 0.0065,
#       spores_per_gp_per_wet_hour = 0.22,
#       latent_period_cdd = 150,
#       primary_infection_intensity = 100
#     )
