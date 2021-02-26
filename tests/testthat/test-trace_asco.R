context("trace_asco is the main function that runs the ascochyta model")

sowing_date <- as.POSIXct("1998-03-09")
harvest_date <- as.POSIXct("1998-03-12")


# Test running for 3 days
test1 <- trace_asco(
  weather = newM_weather,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-03-10",
  sowing_date = as.POSIXct("1998-03-09"),
  harvest_date = as.POSIXct("1998-03-12"),
  time_zone = "Australia/Perth",
  primary_infection_foci = "center"
)
test1[[5]]
test_that("days have updated after 5 increments",{
  expect_equal(sapply(test1, function(x){as.character(x[["i_date"]])}), as.character(seq(from = sowing_date,
                                                                            to = harvest_date + lubridate::ddays(1),
                                                                            by = "days")))
  expect_length(test1, 5)
  expect_length(test1[[1]], 11)
})


# test running for 14 days
test2 <- trace_asco(
  weather = newM_weather,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-03-10",
  sowing_date = as.POSIXct("1998-03-09"),
  harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(14),
  time_zone = "Australia/Perth",
  primary_infection_foci = "center"
)
test2[[16]] # look at values on the 16th day

# test running for 28 days
test3 <- trace_asco(
  weather = newM_weather,
  paddock_length = 100,
  paddock_width = 100,
  initial_infection = "1998-03-10",
  sowing_date = as.POSIXct("1998-03-09"),
  harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(28),
  time_zone = "Australia/Perth",
  primary_infection_foci = "center"
)
test3[[30]] # look at values on the 30th day
test_that("test3 returns some sporulating gps",{
  expect_equal(test3[[30]][["paddock"]][,sum(sporulating_gp)], 1)
  expect_length(test3, 30)
  expect_length(test3[[1]], 11)


})


# test running for 28 days with multiple start locations
pdk <- as.data.table(expand.grid(x = 1:100,
                                 y = 1:100,
                                 load = 3))
qry <- pdk[sample(1:nrow(pdk),10),]

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
test3[[30]] # look at values on the 30th day
tracer_plot(test3,30)
test_that("test3 returns some sporulating gps",{
  expect_equal(test3[[30]][["paddock"]][,sum(sporulating_gp)], 1)
  expect_length(test3, 30)
  expect_length(test3[[1]], 11)


})

#
# # test running for 100 days
# test4 <- trace_asco(
#   weather = newM_weather,
#   paddock_length = 100,
#   paddock_width = 100,
#   initial_infection = "1998-03-10",
#   sowing_date = as.POSIXct("1998-03-09"),
#   harvest_date = as.POSIXct("1998-03-09") + lubridate::ddays(100),
#   time_zone = "Australia/Perth",
#   primary_infection_foci = "center"
# )
# test4[[102]] # look at values on the 102nd day
#
# test_that("test4 returns some sporulating gps",{
#   expect_equal(test4[[102]][["paddock"]][,sum(sporulating_gp)], 5)
#
#
# })
#
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
#   primary_infection_foci = "center"
# )
# test5[[102]] # look at values on the 102nd day
#
# test_that("test4 returns some sporulating gps from june",{
#   expect_equal(test4[[102]][["paddock"]][,sum(sporulating_gp)], 5)
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
