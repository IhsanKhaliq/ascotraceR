context("Simulate disease progression for a single day increment")

# import and define data
seeding_rate <- 40
primary_infection_foci <- data.table(x = 50,
                                     y = 50)

paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))

# initialise daily_vals
paddock[, c(
  "new_gp", # Change in the number of growing points since last iteration
  "susceptible_gp",
  "exposed_gp",
  "infectious_gp", # replacing InfectiveElementList
  "cdd_at_infection"
) :=
  list(
    seeding_rate,
    fifelse(x == primary_infection_foci[,x] &
              y == primary_infection_foci[,y], seeding_rate - 1,
            seeding_rate),
    0,
    fifelse(x == primary_infection_foci[,x] &
              y == primary_infection_foci[,y], 1,
            0),
    0
  )]

max_gp_lim <- 15000
max_new_gp <- 350

spore_interception_parameter <-
  0.00006 * (max_gp_lim/max_new_gp)

max_gp <- max_gp_lim * (1 - exp(-0.138629 * seeding_rate))

# set the iteration date
od_t1_i_date <-as.POSIXct("1998-05-12", tz = "Australia/Perth")

daily_vals_list <-
  list(
    paddock = paddock, # data.table each row is a 1 x 1m coordinate
    i_date = od_t1_i_date,  # day of the simulation (iterator)
    i_day = 1,
    day = lubridate::yday(od_t1_i_date),    # day of the year
    cdd = 0,    # cumulative degree days
    cwh = 0,    # cumulative wet hours
    cr = 0,     # cumulative rainfall
    gp_standard = seeding_rate,     # standard number of growing points for 1m^2 if not inhibited by infection (refUninfectiveGrowingPoints)
    new_gp = seeding_rate,    # new number of growing points for current iteration (refNewGrowingPoints)
    infected_coords = primary_infection_foci,  # data.frame
    exposed_gps = data.table() # data.table of infected growing points still in latent period and not sporilating (exposed_gp)
  )


#newly_inf <- fread("tests/testthat/data-newly_infected_list.csv")


# begin testing
set.seed(666)
test1 <- one_day(i_date = od_t1_i_date,
                 daily_vals = daily_vals_list,
                 weather_dat = newM_weather,
                 gp_rr = 0.0065,
                 max_gp = max_gp,
                 spore_interception_parameter = spore_interception_parameter,
                 spores_per_gp_per_wet_hour = 0.22)


test_that("one_day single infection foci returns expected output", {
  expect_silent(test1[["paddock"]]) # This line is here due to https://github.com/Rdatatable/data.table/issues/869
  test1[["paddock"]]
  expect_is(test1, "list")
  expect_length(test1, 11)
  expect_equal(
    names(test1),
    c(
      "paddock",
      "i_date",
      "i_day",
      "day",
      "cdd",
      "cwh",
      "cr",
      "gp_standard",
      "new_gp",
      "infected_coords",
      "exposed_gps"
    )
  )
  expect_equal(test1[["i_date"]], od_t1_i_date )
  expect_equal(test1[["i_day"]],1 )
  expect_equal(test1[["day"]], 132 )
  expect_equal(test1[["cdd"]],18)
  expect_equal(test1[["cwh"]],15 )
  expect_equal(test1[["cr"]],78.85 )
  expect_equal(round(test1[["gp_standard"]],5), 44.66747)
  expect_equal(test1[["new_gp"]],test1[["gp_standard"]] - seeding_rate)
  expect_equal(test1[["exposed_gps"]], data.table(x = c(50,54,50),
                                                  y = c(49,37,50),
                                                  spores_per_packet = c(1,1,1),
                                                  cdd_at_infection = c(18,18,18)))
  expect_is(test1[["paddock"]], "data.table")
  expect_equal(
    colnames(test1[["paddock"]]),
    c(
      "x",
      "y",
      "new_gp",
      "susceptible_gp",
      "exposed_gp",
      "infectious_gp",
      "cdd_at_infection"
    )
  )
  expect_equal(round(test1[["paddock"]][,unique(new_gp)],6), c(4.667471, 4.551090))
  expect_equal(round(test1[["paddock"]][,unique(susceptible_gp)],5), c(44.66747, 43.55109))
  expect_equal(round(test1[["paddock"]][,unique(exposed_gp)],6), 0)
  expect_equal(round(test1[["paddock"]][,unique(infectious_gp)],6), c(0, 1))
  })

test2 <- one_day(i_date = od_t1_i_date,
                 daily_vals = test1,  # add to test1 daily vals
                 weather_dat = newM_weather,
                 gp_rr = 0.0065,
                 max_gp = max_gp,
                 spore_interception_parameter = spore_interception_parameter,
                 spores_per_gp_per_wet_hour = 0.22)

test_that("one_day test2 repeat using test1 single infection foci returns expected output", {
  expect_silent(test2[["paddock"]]) # This line is here due to https://github.com/Rdatatable/data.table/issues/869
  test2[["paddock"]]
  expect_is(test2, "list")
  expect_length(test2, 11)
  expect_equal(
    names(test2),
    c(
      "paddock",
      "i_date",
      "i_day",
      "day",
      "cdd",
      "cwh",
      "cr",
      "gp_standard",
      "new_gp",
      "infected_coords",
      "exposed_gps"
    )
  )
  expect_equal(test2[["i_date"]], od_t1_i_date )
  expect_equal(test2[["i_day"]],1 )
  expect_equal(test2[["day"]], 132 )
  expect_equal(test2[["cdd"]],18 + 18)
  expect_equal(test2[["cwh"]],15 +15)
  expect_equal(test2[["cr"]],78.85 +78.85)
  expect_equal(round(test2[["gp_standard"]],5), 44.66747 + 5.21047)
  expect_equal(round(test2[["new_gp"]],5),5.21047)
  expect_equal(test2[["exposed_gps"]], data.table(x = c(50,54,50),
                                                     y = c(49,37,50),
                                                     spores_per_packet = c(1,1,1),
                                                     cdd_at_infection = c(18,18,18)))
  expect_is(test2[["paddock"]], "data.table")
  expect_equal(
    colnames(test2[["paddock"]]),
    c(
      "x",
      "y",
      "new_gp",
      "susceptible_gp",
      "exposed_gp",
      "infectious_gp",
      "cdd_at_infection"
    )
  )
  expect_equal(round(test2[["paddock"]][,unique(new_gp)],6), c(5.210471, 5.080625))
  expect_equal(round(test2[["paddock"]][,unique(susceptible_gp)],5), c(49.87794, 48.63171))
  expect_equal(round(test2[["paddock"]][,unique(exposed_gp)],6), 0)
  expect_equal(round(test2[["paddock"]][,unique(infectious_gp)],6), c(0, 1))
})

# update cumulative degree days to past threshold
test2[["cdd"]] <- 201

test3 <- one_day(i_date = od_t1_i_date,
                    daily_vals = test2,  # add to test1 daily vals
                    weather_dat = newM_weather,
                    gp_rr = 0.0065,
                    max_gp = max_gp,
                    spore_interception_parameter = spore_interception_parameter,
                 spores_per_gp_per_wet_hour = 0.22)

test_that("one_day test3 adds to cumulative degree days and passes latent period", {
  expect_silent(test3[["paddock"]])
  test3[["paddock"]]
  expect_is(test3, "list")
  expect_length(test3, 11)
  expect_equal(
    names(test3),
    c(
      "paddock",
      "i_date",
      "i_day",
      "day",
      "cdd",
      "cwh",
      "cr",
      "gp_standard",
      "new_gp",
      "infected_coords",
      "exposed_gps"
    )
  )
  expect_equal(test3[["i_date"]], od_t1_i_date )
  expect_equal(test3[["i_day"]],1 )
  expect_equal(test3[["day"]], 132 )
  expect_equal(test3[["cdd"]],219)
  expect_equal(test3[["cwh"]],15 +15 +15)
  expect_equal(test3[["cr"]], 78.85 + 78.85 + 78.85)
  expect_equal(round(test3[["gp_standard"]],5), 44.66747 + 5.21047 + 5.81624)
  expect_equal(round(test3[["new_gp"]],5),5.81624)
  expect_equal(test3[["exposed_gps"]], data.table(x = numeric(),
                                                     y = numeric(),
                                                     spores_per_packet = numeric(),
                                                     cdd_at_infection = numeric()))
  expect_is(test3[["paddock"]], "data.table")
  expect_equal(
    colnames(test3[["paddock"]]),
    c(
      "x",
      "y",
      "new_gp",
      "susceptible_gp",
      "exposed_gp",
      "infectious_gp",
      "cdd_at_infection"
    )
  )
  expect_equal(round(test3[["paddock"]][,unique(new_gp)],6), c(5.816238, 5.700011,5.438883))
  expect_equal(round(test3[["paddock"]][,unique(susceptible_gp)],5), c(55.69418, 54.57795, 52.0706))
  expect_equal(round(test3[["paddock"]][,unique(exposed_gp)],6), 0)
  expect_equal(round(test3[["paddock"]][,unique(infectious_gp)],6), c(0,1,3))
})

