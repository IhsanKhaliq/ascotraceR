context("Simulate disease progression for a single day increment")

# import and define data
seeding_rate <- 40
primary_infection_foci <- c(50,50)

paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))

# initialise daily_vals
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
    0)]

daily_vals <-
  list(
    paddock = paddock,
#    i_date = sowing_date,  # day of the simulation (iterator)
    i_day = 1,
#    day = lubridate::yday(sowing_date),    # day of the year
    cdd = 0,    # cumulative degree days
    cwh = 0,    # cumulative wet hours
    cr = 0,     # cumulative rainfall
    gp_standard = seeding_rate,     # standard number of growing points for 1m^2 if not inhibited by infection (refUninfectiveGrowingPoints)
    new_gp = seeding_rate    # new number of growing points for current iteration (refNewGrowingPoints)
  )

newly_inf <- fread("tests/testthat/data-newly_infected_list.csv")


od_t1_i_date <-as.POSIXct("1998-05-12", tz = "Australia/Perth")
od_t1 <- one_day(i_date = od_t1_i_date,
                 daily_vals = daily_vals_dt,
                 weather_dat = newM_weather)

test_that("one_day returns a data.table", {
  expect_is(od_t1, "data.table")
})

test_that("one_day daily_vals returns an daily vals data.frame", {
  expect_is(ta1, "data.frame")
  expect_true(all(c("cdd", "cwh", "cr", "i", "day") %in% colnames(od_t1)))
})

test_that("trace_asco daily_vals returns the correct data.frame dimensions", {
  expect_equal(nrow(od_t1), 2)
  expect_equal(ncol(od_t1), 5)
})

test_that("trace_asco daily_vals the following contents", {
  expect_equal(od_t1$cdd, c(0,18))
  expect_equal(od_t1$cwh, c(0,15))
  expect_equal(od_t1$cr, c(0,78.85))
  expect_equal(od_t1$i[2], od_t1_i_date)
  expect_equal(od_t1$day, c(0, 132))
})



test_that("newly_infected_list updates the daily_vals paddock infected gps",{
  daily_vals <-
    apply(newly_inf, 1, function(NIL, DVL = daily_vals) {
      DVL[[paddock]][x == NIL["x"] &
                       y == NIL["x"], c("infected_gp", "cdd_at_infection") :=
                       list(NIL["spores_per_packet"],
                            DVL[["cdd"]])]

      return(DVL)

    })

})
