#context("makes some infected growing points infective or a source of innoculum")
load_all()
newly_infected_list <- fread(file = "tests/testthat/data-newly_infected_list.csv")

# create data and parameters
seeding_rate <- 40
paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))
paddock[, c("new_gp",
            "noninfected_gp",
            "infected_gp",
            "sporilating_gp",
            # replacing InfectiveElementList
            "cdd_at_infection") :=
          list(
            seeding_rate,
            fifelse(x >= 53 &
                      x <= 57 &
                      y >= 53 &
                      y <= 57, seeding_rate - 5,
                    seeding_rate),
            0,
            fifelse(x >= 53 &
                      x <= 57 &
                      y >= 53 &
                      y <= 57, 5,
                    0)
          )]

daily_values <- list(
  paddock = paddock,
  # i_date = sowing_date,  # day of the simulation (iterator)
  i_day = 1,
  # day = lubridate::yday(sowing_date),    # day of the year
  cdd = 0,    # cumulative degree days
  cwh = 0,    # cumulative wet hours
  cr = 0,     # cumulative rainfall
  gp_standard = seeding_rate,     # standard number of growing points for 1m^2 if not inhibited by infection (refUninfectiveGrowingPoints)
  new_gp = seeding_rate    # new number of growing points for current iteration (refNewGrowingPoints)
  #infected_coords = primary_infection_foci  # data.frame
)

sp1 <- c(30, 10, 1)
names(sp1) <- c("x", "y", "spores_per_packet")


test1 <- make_some_infective(spore_packet = sp1,
                             daily_vals = daily_values,
                             latent_period = 200)

test1[["paddock"]][,cdd_at_infection  > 0,]
test_that("test1 returns a list with changes to paddock", {
  expect_is(test1, "list")
  expect_length(test1, 7)
  expect_is(test1[["paddock"]], "data.table")
  expect_equal(names(test1),
               c(
                 "paddock",
                 "i_day",
                 "cdd",
                 "cwh",
                 "cr",
                 "gp_standard",
                 "new_gp"
               ))
  expect_equal(test1[["paddock"]][x == sp1["x"] &
                                    y == sp1["y"], sporilating_gp], 1)
  expect_equal(test1[["paddock"]][x == sp1["x"] &
                                    y == sp1["y"], noninfected_gp], 39)
  expect_silent(test1 <- make_some_infective(spore_packet = sp1,
                                             daily_vals = daily_values))
  expect_equal(test1[["paddock"]][x == sp1["x"] &
                                    y == sp1["y"], sporilating_gp], 2)
  expect_equal(test1[["paddock"]][x == sp1["x"] &
                                    y == sp1["y"], noninfected_gp], 38)
  test1[["paddock"]][x == sp1["x"] &
                       y == sp1["y"], cdd_at_infection := 10]


  expect_silent(test1 <- make_some_infective(spore_packet = sp1,
                                             daily_vals = daily_values))
  expect_equal(test1[["paddock"]][x == sp1["x"] &
                                    y == sp1["y"], sporilating_gp], 3)
  expect_equal(test1[["paddock"]][x == sp1["x"] &
                                    y == sp1["y"], noninfected_gp], 37)
  expect_is(test1[["paddock"]][, noninfected_gp], "numeric")
  expect_is(test1[["paddock"]][, sporilating_gp], "numeric")
  expect_false(any(is.na(test1[["paddock"]][, -("cdd_at_infection")])))
})

sp2 <- fread("tests/testthat/data-newly_infected_list.csv")

test2 <- make_some_infective(spore_packet = sp2,
               daily_vals = daily_values)

#test2[["paddock"]][sporilating_gp > 0,]
test_that("test2 long dt input returns a list with changes to paddock", {
  expect_is(test2, "list")
  expect_length(test2, 7)
  expect_is(test2[["paddock"]], "data.table")
  expect_equal(names(test2),
               c(
                 "paddock",
                 "i_day",
                 "cdd",
                 "cwh",
                 "cr",
                 "gp_standard",
                 "new_gp"
               ))
  expect_equal(test2[["paddock"]][sporilating_gp > 0, .N ], 30)
  expect_equal(test2[["paddock"]][sporilating_gp > 0, max(sporilating_gp) ], 3)
  expect_true(all(test2[["paddock"]][, sporilating_gp + noninfected_gp] == 40))
  # expect_silent(test2 <- make_some_infective(spore_packet = sp1,
  #                                            daily_vals = daily_values))
  # expect_equal(test2[["paddock"]][x == sp1["x"] &
  #                                   y == sp1["y"], sporilating_gp], 2)
  # expect_equal(test2[["paddock"]][x == sp1["x"] &
  #                                   y == sp1["y"], noninfected_gp], 38)
  # test2[["paddock"]][x == sp1["x"] &
  #                      y == sp1["y"], cdd_at_infection := 10]
  # expect_silent(test2 <- make_some_infective(spore_packet = sp1,
  #                                            daily_vals = daily_values))
  # expect_equal(test2[["paddock"]][x == sp1["x"] &
  #                                   y == sp1["y"], sporilating_gp], 3)
  # expect_equal(test2[["paddock"]][x == sp1["x"] &
  #                                   y == sp1["y"], noninfected_gp], 37)
  # expect_is(test2[["paddock"]][, noninfected_gp], "numeric")
  # expect_is(test2[["paddock"]][, sporilating_gp], "numeric")
  # expect_false(any(is.na(test2[["paddock"]][, -("cdd_at_infection")])))
})

daily_values[["paddock"]][,cdd_at_infection := 300]

test3 <- make_some_infective(spore_packet = sp2,
                             daily_vals = daily_values)
test3[["paddock"]][sporilating_gp > 0,]
