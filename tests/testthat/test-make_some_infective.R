context("makes some infected growing points infective or a source of innoculum")

newly_infected_list <- fread(file = "tests/testthat/data-newly_infected_list.csv")

# create data and parameters
seeding_rate <- 40
paddock <- as.data.table(expand.grid(x = 1:100,
                                     y = 1:100))
paddock[, c("new_gp",
            # Change in the number of growing points since last iteration
            "noninfected_gp",
            "infected_gp",
            "sporilating_gp",
            # replacing InfectiveElementList
            "ccd_at_infection") :=
          list(
            seeding_rate,
            seeding_rate,
            fifelse(x >= 53 &
                      x <= 57 &
                      y >= 53 &
                      y <= 57, 5,
                    0),
            0,
            NA
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

sp1 <- c(30, 10, 11)
names(sp1) <- c("x", "y", "spores_per_packet")


test1 <- make_some_infective(spore_packet = sp1,
                             daily_vals = daily_values)

test1[["paddock"]][sporilating_gp > 0,]
