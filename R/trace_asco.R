#' Simulates the spread of Ascochyta blight in a lupin field
#'
#' `trace_asco` simulates the spatiotemporal development of Ascochyta blight in a lupin field
#' over a growing season
#'
#' @param weather weather data, recorded by a local weather station, over a lupin
#' growing season for the model operation
#' @param paddock_length length of a paddock in metres (y)
#' @param paddock_width width of a paddock in metres (x)
#' @param sowing_date a character string of a date value indicating sowing
#'  date of lupin seed and the start of the ascotraceR model. Preferably
#'  in \acronym{ISO8601} format (YYYY-MM-DD), \emph{e.g.} \dQuote{2020-04-26}.
#'  Assumes there is sufficient soil moisture to induce germination and start the
#'  crop growing season.
#' @param harvest_date a character string of a date value indicating harvest date of
#' lupin crop, which is also the last day to run the ascotraceR model. Preferably in
#'  \acronym{ISO8601} format (YYYY-MM-DD), \emph{e.g.} \dQuote{2020-04-26}.
#' @param seeding_rate indicate the rate at which chickpea seed is sown per
#' square metre. Defaults to \code{40}
#' @param gp_rr refers to rate of increase in chickpea growing points
#' per degree Celsius per day. Defaults to \code{0.0065}
#' @param max_gp_lim Maximum number of chickpea growing points (meristems) allowed
#'  per square meter. Defaults to \code{15000}.
#' @param max_new_gp Maximum number of new chickpea growing points (meristems)
#'  which develop per day, per square meter. Defaults to \code{350}.
#' @param primary_infection_foci refers to the inoculated coordinates where the
#'  epidemic starts. Accepted inputs are: \code{"centre"} (Default), \code{random}
#'  a randomly selected coordinate in the paddock, a two column data.table of
#'  coordinates with colnames c("x","y"), a three column data.table where the third
#' @param primary_infection_intensity The intensity of the starting epidemic as
#'  described by the number of number of sporulating growing points.
#' @param latent_period_cdd latent period in cumulative degree days (sum of
#'  daily temperature means) is the period between infection and production of
#'  lesions on susceptible growing points. Defaults to \code{200}
#' @param initial_infection refers to initial or primary infection on seedlings,
#'  resulting in the production of infected growing points
#' @param time_zone refers to time in Coordinated Universal Time (UTC)
#' @param spores_per_gp_per_wet_hour Number of spores produced per sporulating growing point each wet hour.
#'   Also known as the 'spore_rate'. Value is dependent on the susceptibility of the host genotype.
#' @param n_foci only relevant when primary_infection_foci = "random" and notes the number
#'  of primary_infection_foci at initial infection.
#'
#' @return a x y `data.frame` simulating the spread of Ascochyta blight in a
#' chickpea paddock
#' @export
#'
#' @examples
#' # First weather data needs to be imported and formatted with `format_weather`
#' Newmarracarra <-
#'    read.csv(system.file("extdata",
#'             "1998_Newmarracarra_weather_table.csv", package = "ascotraceR"))
#' station_data <-
#'    system.file("extdata", "stat_dat.csv", package = "ascotraceR")
#'
#' weather_dat <- format_weather(
#'    x = Newmarracarra,
#'    POSIXct_time = "Local.Time",
#'    temp = "mean_daily_temp",
#'    ws = "ws",
#'    wd_sd = "wd_sd",
#'    rain = "rain_mm",
#'    wd = "wd",
#'    station = "Location",
#'    time_zone = "Australia/Perth",
#'    lonlat_file = station_data)
#'
#'
#' traced <- trace_asco(
#'   weather = weather_dat,
#'   paddock_length = 100,
#'   paddock_width = 100,
#'   initial_infection = "1998-06-10",
#'   sowing_date = as.POSIXct("1998-06-09"),
#'   harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(70),
#'   time_zone = "Australia/Perth",
#'   gp_rr = 0.0065,
#'   primary_infection_intensity = 1000,
#'   spores_per_gp_per_wet_hour = 0.22,
#'   primary_infection_foci = "centre")
#'   traced[[70]]
#'
#' write.csv(traced[[70]]$paddock, "testrun2.csv", row.names = FALSE)
trace_asco <- function(weather,
                       paddock_length,
                       paddock_width,
                       sowing_date,
                       harvest_date,
                       initial_infection,
                       seeding_rate = 40,
                       gp_rr = 0.0065,
                       max_gp_lim = 15000,
                       max_new_gp = 350,
                       latent_period_cdd = 200,
                       time_zone = "UTC",
                       primary_infection_foci = "random",
                       primary_infection_intensity = 1,
                       n_foci = 1,
                       spores_per_gp_per_wet_hour = 0.22){


  x <- y <- load <- susceptible_gp <- NULL

  # check date inputs for validity -----------------------------------------
  .vali_date <- function(x) {
    tryCatch(
      # try to parse the date format using lubridate
      x <- lubridate::parse_date_time(x,
                                      c(
                                        "Ymd",
                                        "dmY",
                                        "mdY",
                                        "BdY",
                                        "Bdy",
                                        "bdY",
                                        "bdy"
                                      )),
      warning = function(c) {
        stop(call. = FALSE,
             "\n",
             x,
             " is not a valid entry for date. Enter as YYYY-MM-DD.\n")
      }
    )
  return(x)
  }


  # if (primary_infection_intensity > seeding_rate) {
  #   stop(
  #     "primary_infection_intensity exceeds the number of starting growing points - 'seeding_rate': ",
  #     seeding_rate
  #   )
  # }

  # convert times to POSIXct -----------------------------------------------
  initial_infection <-
    lubridate::ymd(.vali_date(initial_infection), tz = time_zone) + lubridate::dhours(0)

  sowing_date <-
    lubridate::ymd(.vali_date(sowing_date), tz = time_zone) + lubridate::dhours(0)

  harvest_date <-
    lubridate::ymd(.vali_date(harvest_date), tz = time_zone) + lubridate::dhours(23)

  # check epidemic start is after sowing date
  if(initial_infection <= sowing_date){
    stop("initial_infection occurs prior to sowing_date\n
         please submit an initial_infection date which occurs after crop_sowing")
  }


  # makePaddock equivalent
  paddock <- as.data.table(expand.grid(x = 1:paddock_width,
                                       y = 1:paddock_length))



  # sample a paddock location randomly if a starting foci is not given
  if (is.data.table(primary_infection_foci) &
      all(c("x", "y") %in% colnames(primary_infection_foci))) {
    # Skip the rest of the tests
  } else{
    if (class(primary_infection_foci) == "character") {
      if (primary_infection_foci == "random") {
        primary_infection_foci <-
          paddock[sample(seq_len(nrow(paddock)),
                         size = n_foci,
                         replace = TRUE),
                  c("x", "y")]

      } else{
        if (primary_infection_foci == "centre") {
          primary_infection_foci <-
            paddock[x == as.integer(round(paddock_width / 2)) &
                      y == as.integer(round(paddock_length / 2)),
                    c("x", "y")]
        }else{
          stop("primary_infection_foci input not recognised")
      }
      }
    } else{
      if (is.vector(primary_infection_foci)) {
        if (length(primary_infection_foci) != 2 |
            is.numeric(primary_infection_foci) == FALSE) {
          stop("primary_infection_foci should be supplied as a numeric vector of length two")
        }
        primary_infection_foci <-
          as.data.table(as.list(primary_infection_foci))
        setnames(x = primary_infection_foci,
                 old = c("V1", "V2"),
                 new = c("x", "y"))
      }
      if (is.data.table(primary_infection_foci) == FALSE &
          is.data.frame(primary_infection_foci)) {
        setDT(primary_infection_foci)
        if (all(c("x", "y") %in% colnames(primary_infection_foci)) == FALSE) {
          stop("primary_infection_foci data.table needs colnames 'x' and 'y'")
        }

      }

    }
  }


  infected_rows <- which_paddock_row(paddock = paddock,
                                     query = primary_infection_foci)
  if(ncol(primary_infection_foci) == 2){
    primary_infection_foci[,load := primary_infection_intensity]
  }else{
    if(all(colnames(primary_infection_foci) %in% c("x", "y"))){
      stop("colnames for 'primary_infection_foci' not 'x', 'y' & 'load'.")
    }
  }

  # define paddock variables at time 1
  #need to update so can assign a data.table of things primary infection foci!!!!!!!!!!!!!!!
  paddock[, c(
    "new_gp", # Change in the number of growing points since last iteration
    "susceptible_gp",
    "exposed_gp",
    "infectious_gp" # replacing InfectiveElementList
  ) :=
    list(
      seeding_rate,
      seeding_rate,
      0,
      0
    )]

  # calculate additional parameters
  # Pauls interpretation of this calculation
  # For a particular spread event (point in time), in space of all growing points
  #  the maximum number of susceptible growing are 15000/350 = 42.86
  #  The highest probability of a spore landing on the area of these 42 susceptible
  #  growing points is 0.00006 * 42.86. However as the crop is always changing we
  #  need to calculate the actual probability of interception depending on the
  #  density of the crop canopy for that given time. See the function `interception_probability`
  spore_interception_parameter <-
    0.00006 * (max_gp_lim/max_new_gp)

  # define max_gp
  max_gp <- max_gp_lim * (1 - exp(-0.138629 * seeding_rate))


  # Notes: as area is 1m x 1m many computation in the mathematica
  #  code are redundant because they are being multiplied by 1.
  #  I will reduce the number of objects containing the same value,
  #  Below is a list of Mathematica values consolidated into 1
  #
  # refUninfectiveGPs <- minGrowingPoints <- seeding_rate

  # Create a clean daily values list with no infection in paddocks
  daily_vals_list <- list(
    list(
      paddock = paddock, # data.table each row is a 1 x 1m coordinate
      i_date = sowing_date,  # day of the simulation (iterator)
      i_day = 1,
      day = lubridate::yday(sowing_date),    # day of the year
      cdd = 0,    # cumulative degree days
      cwh = 0,    # cumulative wet hours
      cr = 0,     # cumulative rainfall
      gp_standard = seeding_rate,     # standard number of growing points for 1m^2 if not inhibited by infection (refUninfectiveGrowingPoints)
      new_gp = seeding_rate,    # new number of growing points for current iteration (refNewGrowingPoints)
      infected_coords = data.table(x = numeric(),
                                   y = numeric()),  # data.table
      exposed_gps =  data.table(x = numeric(),
                                   y = numeric(),
                                   spores_per_packet = numeric(),
                                   cdd_at_infection = numeric()) # data.table of infected growing points still in latent period and not sporilating (exposed_gp)
    )
  )

  time_increments <- seq(sowing_date,
                         harvest_date,
                         by = "days")

  daily_vals_list <- rep(daily_vals_list,
                         length(time_increments)+1)

  for(i in seq_len(length(time_increments))){

    # update time values for iteration of loop
    daily_vals_list[[i]][["i_date"]] <- time_increments[i]
    daily_vals_list[[i]][["i_day"]] <- i
    daily_vals_list[[i]][["day"]] <- lubridate::yday(time_increments[i])


    # currently working on one_day
    daily_vals_list[[i + 1]] <- one_day(
      i_date = time_increments[i],
      daily_vals = daily_vals_list[[i]],
      weather_dat = weather,
      gp_rr = gp_rr,
      max_gp = max_gp,
      spore_interception_parameter = spore_interception_parameter,
      spores_per_gp_per_wet_hour = spores_per_gp_per_wet_hour
    )

    # When the time of initial infection occurs, infect the paddock coordinates
    if(initial_infection == time_increments[i]){

      # if primary_infection_intensity exceeds the number of growing points send
      #  warning
      if (primary_infection_intensity > daily_vals_list[[i]][["gp_standard"]]) {
        warning(
          "primary_infection_intensity exceeds the number of growing points at time of infection 'growing_points': ",
          daily_vals_list[[i]][["gp_standard"]],
          "\nThis may cause an over estimation of disease spread"
        )
      }


    # update the remaining increments with the primary infected coordinates
    daily_vals_list[i:length(daily_vals_list)] <-
      lapply(daily_vals_list[i:length(daily_vals_list)], function(dl){

        # Infecting paddock
        pad1 <- data.table::copy(dl[["paddock"]])
        pad1[infected_rows,
             c("susceptible_gp",
               "infectious_gp") :=
               list(susceptible_gp - primary_infection_foci[, load],
                 primary_infection_foci[, load])]
        dl[["paddock"]] <- pad1

        # Edit infected_coordinates data.table
        dl[["infected_coords"]] <- primary_infection_foci[,c("x","y")]
      return(dl)
    })

    }
  }

  daily_vals_list[[length(daily_vals_list)]][["i_date"]] <-
    daily_vals_list[[length(daily_vals_list)]][["i_date"]] + lubridate::ddays(1)
  daily_vals_list[[length(daily_vals_list)]][["i_day"]] <- length(daily_vals_list)
  daily_vals_list[[length(daily_vals_list)]][["day"]] <-
    lubridate::yday(daily_vals_list[[length(daily_vals_list)]][["i_date"]])


  return(daily_vals_list)
}
