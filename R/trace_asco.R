#' Simulates the spread of Ascochyta blight in a chickpea field
#'
#' `trace_asco` simulates the spatiotemporal development of Ascochyta blight in a chickpea field
#' over a growing season
#'
#' @param weather weather data, recorded by a local weather station, over a chickpea
#' growing season for the model operation
#' @param paddock_length length of a paddock in metres (y)
#' @param paddock_width width of a paddock in metres (x)
#' @param sowing_date a character string of a date value indicating sowing
#'  date of chickpea seed and the start of the Ascochyta tracer model. Preferably
#'  in \acronym{ISO8601} format (YYYY-MM-DD), \emph{e.g.} \dQuote{2020-04-26}.
#'  Assumes there is sufficient soil moisture to induce germination and start the
#'  crop growing season.
#' @param harvest_date a character string of a date value indicating crop maturity
#'  and the last day to run the Ascochyta tracer model. Preferably in
#'  \acronym{ISO8601} format (YYYY-MM-DD), \emph{e.g.} \dQuote{2020-04-26}.
#' @param seeding_rate indicate the rate at which chickpea seed is sown per
#' square metre. Defaults to \code{40}
#' @param gp_rr refers to rate of increase in chickpea growing points
#' per degree Celsius per day. Defaults to \code{0.0065}
#' @param max_gp Maximum number of chickpea growing points (meristems) allowed
#'  per square meter. Defaults to \code{15000}.
#' @param max_new_gp Maximum number of new chickpea growing points (meristems)
#'  which develop per day, per square meter. Defaults to \code{350}.
#' @param primary_infection_foci it refers to the inoculated quadrat
#' located at the centre of the paddock from where disease spreads
#' Defaults to \code{"centre"}
#' @param latent_period_cdd latent period in cumulative degree days (sum of
#'  daily temperature means) is the period between infection and production of
#'  lesions on susceptible growing points. Defaults to \code{200}
#'
#' @return a x y `data.frame` simulating the spread of Ascochyta blight in a
#' chickpea paddock
#' @export
#'
#' @examples
#' ta1 <- trace_asco(
#'   weather = weather_dat,
#'   paddock_length = 100,
#'   paddock_width = 100,
#'   sowing_date = "1998-03-09"
#'   )
trace_asco <- function(weather,
                       paddock_length,
                       paddock_width,
                       sowing_date,
                       harvest_date,
                       initial_infection,
                       seeding_rate = 40,
                       gp_rr = 0.0065,
                       max_gp = 15000,
                       max_new_gp = 350,
                       min_gp_for_half_limit = 5, # needs a new name
                       latent_period_cdd = 200,
                       time_zone = "UTC",
                       spore_interception_multiplier = 0.00006,
                       primary_infection_foci = "centre"
                       ){


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

  paddock[,new_gp := seeding_rate]
  paddock[,noninfected_gp := seeding_rate]
  paddock[,infected_gp := NA] # Needs to be updated!!!!

  # sample a paddock location randomly if a starting foci is not given
  if(primary_infection_foci == "random") {
    primary_infection_foci <-
      paddock[sample(seq_len(nrow(paddock)),
                               size = 1),
                        c("x", "y")]

  }
  if(primary_infection_foci == "center") {
    primary_infection_foci <-
      paddock[as.integer(round(paddock_width/2)),
              as.integer(round(paddock_length/2))]

  }


  # Notes: as area is 1m x 1m many computation in the mathematica
  #  code are redundant because they are being multiplied by 1.
  #  I will reduce the number of objects containing the same value,
  #  Below is a list of Mathematica values consolidated into 1
  #
  # refUninfectiveGPs <- minGrowingPoints <- seeding_rate

  daily_vals_list <- list(
    i = sowing_date,   # day of the simulation (iterator)
    day = lubridate::yday(sowing_date),  # day of the year
    cdd = 0, # cumulative degree days
    cwh = 0, # cumulative wet hours
    cr = 0,  # cumulative rainfall
    gp_standard = seeding_rate, # standard number of growing points for 1m^2 if not inhibited by infection
    new_gp = seeding_rate, # new nunmber of growing points for current iteration
    infected_coords = epidemic_foci # data.frame
    )

  time_increments <- seq(sowing_date,
                         harvest_date,
                         by = "days")

  for(i in seq_along(time_increments)){

    # skip time increment if initial_infection is after the sowing date
    if(time_increments[i] < initial_infection) next

    # This function or line of code is redundant given this model works
    #  on a 1x1m grid and we do not want to wrap address
    # additional_new_infections <- packets_from_locations(location_list = epidemic_foci)


    # currently working on one_day
    day_out <- one_day(i_date = time_increments[i],
                       daily_vals = daily_vals_list,
                       weather_dat = weather,
                       gp_rr = gp_rr,
                       max_gp = max_gp,
                       paddock = paddock,
                       spore_interception_multiplier = spore_interception_multiplier)

    # temporary line of code to test building of daily_vals in loop
    daily_vals_list <- day_out

  }




  return(daily_vals_list)
}
