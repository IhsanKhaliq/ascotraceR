#' Estimate the spread of ascochyta blight on chickpea
#'
#' `trace_asco` estimates the spatial spread through a chickpea crop
#'   during the growing season.
#'
#' @param weather weather data to inform the disease dynamics and crop
#'  maturity through the chickpea growing season.
#' @param paddock_length length of paddock in meters (y)
#' @param paddock_width width of paddock in meters (x)
#' @param sowing_date a character string of a date value indicating sowing
#'  date of chickpea seed and the start of the Ascochyta tracer model. Preferably
#'  in \acronym{ISO8601} format (YYYY-MM-DD), \emph{e.g.} \dQuote{2020-04-26}.
#'  Assumes there is sufficient soil moisture to induce germination and start the
#'  crop growing season.
#' @param harvest_date a character string of a date value indicating crop maturity
#'  and the last day to run the Ascochyta tracer model. Preferably in
#'  \acronym{ISO8601} format (YYYY-MM-DD), \emph{e.g.} \dQuote{2020-04-26}.
#' @param seedling_rate Chickpea plants per square meter. Defaults to \code{40}
#' @param gp_rr Chickpea growing points (meristems) replication rate as a
#'  proportion of one per degree day. Defaults to \code{0.0065}.
#' @param max_gp Maximum number of chickpea growing points (meristems) allowed
#'  per square meter. Defaults to \code{15000}.
#' @param max_new_gp Maximum number of new chickpea growing points (meristems)
#'  which develop per day, per square meter. Defaults to \code{350}.
#' @param min_new_gp_for_half_limit NEEDS TO BE CLARIFIED Maximum number of new
#'  chickpea growing points (meristems) which develop per day, per square meter.
#'  Defaults to \code{5}.
#' @param epidemic_foci vector of two integers ("x" and "y") indicating the
#'  paddock coordinates which will serve as the initial infection site, and
#'  from which the epidemic will spread. Defaults to \code{"random"}, which
#'  chooses coordinates at random.
#' @param latent_period_cdd Latent period in cumulative degree days (sum of
#'  daily temperature means) between spores landing on a susceptible growing
#'  point and symptoms being observed. Defaults to \code{200}
#'
#' @return a x y `data.frame` providing the paddock coordinates and estimated
#'  severity of ascochyta at the respectic location
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
                       epidemic_start,
                       seedling_rate = 40,
                       gp_rr = 0.0065,
                       max_gp = 15000,
                       max_new_gp = 350,
                       min_gp_for_half_limit = 5, # needs a new name
                       epidemic_foci = "random",
                       latent_period_cdd = 200,
                       time_zone = "UTC"){

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
  epidemic_start <-
    lubridate::ymd(.vali_date(epidemic_start), tz = time_zone) + lubridate::dhours(0)

  sowing_date <-
    lubridate::ymd(.vali_date(sowing_date), tz = time_zone) + lubridate::dhours(0)

  harvest_date <-
    lubridate::ymd(.vali_date(harvest_date), tz = time_zone) + lubridate::dhours(23)

  # check epidemic start is after sowing date
  if(epidemic_start <= sowing_date){
    stop("eppidemic_start occurs prior to sowing_date\n
         please submit an epidemic_start date which occurs after crop_sowing")
  }


  # makePaddock equivalent
  paddock <- expand.grid(x = 1:paddock_width,
                         y = 1:paddock_length)

  # sample a paddock location randomly if a starting foci is not given
  if(epidemic_foci == "random") {
    epidemic_foci <-
      unlist(paddock[sample(seq_len(nrow(paddock)),
                               size = 1),
                        c("x", "y")])

  }

  # Notes: as area is 1m x 1m many computation in the mathematica
  #  code are redundant because they are being multiplied by 1.
  #  I will reduce the number of objects containing the same value,
  #  Below is a list of Mathematica values consolidated into 1
  #
  # refUninfectiveGPs <- minGrowingPoints <- seedling_rate

  daily_vals_dt <- data.table::data.table(
    i = sowing_date,   # day of the simulation (iterator)
    day = lubridate::yday(sowing_date),  # day of the year
    cdd = 0, # cumulative degree days
    cwh = 0, # cumulative wet hours
    cr = 0,  # cumulative rainfall
    gp = seedling_rate, # number of growing points in the current iteration
    noninfected_gp = seedling_rate
    )

  time_increments <- seq(sowing_date,
                         harvest_date,
                         by = "days")

  for(i in seq_along(time_increments)){

    # skip time increment if epidemic_start is after the sowing date
    if(time_increments[i] < epidemic_start) next

    # This function or line of code is redundant given this model works
    #  on a 1x1m grid and we do not want to wrap address
    # additional_new_infections <- packets_from_locations(location_list = epidemic_foci)


    # currently working on one_day
    day_out <- one_day(i_date = time_increments[i],
                       daily_vals = daily_vals_dt,
                       weather_dat = weather,
                       gp_rr = gp_rr)

    # temporary line of code to test building of daily_vals in loop
    daily_vals_dt <- day_out

  }




  return(daily_vals_dt)
}
