#' Simulates the spread of Ascochyta blight in a chickpea field
#'
#' `trace_asco` simulates the spatiotemporal development of Ascochyta blight in a chickpea field
#' over a growing season
#'
#' @param weather weather data, recorded by a local weather station, over a chickpea
#' growing season for the model operation
#' @param paddock_length length of a paddock in metres (y)
#' @param paddock_width width of a paddock in metres (x)
#' @param sowing_date indicates the date at which chickpea crop is sown, which is also the
#' the start date of the Ascotracer model operation. Date value should preferably be
#'  in \acronym{ISO8601} format (YYYY-MM-DD), \emph{e.g.} \dQuote{2020-06-27}.
#' @param harvest_date indicates the date at which chickpea crop is harvested,
#'  which is also the last day of the Ascotracer model operation. Date values should
#'  preferably be in
#'  \acronym{ISO8601} format (YYYY-MM-DD), \emph{e.g.} \dQuote{2020-10-27}.
#' @param seeding_rate indicate the rate at which chickpea seed is sown per
#' square metre. Defaults to \code{40}
#' @param gp_rr refers to rate of increase in chickpea growing points
#' per degree Celsius per day. Defaults to \code{0.0065}
#' @param epidemic_foci vector of two integers ("x" and "y") indicating the
#'  paddock coordinates which will serve as the initial infection site, and
#'  from which the epidemic will spread. Defaults to \code{"random"}, which
#'  chooses coordinates at random.
#' @param latent_period_cdd Latent period in cumulative degree days (sum of
#'  daily temperature means) between infection and production of lesions on a
#'  susceptible growing. Defaults to \code{200}
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
                       seeding_rate = 40,
                       gp_rr = 0.0065,
                       epidemic_foci = "random",
                       latent_period_cdd = 200){

  # Time and date checks


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
    cdd = 0, # cumulative degree days
    cwh = 0, # cumulative wet hours
    cr = 0,  # cumulative rainfall
    i = 0,   # day of the simulation (iterator)
    day = 0  # day of the year
    )

  time_increments <- seq(sowing_date,
                         harvest_date,
                         by = "days")

  for(i in time_increments){

    # skip time increment if epidemic_start is after the sowing date
    if(i < epidemic_start) next

    # This function or line of code is redundant given this model works
    #  on a 1x1m grid and we do not want to wrap address
    # additional_new_infections <- packets_from_locations(location_list = epidemic_foci)

    # currently working on oneday
    one_day(day = i,
           daily_vals = daily_vals_dt)

  }




  return(time_increments)
}
