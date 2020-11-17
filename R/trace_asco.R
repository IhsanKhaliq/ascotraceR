#' Estimate the spread of ascochyta blight on chickpea
#'
#' `trace_asco` estimates the spatial spread through a chickpea crop
#'   during the growing season.
#'
#' @param weather weather data to inform the disease dynamics and crop
#'  maturity through the chickpea growing season.
#' @param paddock_length length of paddock in meters (x)
#' @param paddock_width width of paddock in meters (y)
#' @param sowing_date sowing date of chickpea seed and the start of the
#'  Ascochyta tracer model. Assumes there is sufficient soil moisture to
#'  induce germination
#' @param seedling_rate Chickpea plants per square meter, default 40
#' @param gp_rr Chickpea growing points (meristems) replication rate as a
#'  proportion of one per degree day.
#'
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
                       seedling_rate = 40,
                       gp_rr = 0.0065,
                       epidemic_foci = "random"){

  paddock <- expand.grid(x = 1:paddock_width,
                         y = 1:paddock_length)

  # sample a paddock location randomly if a starting foci is not given
  if(eppidemic_foci == "random") {
    eppidemic_foci <-
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

  daily_vals <- data.table::data.table(
    cdd = 0, # cumulative degree days
    cwh = 0, # cumulative wet hours
    cr = 0,  # cumulative rainfall
    i = 0,   # day of the simulation (iterator)
    day = 0  # day of the year
    )




  return(eppidemic_foci)
}
