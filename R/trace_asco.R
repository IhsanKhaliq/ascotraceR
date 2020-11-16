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
                       sowing_date){

  paddock <- expand.grid(x = 1:paddock_width,
                         y = 1:paddock_length)

  return(paddock)
}
