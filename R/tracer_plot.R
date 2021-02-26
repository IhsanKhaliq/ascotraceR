#' plot one day from trace_asco output as a tile plot
#'
#' @param dat nested list of ascotraceR class
#' @param day integer, day of the simulation to plot
#' @param tiles what to response for the plot to render, options: sporilating_gp, new_gp,noninfected_gp
#'
#' @return ggplot
#' @export
#'
#' @examples
#' test5 <- trace_asco(
#'   weather = Ascotracer::newM_weather,
#'   paddock_length = 100,
#'   paddock_width = 100,
#'   initial_infection = "1998-06-10",
#'   sowing_date = as.POSIXct("1998-06-09"),
#'   harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(100),
#'   time_zone = "Australia/Perth",
#'   primary_infection_foci = "center"
#'
#'   tracer_plot(test5,102))


tracer_plot <- function(dat, day, tiles = "sporilating_gp"){
  dat1 <- dat[[day]][["paddock"]]


  if(tiles == "sporilating_gp"){
  p1 <- ggplot2::ggplot(data = dat1, aes(x,y))+
    geom_tile(aes(fill = sporilating_gp))+
    scale_fill_gradient(low = "lightgoldenrod", high = "red")
  print(p1)
  return(p1)}

  if(tiles == "noninfected_gp"){
    p1 <- ggplot2::ggplot(data = dat1, aes(x,y))+
      geom_tile(aes(fill = noninfected_gp))+
      scale_fill_gradient(low = "lightgoldenrod", high = "palegreen4")
    print(p1)
    return(p1)}
}
