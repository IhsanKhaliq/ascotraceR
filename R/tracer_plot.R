#' context("tracer_plot")
#' Plot trace_asco output
#'
#' @description Wrapper function which takes the output object of a trace_asco
#'   and plots a snapshot at a point in time (day) using ggplot::geom_tile(),
#'   effectively producing a heat-map
#'
#' @param dat nested list of `ascotraceR` class
#' @param day integer, day of the simulation to plot
#' @param tiles what to response for the plot to render, options:
#'   `infectious_gp`, `new_gp`, `susceptible_gp`.
#'
#' @return ggplot2 object
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
#' traced <- trace_asco(
#'   weather = weather_dat,
#'   paddock_length = 100,
#'   paddock_width = 100,
#'   initial_infection = "1998-06-10",
#'   sowing_date = as.POSIXct("1998-06-09"),
#'   harvest_date = as.POSIXct("1998-06-09") + lubridate::ddays(100),
#'   time_zone = "Australia/Perth",
#'   primary_infection_foci = "centre")
#'
#'   tracer_plot(traced, 102)
#' tracer_plot(traced,102) +
#'   ggplot2::scale_fill_gradient(low = "lightgoldenrod", high = "red")
#' tracer_plot(traced,102) +
#'   ggplot2::scale_fill_viridis_b()
#'
#' tracer_plot(traced,102, tiles = "susceptible_gp") +
#'   ggplot2::scale_fill_gradient(low = "lightgoldenrod", high = "palegreen4")
#' tracer_plot(traced,102, tiles = "percent_gp_sporulating") +
#'   ggplot2::scale_fill_gradient(low = "white", high = "black")
tracer_plot <- function(dat, day, tiles = "infectious_gp") {
  x <- y <- infectious_gp <- susceptible_gp <- NULL

  dat1 <- dat[[day]][["paddock"]]

  if (tiles == "infectious_gp") {
    p1 <- ggplot2::ggplot(data = dat1, ggplot2::aes(x, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = infectious_gp))
    print(p1)
    return(p1)
  }

  if (tiles == "susceptible_gp") {
    p1 <- ggplot2::ggplot(data = dat1, ggplot2::aes(x, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = susceptible_gp))
    print(p1)
    return(p1)
  }

  if (tiles == "percent_gp_sporulating") {
    p1 <- ggplot2::ggplot(data = dat1, ggplot2::aes(x, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = infectious_gp /
                                        sum(infectious_gp, susceptible_gp)))
    print(p1)
    return(p1)
  }
}
