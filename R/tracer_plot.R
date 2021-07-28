#' plot one day from trace_asco output as a tile plot
#'
#' @param dat nested list of ascotraceR class
#' @param day integer, day of the simulation to plot
#' @param tiles what to response for the plot to render, options: sporulating_gp, new_gp,noninfected_gp
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' # First weather data needs to be imported and formated with `format_weather`
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
#'   tracer_plot(traced,102)


tracer_plot <- function(dat, day, tiles = "sporulating_gp") {
  x <- y <- sporulating_gp <- noninfected_gp <- NULL

  dat1 <- dat[[day]][["paddock"]]



  if (tiles == "sporulating_gp") {
    p1 <- ggplot2::ggplot(data = dat1, ggplot2::aes(x, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = sporulating_gp)) +
      ggplot2::scale_fill_gradient(low = "lightgoldenrod", high = "red")
    print(p1)
    return(p1)
  }

  if (tiles == "noninfected_gp") {
    p1 <- ggplot2::ggplot(data = dat1, ggplot2::aes(x, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = noninfected_gp)) +
      ggplot2::scale_fill_gradient(low = "lightgoldenrod", high = "palegreen4")
    print(p1)
    return(p1)
  }

  if (tiles == "percent_gp_sporulating") {
    p1 <- ggplot2::ggplot(data = dat1, ggplot2::aes(x, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = sporulating_gp / (sporulating_gp + noninfected_gp))) +
      ggplot2::scale_fill_gradient(low = "lightgoldenrod", high = "red")
    print(p1)
    return(p1)
  }
}
