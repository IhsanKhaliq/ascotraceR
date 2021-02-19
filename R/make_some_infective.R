#' Make infected growing points sources for spore dispersal
#'
#' @param spore_packet atomic vector input through apply function containing
#'  xy coordinates and a column of spore_packets. example input is the
#'  newly_infected data.table
#' @param paddock
#' @param daily_vals list of the current day's values and paddock data.table
#'
#' @return updated daily_vals list
#'
#' @examples
function(spore_packet,
         paddock,
         daily_vals) {
  # save on time data filtering
  row_index <- daily_vals[["paddock"]][x == spore_packet["x"] &
                                         y == spore_packet["y"],
                                       which = TRUE]
  paddock_vals <- daily_vals[["paddock"]][row_index,]

  # This code should only occur on the first day of the model
  if (paddock_vals[, sporilating_gp] == 0 &
      is.na(paddock_vals[, ccd_at_infection])) {
    daily_vals[["paddock"]][row_index, sporilating_gp := 1]
  }


  if (paddock_vals[, noninfected_gp] < spore_packet["spores_per_packet"]) {
    infections_new <-
      random_integer_from_real(paddock_vals[, noninfected_gp])
    daily_vals[["paddock"]][row_index, noninfected_gp := 0]
  } else{
    infections_new <- spore_packet["spores_per_packet"]
    daily_vals[["paddock"]][row_index, noninfected_gp :=
                              paddock_vals[, noninfected_gp] - infections_new]
  }

  daily_vals[["paddock"]][row_index, sporilating_gp :=
                            daily_vals[["paddock"]][row_index, sporilating_gp] +
                            infections_new]

  return(daily_vals)

}
