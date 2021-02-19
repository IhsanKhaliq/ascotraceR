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
         daily_vals){

  # save on time data filtering
  row_index <- daily_vals[["paddock"]][x == spore_packet["x"] &
                                         y == spore_packet["y"],
                                       which = TRUE]
  paddock_vals <- daily_vals[["paddock"]][row_index, ]

  # updates paddock$infective_element to indicate it is infective (replace makesomeinfective)
  if(paddock_vals[,sporilating_gp] == 0){
  daily_vals[["paddock"]][row_index, infective_element := 1]}

  if(daily_vals[["paddock"]][row_index, noninfected_gp] < spore_packet["spores_per_packet"]

  ){
    infections_new
  }


}
