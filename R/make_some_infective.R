#' Make infected growing points sources for spore dispersal
#'
#' @param spore_packet data.table with three variables, 'x', 'y' and
#'   'spores_per_packet'
#' @param daily_vals list of the current day's values and paddock data.table
#' @param latent_period latent period in cumulative degree days (sum of daily
#'   temperature means) is the period between infection and production of
#'   lesions on susceptible growing points. Defaults to `200`.
#' @param cdd_at_infection cummulative degree days when infection occurred
#' @param susceptible_gp represent non-infected growing points
#' @param  infectious_gp represent sporulating lesions
#' @param  max_growing_points_limit Are the maximum number of growing points per
#'   square metre. efaults to \code{5000}
#' @param paddock data.table detailing the growing points and infections at each
#'
#'
#' @return updated daily_vals list
#' @keywords internal
#' @noRd

make_some_infective <- function(daily_vals,
                                latent_period = 200) {
  cdd_at_infection <- x <- y <- susceptible_gp <- spores_per_packet <-
    infectious_gp <-
    max_growing_points_limit <- min_growing_points <- NULL

  newly_exposed <- daily_vals[["exposed_gps"]]

  newly_infectious <-
    newly_exposed[cdd_at_infection + latent_period <= daily_vals[["cdd"]],]


  for (i_row in seq_len(NROW(newly_infectious))) {
    # save on time data filtering
    row_index <-
      daily_vals[["paddock"]][x == newly_infectious[i_row, x] &
                                y == newly_infectious[i_row, y],
                              which = TRUE]
    paddock_vals <- daily_vals[["paddock"]][row_index,]


    if (paddock_vals[, susceptible_gp] <
        newly_infectious[i_row, spores_per_packet]) {
      infections_new <-
        random_integer_from_real(paddock_vals[, susceptible_gp])
      daily_vals[["paddock"]][row_index, susceptible_gp := 0]
    } else{
      infections_new <- newly_infectious[i_row, spores_per_packet]
      daily_vals[["paddock"]][row_index, susceptible_gp :=
                                paddock_vals[, susceptible_gp] - infections_new]
    }

    daily_vals[["paddock"]][row_index, infectious_gp :=
                              daily_vals[["paddock"]][row_index, infectious_gp] +
                              infections_new]

  }

  daily_vals[["exposed_gps"]] <-
    newly_exposed[cdd_at_infection + latent_period > daily_vals[["cdd"]],]

  # This line is here due to https://github.com/Rdatatable/data.table/issues/869
  daily_vals[["paddock"]]

  return(daily_vals)
}
