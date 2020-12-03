#' Wrapping actions - enabled (True) or disabled (False)
#'
#' wrap_width_rows = TRUE
#'
#' wrap_length_rows = TRUE
#'
#' When spores are dispersed, r. The destination subunit may be the same subunit
#' from which a spore has originated, or it may be another subunit within or outside
#' the field. When the destination subunit is located outside the field, the model
#' can be set to either of two options, to ignore it or to wrap it. When wrapping is chosen,
#'  the model simulates the equivalent of an infinite area made up of repeated copies of the
#'  field. When wrapping is not chosen, the model simulates a finite area isolated from
#'  other sources of disease
#'
#' @keywords internal
#' @noRd

wrap_addresses <- function(packet_list) {
  new_list <- list()
  for (i in index:length(packet_list)) {
    if (wrap_width_rows) {
      width_row <- packet_list[[index, 1, 1]] %% paddock_dimentions[[1]]
      if (width_row == 0) {
        widthRow <- paddock_dimentions[[1]]
      }
    } else{
      width_row <- packet_list[[index, 1, 1]]
    }


    if (wrap_length_rows) {
      length_row <- packet_list[[index, 1, 2]] %% paddock_dimentions[[2]]
      if (length_row == 0) {
        length_row <- paddock_dimentions[[2]]
      } else{
        length_row <- packet_list[[index, 1, 2]]
      }
    }


    if (1 <= width_row &
        width_row <= paddock_dimentions[[1]] &
        1 <= length_row &
        length_row <= paddock_dimentions[[2]]) {
      new_list <-
        c(new_list, c(width_row, length_row), packet_list[[index, 2]])
    } else{
      NULL
    }

  }
  return(new_list)
}
