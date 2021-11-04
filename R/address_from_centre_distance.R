#' Destination address from source address
#'
#' Utilises a source coordinate, a distance vector and angle, then returns the
#' destination coordinate
#'
#' @param start_address Starting address of spores dispersal
#' @param offset_distance The offset distance of the actual distance spores
#'   travel
#' @keywords internal
#' @noRd

address_from_centre_distance <-
  function(offset_distance, start_address) {
    destination <- as.integer(c(sum(start_address[1],
                                    floor(
                                      sum(0.5, offset_distance[1])
                                    )),
                                sum(start_address[2],
                                    floor(
                                      sum(0.5, offset_distance[2])
                                    ))))
    if (any(is.na(destination))) {
      stop(
        call. = FALSE,
        "`address_from_centre_distance()` is returning `NA`; ",
        "check input coordinates\n",
        "offset_distance: ",
        offset_distance[1],
        ", ",
        offset_distance[2],
        "; start_address: ",
        start_address[1],
        ", ",
        start_address[2]
      )
    }

    # make destination a data.table
    destination <- data.table(x = destination[1],
                              y = destination[2])
    return(destination)
  }
