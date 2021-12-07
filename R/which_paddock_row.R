#' Returns the row numbers of an x y data frame
#'
#' @param paddock The template data frame you wish to know the row numbers of
#' @param query Data.frame with column names 'x' and 'y' for which you want to
#'   know the row number of in the paddock data.table
#'
#' @return Vector of row numbers
#'
#' @examples
#' pdk <- data.table(CJ(x = 1:100,
#'                      y = 1:100))
#' qry <- pdk[sample(1:nrow(pdk), 5),]
#' which_paddock_row(pdk, qry)
#' @keywords internal
#' @noRd
which_paddock_row <- function(paddock, query){
  x <- y <- NULL

  setDT(paddock)
  rows1 <-
    apply(query, 1, function(qu){

      y_max <- paddock[, max(y)]
      x_max <- paddock[, max(x)]

      x_rows <- qu["x"]
      y_rows <- (qu["y"] * x_max) - x_max

      return(x_rows + y_rows)

    })

  return(unlist(rows1))
}
