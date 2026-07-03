#' Returns the row numbers of an x y data.table
#'
#' @param paddock The template data.table you wish to know the row numbers of
#' @param query data.frame with column names 'x' and 'y' for which you want to
#'   know the row number of in the paddock data.table
#'
#' @return Vector of row numbers
#'
#' @examples
#' pdk <- CJ(x = 1:100,
#'           y = 1:100)
#' qry <- pdk[sample(1:nrow(pdk), 5), ]
#' which_paddock_row(paddock = pdk, query = qry)
#' @keywords internal
#' @noRd
which_paddock_row <- function(paddock, query) {
  x <- y <- NULL

  # Use a data.table join to find matching row numbers rather than deriving
  # them from a hand-rolled x/y formula. The previous formula assumed `x`
  # cycled fastest through `paddock`'s rows, but data.table::CJ() (which is
  # how `paddock` is built elsewhere in this package) actually cycles `y`
  # fastest -- so the formula silently returned the wrong row whenever a
  # query's x and y differed. A join is correct regardless of how `paddock`
  # happens to be ordered.
  query <- data.table::as.data.table(query)

  rows1 <- paddock[query, on = c("x", "y"), which = TRUE]

  return(rows1)
}
