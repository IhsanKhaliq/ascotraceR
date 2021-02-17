#' Title
#'
#' @import data.table
#' @export
#'
#' DT_test <- function()
{
  dt <- data.table(acol=c("aA", "aB", "aC"),
                   bcol=c("bA", "bB", "bC"))
  dt[, ccol := c(1,2,3)]
  return(dt)
}
