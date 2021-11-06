#' @import data.table
NULL

# This function is never called.
# It only makes `sf` available in the package for `lutz` to use.
# `lutz` only has `sf` in Suggests, but it is needed for the function we use
# from `lutz`

stub <- function() {
  sf::st_as_sf() # nocov
}
