#' Maximum number of growing points chickpea plant achieved
#'
#' 'max_growing_points()' calculates maximum number of growing points chickpea plant
#' achived. The growth of chickpea is described in terms of number of growing points
#'
#' @param per_area is given area (observatoin quadrat or paddock) in metres
#' @param max_growing_points_limit is the limit on number of growing points chickpea
#' will ever achieve. It depends on chickpa cultivar
#' @param min_growing_points are the minimum number of growing points
#' @return a *numeric* vector



max_growing_points <- function(){
  per_area(max_growing_points_limit * (1 - exp(-0.138629 * min_growing_points)))
}
