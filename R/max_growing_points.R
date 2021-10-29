#' Maximum number of growing points chickpea plant can achieve
#'
#' Calculates maximum number of growing points chickpea plant can achieve. The
#' growth of chickpea is described in terms of increase in the number of growing
#' points. The number will vary depending on the cultivar resistance.
#'
#' @param per_area is given area (observation quadrat or a paddock) in metres
#' @param max_growing_points_limit Maximum number of chickpea growing points
#'   allowed per square metre. Defaults to `15000`.
#' @param min_growing_points are the minimum number of growing points. Defaults
#'   to `40`.
#' @return a numeric vector
#' @keywords internal
#' @noRd

max_growing_points <-
  function(max_growing_points_limit,
           min_growing_points) {
    per_area(max_growing_points_limit *
               (1 - exp(-0.138629 * min_growing_points)))
  }
