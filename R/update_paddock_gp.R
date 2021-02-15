#' Update chickpea growing points over a whole paddock
#'
#' @param paddock_coords a data.frame of paddock coordinates containing a column variable "noninfected_gp
#' @param mean_air_temp mean daily air temperature
#' @param gp_rr growing points replication rate
#' @param max_gp maximum growing points
#'
#' @return a vector of updated growing points at each paddock location
#'
#' @examples
update_paddock_gp <-
  function(paddock_coords,
           mean_air_temp,
           gp_rr,
           max_gp) {

  paddock_gp <-
    apply(paddock_coords, 1, function(xy) {

    if (xy["noninfected_gp"] > -0.5) {
      updated_gp <-
        xy["noninfected_gp"] +
        calc_new_gp(xy["noninfected_gp"],
                               mean_air_temp,
                               gp_rr = gp_rr,
                               max_gp = max_gp)
      return(updated_gp)
    }
    return(xy["noninfected_gp"])
  })
  return(paddock_gp)
}
