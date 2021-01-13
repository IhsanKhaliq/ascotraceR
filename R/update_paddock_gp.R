update_paddock_gp <- function(paddock_coords,
                  mean_air_temp){
  apply(paddock_coords, 1, function(xy){

    g_points <- paddockUnifectiveGrowingPoints[xy[1],xy[2]]

    if(g_points > -0.5){
      updated_gp <-
        g_points + calc_new_gp(g_points,
                               mean_air_temp,
                               gp_rr = gp_rr,
                               max_gp = max_gp)
      return(updated_gp)
    }
    return(g_points)
  })
}
