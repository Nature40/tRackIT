#' distance to station
#'
#' @description calculates the distance of traingulations to the station with maxDB
#'
#'
#' @author Jannis Gottwald
#'
#'
#'
#' @param data data.frame, generatet by triangulation function
#' @param projList list, generatet by initProject function
#'
#'
#' @export
#'
#'




dist_to_stat <- function(data, projList) {
  data$Stat_X <- NA
  data$Stat_y <- NA

  stations <- projList$stations
  for (i in seq_len(data)) {
    data$Stat_Y[i] <- stations$Y[stations$station == data$maxStation[i]]
    data$Stat_X[i] <- stations$X[stations$station == data$maxStation[i]]
  }

  data$stat_dist <- raster::pointDistance(cbind(data$Stat_X, data$Stat_Y), cbind(data$x, data$y), lonlat = T)


  return(data)
}
