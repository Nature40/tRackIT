#' time match logger data
#'
#' @description timematch of simoultaneously received signals by stations
#'
#'
#' @author Jannis Gottwald
#'
#'
#'
#' @param data data.frame, data containing vhf-signals
#' @param method string, which bearing calculation method ("linear", "acos", "ML")
#' @param tmstmp string, timestamp
#'
#'
#' @export
#'
#'


time_match_station <- function(data, method, tmstmp) {
  data$timestamp <- as.character(tmstmp)

  grid <- expand.grid(unique(data$station), unique(data$station))
  grid <- grid[grid$Var1 != grid$Var2, ]
  indx <- !duplicated(t(apply(grid, 1, sort))) # finds non - duplicates in sorted rows
  grid <- grid[indx, ] # selects only the non - duplicates according to that index

  df <- data.frame()
  for (i in seq_len(grid)) {
    tmp <- data[data$station == grid$Var1[i] | data$station == grid$Var2[i], ]
    dcst <- reshape2::dcast(tmp, timestamp ~ station, value.var = method, mean)
    dcst$name_s1 <- names(dcst)[2]
    dcst$name_s2 <- names(dcst)[3]
    colnames(dcst) <- c("timestamp", "s1", "s2", "name_s1", "name_s2")


    df <- rbind(df, dcst)
  }



  return(df)
}
