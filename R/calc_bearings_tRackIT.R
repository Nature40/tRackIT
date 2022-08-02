
#' bearing calculation
#'
#' @description calculates bearings using linear approximation and acos. 0,90.180,270 degrees Antenna orinetations are currently expected
#'
#'
#' @author Jannis Gottwald
#'
#'
#' @param projList list, generatet by initProject()
#' @param animal list, generatet by initAnimal()
#' @param path_to_data string, path to filtered file
#' @param dbLoss num, gain loss from 0 to 90° antenna orientation
#'
#' @export
#'
#' @examples
#'
#' projroot <- "/test_project/"
#' p <- getProject(projroot)
#' animal <- getAnimal(projroot, animalID = "woodpecker")
#' m_logger <- list.files(animal$path$logger_timematch, full.names = TRUE)
#' lapply(m_logger, function(x) calc_bearings_tRackIT(x, dbLoss = 21, animal = animal, projList = p))
#'
calc_bearings_tRackIT <- function(path_to_data, dbLoss, animal, projList) {
  
  
  #error handling
  if (!file.exists(path_to_data)) {
    stop(paste0("File: ", path_to_data, " does not exist"))
  }


  orientations_actual <- unique(projList$stations$orientation)
  orientations_expected <- c(0, 90, 180, 270)
  
  if (!all(orientations_expected %in% orientations_actual)) {
    idx <- nms_expected %in% nms_actual
    stop(paste0("The Bearing calculation currently expects Antenna orientations of 0, 90, 180, 270 degrees"))
  }


  data <- data.table::fread(path_to_data, header = TRUE)

  nms_actual <- colnames(data)
  nms_expected <- c("timestamp", "station", "0", "1", "2", "3")
  if (!all(nms_expected %in% nms_actual)) {
    idx <- nms_expected %in% nms_actual
    stop(paste0("Required column ", nms_expected[!idx], " not found! "))
  }

  station <- projList$stations[projList$stations$station == data$station[1], ]

  names(data)[names(data) == 0] <- as.character(station$orientation[station$receiver == 0])
  names(data)[names(data) == 1] <- as.character(station$orientation[station$receiver == 1])
  names(data)[names(data) == 2] <- as.character(station$orientation[station$receiver == 2])
  names(data)[names(data) == 3] <- as.character(station$orientation[station$receiver == 3])
orientations <- as.character(c(station$orientation))

  data <- as.data.frame(data)

  # identify lead antenna
  l1 <- (apply(data[, orientations], 1, maxn(1))) - 1
  l1[l1 == 0] <- orientations[1]
  l1[l1 == 1] <- orientations[2]
  l1[l1 == 2] <- orientations[3]
  l1[l1 == 3] <- orientations[4]
  # identify second antenna
  l2 <- (apply(data[, orientations], 1, maxn(2))) - 1
  l2[l2 == 0] <- orientations[1]
  l2[l2 == 1] <- orientations[2]
  l2[l2 == 2] <- orientations[3]
  l2[l2 == 3] <- orientations[4]
  # identify third antenna
  l3 <- (apply(data[, orientations], 1, maxn(3))) - 1
  l3[l3 == 0] <- orientations[1]
  l3[l3 == 1] <- orientations[2]
  l3[l3 == 2] <- orientations[3]
  l3[l3 == 3] <- orientations[4]

  data$lead <- l1
  data$second_antenna <- l2
  data$third_antenna <- l3


  # bearings lead=0 second=270
  
  L0 <- data[data$lead == 0 & data$second_antenna == 270, ]
  L0_b <- data[data$lead == 0 & data$second_antenna == 180 & data$third_antenna == 270, ]
  L0 <- rbind(L0, L0_b)
  if (nrow(L0) > 0) {
    # for angle a/LEFT of lead
    l <- 270
    r <- 0
    alpha <- angle_between(l, r)
    L0$delta <- (L0$"270" - L0$`0`) / dbLoss
    L0$linear <- 1 / 2 * (alpha - alpha * L0$delta / (sin(pi * alpha / 180)^2)) + l
    L0$acos <- acos(L0$delta) * 90 / pi + l
  } else {
    (L0 <- data.frame())
  }

  # bearings lead= 0 second=90
  
  R0 <- data[data$lead == 0 & data$second_antenna == 90, ]
  R0_b <- data[data$lead == 0 & data$second_antenna == 180 & data$third_antenna == 90, ]
  R0 <- rbind(R0, R0_b)
  if (nrow(R0) > 0) {
    l <- 0
    r <- 90
    alpha <- angle_between(l, r)
    R0$delta <- (R0$`0` - R0$`90`) / dbLoss
    R0$linear <- 1 / 2 * (alpha - alpha * R0$delta / (sin(pi * alpha / 180)^2))
    R0$acos <- acos(R0$delta) * 90 / pi
  } else {
    R0 <- data.frame()
  }

  # bearings lead=90 second=0
  
  L1 <- data[data$lead == 90 & data$second_antenna == 0, ]
  L1_b <- data[data$lead == 90 & data$second_antenna == 270 & data$third_antenna == 0, ]
  L1 <- rbind(L1, L1_b)
  if (nrow(L1) > 0) {
    l <- 0
    r <- 90
    alpha <- angle_between(l, r)
    L1$delta <- (L1$`0` - L1$`90`) / dbLoss
    L1$linear <- 1 / 2 * (alpha - alpha * L1$delta / (sin(pi * alpha / 180)^2)) + l
    L1$acos <- acos(L1$delta) * 90 / pi + l
  } else {
    L1 <- data.frame()
  }

  # bearings lead=90 second=180
  
  R1 <- data[data$lead == 90 & data$second_antenna == 180, ]
  R1_b <- data[data$lead == 90 & data$second_antenna == 270 & data$third_antenna == 180, ]
  R1 <- rbind(R1, R1_b)

  if (nrow(R1) > 0) {
    l <- 90
    r <- 180
    alpha <- angle_between(l, r)
    R1$delta <- (R1$`90` - R1$`180`) / dbLoss
    R1$linear <- 1 / 2 * (alpha - alpha * R1$delta / (sin(pi * alpha / 180)^2)) + l
    R1$acos <- acos(R1$delta) * 90 / pi + l
  } else {
    R1 <- data.frame()
  }

  # bearings lead=180 second=90
  L2 <- data[data$lead == 180 & data$second_antenna == 90, ]
  L2_b <- data[data$lead == 180 & data$second_antenna == 0 & data$third_antenna == 90, ]
  L2 <- rbind(L2, L2_b)
  if (nrow(L2) > 0) {
    l <- 90
    r <- 180
    alpha <- angle_between(l, r)
    L2$delta <- (L2$`90` - L2$`180`) / dbLoss
    L2$linear <- 1 / 2 * (alpha - alpha * L2$delta / (sin(pi * alpha / 180)^2)) + l
    L2$acos <- acos(L2$delta) * 90 / pi + l
  } else {
    L2 <- data.frame()
  }

  # bearings lead=180 second=270
  R2 <- data[data$lead == 180 & data$second_antenna == 270, ]
  R2_b <- data[data$lead == 180 & data$second_antenna == 0 & data$third_antenna == 270, ]
  R2 <- rbind(R2, R2_b)

  if (nrow(R2) > 0) {
    l <- 180
    r <- 270
    alpha <- angle_between(l, r)
    R2$delta <- (R2$`180` - R2$`270`) / dbLoss
    R2$linear <- 1 / 2 * (alpha - alpha * R2$delta / (sin(pi * alpha / 180)^2)) + l
    R2$acos <- acos(R2$delta) * 90 / pi + l
  } else {
    R2 <- data.frame()
  }

  # bearings lead=270 second=180
  L3 <- data[data$lead == 270 & data$second_antenna == 180, ]
  L3_b <- data[data$lead == 270 & data$second_antenna == 90 & data$third_antenna == 180, ]
  L3 <- rbind(L3, L3_b)
  if (nrow(L3) > 0) {
    l <- 180
    r <- 270
    alpha <- angle_between(l, r)
    L3$delta <- (L3$`180` - L3$`270`) / dbLoss
    L3$linear <- 1 / 2 * (alpha - alpha * L3$delta / (sin(pi * alpha / 180)^2)) + l
    L3$acos <- acos(L3$delta) * 90 / pi + l
  } else {
    L3 <- data.frame()
  }

  # bearings lead=270 second=0
  R3 <- data[data$lead == 270 & data$second_antenna == 0, ]
  R3_b <- data[data$lead == 270 & data$second_antenna == 90 & data$third_antenna == 0, ]
  R3 <- rbind(R3, R3_b)
  if (nrow(R3) > 0) {
    l <- 270
    r <- 0
    alpha <- angle_between(l, r)
    R3$delta <- (R3$`270` - R3$`0`) / dbLoss
    R3$linear <- 1 / 2 * (alpha - alpha * R3$delta / (sin(pi * alpha / 180)^2)) + l
    R3$acos <- acos(R3$delta) * 90 / pi + l
  } else {
    R3 <- data.frame()
  }

  data <- rbind(L0, R0, L1, R1, L2, R2, L3, R3)

  data.table::fwrite(data, paste0(animal$path$bearings, "/", gsub(".csv", "", basename(path_to_data)), "_bearings.csv"))
}

maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

angle_between <- function(angle_a, angle_b) {
  ((((angle_b - angle_a) %% 360) + 540) %% 360) - 180
}
