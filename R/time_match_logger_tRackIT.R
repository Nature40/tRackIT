
#' time match logger data
#'
#' @description timematch of simoultaneously received signals by logger of one station
#'
#'
#' @author Jannis Gottwald
#'
#'
#'
#' @param animalID string, label of the tagged individual
#' @param path_to_data string, path to filtered data
#'
#' @export
#'
#' @examples
#' projroot<-paste0(getwd(),"/tRackIT_test_data/")
#' 
#' p <- getProject(projroot)
#' animal <- getAnimal(projroot, animalID = "woodpecker")
#' fls <- list.files(animal$path$filtered, full.names = TRUE)
#' lapply(fls, function(x) time_match_logger_tRackIT(animal, x))
#'
time_match_logger_tRackIT <- function(animal = NULL, path_to_data) {
  if (is.null(animal)) {
    stop("No animal file provided. Please see ?initAnimal or ?getAnimal")
  }


  if (!file.exists(path_to_data)) {
    stop(paste0("File: ", path_to_data, " does not exist"))
  }

  data <- data.table::fread(path_to_data)

  nms_actual <- colnames(data)

  nms_expected <- c("timestamp", "station", "receiver", "max")

  if (!all(nms_expected %in% nms_actual)) {
    idx <- nms_expected %in% nms_actual

    stop(paste0("Required column ", nms_expected[!idx], " not found! "))
  }


  data <- data[!is.na(data$max), ]

  # order by timestamp
  data <- data[order(data$timestamp), ]
  data$timestamp <- as.character(data$timestamp)

  # reshape

  data <- data.table::dcast(data, timestamp + station ~ receiver, value.var = "max", mean)

  # save
  if (nrow(data[!is.na(data$`0`), ]) > 10) {
    data$`0` <- imputeTS::na_locf(data$`0`, maxgap = 2)
  }
  if (nrow(data[!is.na(data$`1`), ]) > 10) {
    data$`1` <- imputeTS::na_locf(data$`1`, maxgap = 2)
  }
  if (nrow(data[!is.na(data$`2`), ]) > 10) {
    data$`2` <- imputeTS::na_locf(data$`2`, maxgap = 2)
  }
  if (nrow(data[!is.na(data$`3`), ]) > 10) {
    data$`3` <- imputeTS::na_locf(data$`3`, maxgap = 2)
  }


  if (!("0" %in% colnames(data))) {
    data$`0` <- NA
  }

  if (!("1" %in% colnames(data))) {
    data$`1` <- NA
  }

  if (!("2" %in% colnames(data))) {
    data$`2` <- NA
  }

  if (!("3" %in% colnames(data))) {
    data$`3` <- NA
  }
  data$timestamp <- as.character(data$timestamp)



  if (nrow(data) >= 10) {
    data.table::fwrite(data, paste0(animal$path$logger_timematch, "/", gsub(".csv", "", basename(path_to_data)), "_logger_time_match.csv"))
  }
}
