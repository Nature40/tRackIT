#' Init Animal Tracking
#'
#' @description Creates directories and a R list with all the needed tRackIT information for one animal. R list is stored as .rds in animals directory-projroot/data/individuals/animalID/animalID_idFile.rds
#'
#'
#' @author Marvin Ludwig & Jannis Gottwald
#'
#'
#' @param projList list, generatet by initProject function
#' @param projroot string, project directory
#' @param animalID string, label of the tagged individual
#' @param species string, species of the tagged individual
#' @param sex string, sex of the tagged individual
#' @param age string, age of the tagged individual
#' @param weight string, weight of the tagged individual
#' @param rep.state string, reproductive state of the tagged individual
#' @param freq num, tag frequency (khz)
#' @param start string, start of tracking YYYY-MM-DD
#' @param end string, end of tracking YYYY-MM-DD
#' @param duration_min numeric, minimum duration of single signal
#' @param duration_max numeric, maximum duration of single signal
#'
#' @export
#'
#' @examples
#' # get project file
#' test_project <- getProject(projroot = projroot, plot = TRUE)
#' # init animal
#' initAnimal(projList = test_project, projroot = "test_project/", saveAnml = TRUE, animalID = test_project$tags$ID[1], species = "woodpecker", sex = "m", age = "adult", weight = 36, rep.state = "breeding", freq = test_project$tags$frequency[1], start = test_project$tags$start[1], end = test_project$tags$end[1], duration_min = 0.012, duration_max = 0.4)
#'
initAnimal <- function(projList = NULL,
                       projroot = ".",
                       saveAnml = FALSE,
                       animalID = NULL,
                       species = NA,
                       sex = NA,
                       age = NA,
                       weight = NA,
                       rep.state = NA,
                       freq = NA,
                       start = NA,
                       end = NA,
                       duration_min = NA,
                       duration_max = NA) {
  if (is.null(projList)) {
    stop("No project information provided (projList=Null). See ?initProject or ?getProject")
  }

  if (is.null(animalID)) {
    stop("No animalID provided")
  }

  if (is.na(freq)) {
    stop("No tag frequency provided (something like 150153)")
  }

  if (is.na(start)) {
    stop("No start date of the tagging period provided")
  }

  if (is.na(end)) {
    stop("No end date of the tagging period provided")
  }

  if (is.na(duration_min)) {
    stop("No expected minimum duration of signals is provided. This is important for signal filtering. Set it low (0) if you are unsure.")
  }

  if (is.na(duration_max)) {
    stop("No expected maximum duration of signals is provided. This is important for signal filtering. Set it high (100) if you are unsure.")
  }


  if (projroot == ".") {
    stop("No root directory provided")
  }


  if (class(projList) != class(list())) {
    stop("projList seems to be in the wrong format")
  }

  # create project sturture
  dir.create(paste0(projList$path$ids, animalID), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/filtered_awk"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/filtered"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/logger_timematch"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/station_timematch"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/imputed"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/calibrated"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/gps_timematch"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/bearings"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/triangulations"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/variables"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/bearings_filtered"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/classification"), showWarnings = FALSE)

  # save meta data
  meta <- list(
    animalID = as.character(animalID),
    species = as.character(species),
    sex = as.character(sex),
    age = as.character(age),
    weight = as.character(weight),
    rep.state = as.character(rep.state),
    freq = as.character(freq),
    start = as.character(start),
    end = as.character(end),
    duration_min = as.character(duration_min),
    duration_max = as.character(duration_max)
  )




  # get paths
  path <- list(
    raw = projList$path$csv,
    root = paste0(projList$path$ids, "/", animalID),
    filtered_awk = paste0(projList$path$ids, animalID, "/filtered_awk/"),
    filtered = paste0(projList$path$ids, animalID, "/filtered/"),
    logger_timematch = paste0(projList$path$ids, animalID, "/logger_timematch/"),
    station_timematch = paste0(projList$path$ids, animalID, "/station_timematch/"),
    imputed = paste0(projList$path$ids, animalID, "/imputed/"),
    calibrated = paste0(projList$path$ids, animalID, "/calibrated/"),
    gps_matched = paste0(projList$path$ids, animalID, "/gps_timematch/"),
    bearings = paste0(projList$path$ids, animalID, "/bearings/"),
    triangulations = paste0(projList$path$ids, animalID, "/triangulations/"),
    vars = paste0(projList$path$ids, animalID, "/variables/"),
    bearings_filtered = paste0(projList$path$ids, animalID, "/bearings_filtered/"),
    classification = paste0(projList$path$ids, animalID, "/classification/")
  )

  animal <- list(
    meta = meta,
    path = path
  )

  if (saveAnml == TRUE) {
    animal_file <- animal


    animal_file$path <- lapply(animal_file$path, function(x) gsub(projroot, "", x))

    saveRDS(animal_file, paste0(projList$path$ids, "/", animalID, "/", animalID, "_idFile.rds"))
  }

  return(animal)
}
