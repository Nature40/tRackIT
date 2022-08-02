#' Init radio-tracking project
#'
#' @description Creates directories and a R list with all the needed tRackIT-Project information. R list is stored as .rds in projroot directory-projroot/projID_idFile.rds
#'
#'
#' @author Jannis Gottwald
#'
#' @param projroot string, project directory
#' @param tags data.frame, table containing all relevant information of tagged individuals
#' @param id_col string, name of the column in tags data.frame containing the name (ID) of the tagged individual
#' @param start_col string, name of the column in tags data.frame containing the date of the tagging (yy-mm-dd)
#' @param end_col string, name of the column in tags data.frame containing the end of the transmitter lifetime (yy-mm-dd)
#' @param freq_col string, name of the column in tags data.frame containing the frequency of the transmitter in kHz
#' @param dmin_col string, name of the column in tags data.frame containing the min duration of the transmitter signal in seconds (10ms=0.01 sec)
#' @param dmax_col string, name of the column in tags data.frame containing the max duration of the transmitter signal in seconds (10ms=0.01 sec)
#' @param logger_data_raw string, path to the full RTS dataset
#' @param stations data.frame, with station coordinates, station name, receiver name and antenna orientation
#' @param s_col string, name of the column in tags data.frame containing the name of each station
#' @param r_col string, name of the column in tags data.frame containing the name of each receiver
#' @param x_col string, name of the column in tags data.frame containing the X Coordinates
#' @param y_col string, name of the column in tags data.frame containing the Y Coordinates
#' @param o_col string, name of the column in tags data.frame containing the orientation of antennas
#' @param epsg numeric, epsg code of the coordinate system of stations coordinates- will be transformed to latlon
#' @param tz string, timezone of project
#' @param dmax_col string, name of the column in tags data.frame containing the max duration of the transmitter signal in seconds (10ms=0.01 sec)
#' @export
#' @examples
#' # data frame with id information
#' tag_df <- data.frame(id = "woodpecker", freq = 150050, start = "2021-06-10", end = "2021-06-14", dmax = 0.025, dmin = 0.012)
#' # data frame with tRackIt station information
#' stations <- data.frame(station = "station1", receiver = c(0, 1, 2, 3), orientation = c(0, 90, 180, 270), X = 8.677942, Y = 50.84420)
#' test_project <- initProject(projroot = "/test_project/", logger_data_raw = ".", tags = tag_df, id_col = "id", start_col = "start", end_col = "end", freq_col = "freq", dmin_col = "dmin", dmax_col = "dmax", stations = stations, s_col = "station", x_col = "X", y_col = "Y", r_col = "receiver", o_col = "orientation", epsg = 4326, tz = "CET")
#' tst$path
#'
initProject <- function(projroot = ".",
                        logger_data_raw,
                        tags = NULL,
                        id_col = NULL,
                        start_col = NULL,
                        end_col = NULL,
                        freq_col = NULL,
                        dmin_col = NULL,
                        dmax_col = NULL,
                        stations = NULL,
                        s_col,
                        x_col,
                        y_col,
                        r_col,
                        o_col,
                        epsg = NULL,
                        tz = NULL) {
  if (is.null(epsg)) {
    stop("A epsg code is mandatory. If you are unsure about the epsg code for your spatial reference system please check: https://spatialreference.org/ref/epsg/")
  }

  if (is.null(tz)) {
    stop("The timezone of your project needs to be provided (e.g.CET for central europe)")
  }

  if (projroot == ".") {
    stop("No root directory provided")
  }

  if (is.null(tags)) {
    stop("No tags data.frame provided")
  }

  if (is.null(stations)) {
    stop("No tags data.frame provided")
  }



  rtRoot(projroot)
  # create project sturture
  dir.create(paste0(projroot, "data/catalogues/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "data/logger_data_csv/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/reference_data/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/calibration_curves/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/correction_values/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/individuals/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/batch_awk"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/param_lst"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/models"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/R/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/results/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "R/scripts/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "R/fun/"), showWarnings = FALSE)

  # create path list
  path <- list(
    raw = logger_data_raw,
    catalogues = paste0(projroot, "data/catalogues/"),
    csv = paste0(projroot, "data/logger_data_csv/"),
    ref = paste0(projroot, "/data/reference_data/"),
    c_Curves = paste0(projroot, "/data/calibration_curves/"),
    correction = paste0(projroot, "/data/correction_values/"),
    ids = paste0(projroot, "/data/individuals/"),
    awk = paste0(projroot, "/data/batch_awk/"),
    param_lst = paste0(projroot, "/data/param_lst"),
    models = paste0(projroot, "/data/models/"),
    fun = paste0(projroot, "/R/fun/"),
    results = paste0(projroot, "/results/")
  )


  # prepare tag data
  tags <- as.data.frame(tags)
  tag_data <- data.frame(ID = tags[, id_col], start = tags[, start_col], end = tags[, end_col], frequency = tags[, freq_col], duration_min = tags[, dmin_col], duration_max = tags[, dmax_col])

  # check wether columns are missing and fill them
  nms <- c("ID", "start", "end", "frequency", "duration_min", "duration_max")
  nms_missing <- nms[which(!(nms %in% names(tag_data)))]

  if (length(nms_missing) > 0) {
    cat(paste("The following column is missing from tags and will be filled with NA values:", nms_missing))
  }

  for (n in nms_missing) {
    tag_data[, n] <- NA
  }

  # prepare station data
  stations <- as.data.frame(stations)
  stations <- data.frame(station = stations[, s_col], X = stations[, x_col], Y = stations[, y_col], receiver = stations[, r_col], orientation = stations[, o_col])

  # transform to lat lon
  sp::coordinates(stations) <- c("X", "Y")
  sp::proj4string(stations) <- sp::CRS(paste0("+init=epsg:", epsg))
  stations <- sp::spTransform(stations, sp::CRS(paste0("+init=epsg:", 4326)))

  stations <- as.data.frame(stations)

  # check wether columns are missing
  nms <- c("station", "X", "Y", "receiver", "orientation")
  nms_missing <- nms[which(!(nms %in% names(stations)))]

  if (length(nms_missing) > 0) {
    cat(paste("The following columns are missing from stations and will be filled with NA values:", nms_missing))
  }
  for (n in nms_missing) {
    stations[, n] <- NA
  }

  # create project list
  project <- list(path = path, tags = tag_data, stations = stations, epsg = epsg, tz = tz)

  # save project_file for exchange
  project_file <- project
  project_file$path <- lapply(project_file$path, function(x) gsub(projroot, "", x))
  saveRDS(project_file, paste0(projroot, "/", basename(projroot), "_projectFile.rds"))

  return(project)
}
