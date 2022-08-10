#' get radio-tracking project
#'
#' @description reads project file from projroot
#'
#'
#' @author Jannis Gottwald
#'
#' @param projroot string, project directory
#' @param plot logical, if TRUE a map with stations will be plottet
#'
#'
#' @export
#'
#' @examples
#' # get project
#' #projroot<-paste0(getwd(),"/tRackIT_test_data/")
#' #test_project <- getProject(projroot = projroot, plot = FALSE)
#'
getProject <- function(projroot = ".", plot = FALSE) {
  if (projroot == ".") {
    stop("No root directory provided")
  }

  if (file.exists(paste0(projroot, basename(projroot), "_projectFile.rds")) == FALSE) {
    stop(paste0("There is no project file called: ", paste0(projroot, basename(projroot), "_projectFile.rds. ", "Use tRackIT::initProject() to create a new project file")))
  }



  project <- readRDS(paste0(projroot, basename(projroot), "_projectFile.rds"))




  b <- projroot
  # complete path
  project$path <- lapply(project$path, function(x) paste0(b, x))

  # plot map
  if (plot == TRUE) {
    stations <- project$stations
    epsg <- project$epsg
    sp::coordinates(stations) <- c("X", "Y")
    sp::proj4string(stations) <- sp::CRS(paste0("+init=epsg:", epsg))

    print(mapview::mapview(stations))
  }

  return(project)
}
