#' get Animal
#'
#' @description reads anmimal file from animal root
#'
#'
#' @author Jannis Gottwald
#'
#'
#' @param projroot string, project directory
#' @param animalID string, label of the tagged individual
#'
#'
#' @export
#'
#' @examples
#' #projroot<-paste0(getwd(),"/tRackIT_test_data/")
#' #getAnimal(projroot = projroot, animalID = "woodpecker")
#'


getAnimal <- function(projroot = ".", animalID) {
  
  
  #error handling
  if (projroot == ".") {
    stop("No root directory provided")
  }
  if (file.exists(paste0(projroot, "/data/individuals/", animalID, "/", animalID, "_idFile.rds")) == FALSE) {
    stop(paste0("There is no animalID file called: ", paste0(projroot, "/data/individuals/", animalID, "/", animalID, "_idFile.rds. "), "Use tRackIT::initAnimal() to create a new animalID file"))
  }

  animal <- readRDS(paste0(projroot, "/data/individuals/", animalID, "/", animalID, "_idFile.rds"))
  
  #adjust filepath
  animal$path <- lapply(animal$path, function(x) paste0(projroot, x))
  
  return(animal)
}
