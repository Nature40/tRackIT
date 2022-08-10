#' Initialise a Radiotracking Project
#' @param projroot string, path to home directory of the project
#' @export
#'



rtRoot <- function(projroot = ".") {
  dirCreate <- function(x) {
    x <- file.path(projroot, x)
    if (!file.exists(x)) dir.create(x) else warning("Directory '", x, "' already exists.")
  }

  if (projroot != ".") dirCreate("")

  dirCreate("data")
}
