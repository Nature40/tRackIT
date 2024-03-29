% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tRackIT_fun.R
\name{tRackIT}
\alias{tRackIT}
\title{calculate triangulations}
\usage{
tRackIT(
  animal,
  projList,
  tw = 5,
  be_col = "bearings_filtered",
  path_to_directory
)
}
\arguments{
\item{animal}{list, list generated by initAnimal functiom}

\item{projList}{list, list generated by initProject functiom}

\item{tw}{numeric, time window in which bearings are used for triangulation}

\item{be_col}{character, colname of bearing column}

\item{path_to_directory}{string, path to directory where files with bearings are stored}
}
\description{
calculates triangulations
}
\examples{
#projroot<-paste0(getwd(),"/tRackIT_test_data/")
# get project file
#p <- getProject(projroot)
# get meta data of 1 individual
#animal <- getAnimal(projroot, animalID = "woodpecker")
#tRackIT(animal = animal, projList = p, be_col = "bearings_filtered", tw = 5, path_to_directory = animal$path$bearings_filtered)

}
\author{
Jannis Gottwald
}
