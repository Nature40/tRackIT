test_that("Expected output", {
 
  anml<-initAnimal(projList = test_project,projroot= "H:/projects/repositories/test_project/",saveAnml = TRUE, animalID = test_project$tags$ID[1], species = "woodpecker", sex = "m", age = "adult", weight = 36, rep.state = "breeding", freq = 150050, start = test_project$tags$start[1], end = test_project$tags$end[1] )
  
  expect_equal(names(anml), c("meta", "path"))
  
  expect_equal(!is.na(anml$meta$animalID),!is.na(1)) 
  
  expect_equal(!is.na(anml$meta$freq),!is.na(1))

})
