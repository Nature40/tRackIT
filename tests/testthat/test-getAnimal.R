test_that("Expected output", {
  projroot = "H:/projects/repositories/test_project/"
  test<-getProject(projroot = projroot, plot=FALSE)
  anml<-getAnimal(projroot = projroot, projList = test, animalID = "woodpecker")
  expect_equal(names(anml), c("meta", "path"))
  
  expect_equal(!is.na(anml$meta$animalID),!is.na(1)) 
  
  expect_equal(!is.na(anml$meta$freq),!is.na(1))
  
  expect_error(getAnimal(projList = test, projroot = ".", animalID = "wodpecker"))
})
