test_that("Variable calculation works", {
  projroot = "H:/projects/repositories/test_project/"
  test<-getProject(projroot = projroot, plot=FALSE)
  anml<-getAnimal(projroot = projroot, projList = test, animalID = "woodpecker")
  vars<-activity_vars_tRackIT(animal=anml, tcol="timestamp", scol="max", dcol="receiver", tzone="CET", rscale=0)
  data<-plyr::ldply(list.files(anml$path$vars, full.names = TRUE), data.table::fread)
  
  expect_equal(nrow(data), 433839)
  
})
