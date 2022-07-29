test_that("multiplication works", {
  
  projroot = "H:/projects/repositories/test_project/"
  df<-data.frame(timestamp=as.POSIXct("2021-06-10 05:25:19"), station=as.character("Station_1"), receiver=as.integer(2), max=-68.96852, n_receivers=as.integer(1), prediction="active")
  df$station<-as.character(df$station)
  
  test_project<-getProject(projroot = projroot, plot=FALSE)
  anml<-getAnimal(projList = test_project, projroot =   projroot, animalID = "woodpecker")
  
  pred<-activity_predict_tRackIT(animal=anml)
  
  expect_true(identical(attributes(df)$names, attributes(pred)$names))
  
  expect_true(identical(attributes(df)$class, attributes(pred)$class))
  
  expect_true(identical(sapply(df, class), sapply(pred, class)))
  
})
