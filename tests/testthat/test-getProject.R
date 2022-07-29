test_that("Expected output", {
  
  projroot = "H:/projects/repositories/test_project/"
  test<-getProject(projroot = projroot, plot=FALSE)
 nms<-c( "path","tags","stations","epsg","tz") 
 expect_equal(class(getProject(projroot = projroot, plot=FALSE)), class(list()))
 
 expect_equal(names(getProject(projroot = projroot, plot=FALSE)), nms)
 
 expect_error(getProject(projroot = ".", plot=FALSE))
 
 
})
