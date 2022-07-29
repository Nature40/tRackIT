test_that("Does output match expected output", {
  
  tag_df<-data.frame(id="woodpecker", freq=150050, start="2021-06-10", end="2021-06-14")
  
  #
  stations<-data.table::fread("H:/projects/repositories/test_project/reference_data/stations_mof_2021.csv")
  
  #init Project
  test_project<-initProject(projroot = "H:/projects/repositories/test_project/",
                            logger_data_raw=".",
                            tags=tag_df,
                            id_col="id",
                            start_col="start", 
                            end_col="end",
                            freq_col="freq",
                            stations=stations,
                            s_col="station",
                            x_col="X",
                            y_col="Y",
                            r_col="receiver",
                            o_col="orientation",
                            epsg=4326,
                            tz="CET")
  
  nms<-c("raw", "catalogues", "csv","ref","c_Curves","correction", "ids",        "awk","param_lst","models","fun","results")   
  
  expect_equal(class(test_project$path), class(list()))
  
  expect_equal(class(test_project$tags), class(data.frame()))
  expect_equal(class(test_project$stations), class(data.frame()))
  expect_equal(names(test_project$path), nms)
  
  expect_warning(test_project<-initProject(projroot = "H:/projects/repositories/test_project/",
                                           logger_data_raw=".",
                                           tags=tag_df,
                                           id_col="id",
                                           start_col="start", 
                                           end_col="end",
                                           freq_col="freq",
                                           stations=stations,
                                           s_col="station",
                                           x_col="X",
                                           y_col="Y",
                                           r_col="receiver",
                                           o_col="orientation",
                                           epsg=4326,
                                           tz="CET"))
  
})
