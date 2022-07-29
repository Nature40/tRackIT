#' join raw logger data per station
#'
#' @description joins all raw logger files in a folder (only for files recorded with tRackit.OS!)
#'
#'
#' @author Jannis Gottwald
#'
#' @param projList list, generatet by initProject function 
#' 
#' @export
#'
#'


read.logger.data.tRackIT<-function(projList){
  
  
  dirs<-list.dirs(projList$path$raw, recursive=FALSE)
  
  lapply(dirs,function(d){
    
    fls<-list.files(paste0(d, "/radiotracking/"), full.names = TRUE, pattern=".csv")
    fls<-fls[grep("matched",fls, invert=TRUE)]  
  
    
    stat_dat<-plyr::ldply(fls, function(f){
      
      print(fls)
      
      tryCatch(exp={
        sig<-data.table::fread(f)
        if(lubridate::is.POSIXct(sig$Time)){
        return(sig)}},
        error=function(e) {
          message("file broken")
          
        })
      
      
      
      })
    
    colnames(stat_dat)[which(names(stat_dat) == "Device")] <- "receiver"
    colnames(stat_dat)[which(names(stat_dat) == "Time")] <- "timestamp"
    colnames(stat_dat)[which(names(stat_dat) == "Frequency")] <- "frequency"
    colnames(stat_dat)[which(names(stat_dat) == "Duration")] <- "duration"
    colnames(stat_dat)[which(names(stat_dat) == "max (dBW)")] <- "max"
    colnames(stat_dat)[which(names(stat_dat) == "avg (dBW)")] <- "avg"
    colnames(stat_dat)[which(names(stat_dat) == "std (dB)")] <- "std"
    colnames(stat_dat)[which(names(stat_dat) == "noise (dBW)")] <- "noise"
    colnames(stat_dat)[which(names(stat_dat) == "snr (dB)" )] <- "snr"
    
    stat_dat$frequency<-stat_dat$frequency/1000
    
    stat_dat$station<-basename(d)
    
    
data.table::fwrite(stat_dat, paste0(projList$path$csv, "/",basename(d), "_FROM_", as.Date(min(stat_dat$timestamp)), "_TO_",as.Date(max(stat_dat$timestamp)), ".csv" ))
    
  })
 
  
}



