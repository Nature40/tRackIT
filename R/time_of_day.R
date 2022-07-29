#' calculates time of day information
#'
#' @description calculates sunset and sunrise for each data point and the distance to it. For nocturnal species sunrise is +1 day of sunset
#'
#'
#' @author Jannis Gottwald
#'
#' @param data data.frame, data with timestamps 
#' @param tcol string, colname of timestamp column
#' @param Lat numeric, reference latitude for sunrise/sunset calculation
#' @param Lon numeric, reference longitude for sunrise/sunset calculation
#' @param tz string, time zone of region ton convert timezone of data to the right format
#' @param activity_period string, either "diurnal" or "nocturnal"
#' @export
#'
#'



time_of_day<-function(data,  Lat, Lon, tcol, tz, activity_period){
 if(activity_period=="nocturnal"){
  
  
  results<-nocturnal(data=data, Lat=Lat, Lon=Lon, tcol = tcol, tz = tz)
  return(results)
  
}

if(activity_period=="diurnal"){
  
  
  results<-diurnal(data=data, Lat=Lat, Lon=Lon, tcol = tcol, tz = tz)
  return(results)
  
}

}



