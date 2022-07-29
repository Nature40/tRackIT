
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
#' 
#'
#' @export
#'
#'





diurnal<-function(data,  Lat, Lon, tcol, tz){
  
  data$timestamp<-data[,tcol]
  
  data$timestamp<-lubridate::with_tz(data$timestamp, tzone=tz)
  
  data$sunrise<-NULL
  data$sunset<-NULL
  
  
  data$date<-as.Date(data$timestamp,tz="CET")
  
  
  #sunset time and sunsrise time for all existing dates
  dates<-unique(data$date)
  dates<-dates[!is.na(dates)]
  dates<- format(as.Date(dates), "%Y/%m/%d")
  num<-as.numeric(as.Date(as.character(max(dates)), format="%Y/%m/%d")-as.Date(as.Date(as.character(min(dates))),   format="%Y/%m/%d"))
  sun<-StreamMetabolism::sunrise.set( Lat, Lon, min(dates), timezone=tz, num.days = num+1)
  sun$date<-as.Date(sun$sunrise)
  
  #join to data
  data <- dplyr::inner_join(data, sun)
  
  data<-as.data.frame(data)
  
  data$time_to_rise<-as.numeric(difftime(data$timestamp, data$sunrise, units="hours"))
  data$time_to_set<-as.numeric(difftime(data$timestamp, data$sunset, units="hours"))
  
  
  data$start_datetime<-min(data$timestamp)
  data$stop_datetime<-max(data$timestamp)
  data$year <- lubridate::year(data$date)
  data$ydate <- lubridate::yday(data$date) # Day of the year
  data$month <- lubridate::month(data$date) # Day of the month
  data$t <- data$timestamp-data$start_datetime
  
  
  #define the night
  return(data)
  
  
}