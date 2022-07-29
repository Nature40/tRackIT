#' filter data interactive
#'
#' @description calculate triangulations based on different bearing calculation methods
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param animal list, generatet by initanimal
#' @param projList list, generatet by initProject
#' @param path_to_data string, path to filtered file
#' @param method string,("linear, "acos", "ML") indicates wether linear, acos ore machine learning based bearings are used
#' @param collision logical, if collision corrected data is used a suffix is added to resulting file name
#' 
#'
#' @export
#'
#'



triangulate<-function(method, path_to_data, animal, projList, collision=FALSE){
  
antennas<-projList$stations
  
data<-data.table::fread(path_to_data)
data<-as.data.frame(data)
names(data)<-gsub("a_", "",names(data))
data$intersection<-abs(angle_between(data$s1, data$s2))
data$X<-NA
data$Y<-NA
data<-data[!is.na(data$s1),]
data<-data[!is.na(data$s2),]

for( i in 1:nrow(data)){
 # print(i)
s1<-antennas[as.character(antennas$station)==as.character(data$name_s1)[i],]
s2<-antennas[as.character(antennas$station)==as.character(data$name_s2)[1],]
tri<-tri(s1$X[1],s1$Y[1],data$s1[i],
                s2$X[1],s2$Y[1],data$s2[i])

data$X[i]<-tri$x[1]
data$Y[i]<-tri$y[1]
    
}

data<-data[!is.na(data$X),]
if(collision==FALSE){
data$timestamp<-as.POSIXct(data$timestamp)
data.table::fwrite(data, paste0(animal$path$triangulations, "/", anml$meta$animalID,"_FROM_", as.Date(min(data$timestamp)), "_TO_",as.Date(max(data$timestamp)),"_triangulations_", method, ".csv"))}

if(collision==TRUE){
  data$timestamp<-as.POSIXct(data$timestamp)
  data.table::fwrite(data, paste0(animal$path$triangulations, "/", anml$meta$animalID,"_FROM_", as.Date(min(data$timestamp)), "_TO_",as.Date(max(data$timestamp)),"_triangulations_", method, "_collision.csv"))}



}






tri_in<-function(method, data, projList){
  
  stations<-projList$stations
  data<-as.data.frame(data)
  names(data)<-gsub("a_", "",names(data))
  data$intersection<-abs(angle_between(data$s1, data$s2))

  data<-data[!is.na(data$s1),]
  data<-data[!is.na(data$s2),]
  
  for( i in 1:nrow(data)){
    # print(i)
    stations<-as.data.frame(stations)
    s1<-stations[as.character(stations$station)==as.character(data$name_s1)[i],]
    s2<-stations[as.character(stations$station)==as.character(data$name_s2)[i],]
    tri<-tri(s1$X[1],s1$Y[1],data$s1[i],
                s2$X[1],s2$Y[1],data$s2[i])
    
    if(is.data.frame(tri)){
    data$x[i]<-tri$x[1]
    data$y[i]<-tri$y[1]}
    
  }
  
  return(data)
  
  
  
}



