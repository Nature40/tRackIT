#' calculate triangulations using telemetr
#'
#' @description calculates triangulations using functionalities from telemetr-package
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param data spatial.data.frame, 
#' @param tmstmp character, central timestamp of rolling window 
#' 
#' 
#'
#' @export
#'
#'



calc_telemetr_tRackIT<-function(data, tmstmp){
  require(telemetr)
  
  df_tel<-data.frame()
  
  tryCatch(
    expr = {
      mle<-telemetr::triang(~be,data,method="mle")
      mle$method<-"mle"
    },
    error = function(e){ 
      NULL
    }
  )
  
  
  tryCatch(
    expr = {
      hub<-telemetr::triang(~be,data,method="hub")
      hub$method<-"hub"
    },
    error = function(e){ 
      NULL
    }
  )
  
  
  tryCatch(
    expr = {
      and<-telemetr::triang(~be,data,method="and")
      and$method<-"and"
    },
    error = function(e){ 
      NULL
    }
  )
  
  
  tryCatch(
    expr = {
      rmr<-telemetr::triang(~be,data,method="rmr")
      rmr$method<-"rmr"
      rmr$kappa<-"NA"
      rmr$err<-"NA"
      rmr$sd<-"NA"
      rmr$ijob<-"NA"
      
    },
    error = function(e){ 
      
      rmr<-NULL
    }
  )
  
  tryCatch(
    expr = {
      df_tel<-rbind(mle,hub)
    },
    error = function(e){ 
      NULL
    }
  )
  
  
  tryCatch(
    expr = {
      df_tel<-rbind(df_tel,and)
    },
    error = function(e){ 
      NULL
    }
  )
  
  tryCatch(
    expr = {
      df_tel<-rbind(df_tel,rmr)
    },
    error = function(e){ 
      NULL
    }
  )
  
  if(nrow(df_tel)>0){
    df_tel$time<-tmstmp
    
   
    
    return(df_tel)}
  # 
  else{return(NULL)}
  
}

  
  