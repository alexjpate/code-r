#
#

library(lubridate)
library(tidyverse)
library(DescTools)

stopwatch_start <- function(new_stopwatch = T){
  
  if(new_stopwatch == T){
  stopwatch <<- list()
  }
  
  stopwatch[["start"]] <<- Sys.time()
}

stopwatch_lap <- function(message = T, units = "secs"){
  
  num <- if( any(names(stopwatch) %like% 'lap%') == T){
    as.integer(max(str_split(names(stopwatch[names(stopwatch) %like% 'lap%']),"lap",simplify = T)[,2]))
    } else {
    0
    }
  time <- Sys.time()
  
  last_lap <- ifelse(
    num != 0, 
    stopwatch[names(stopwatch) == paste0("lap",num)][[1]][1]  %>% as.integer(), 
    stopwatch$start 
    )
  class(last_lap) <- c("POSIXct", "POSIXt")
             
  stopwatch[[paste0("lap",num+1)]] <<- list(time,difftime(time,last_lap, units = units))
  
  if(message == T){
    print(difftime(time,last_lap, units = units))
  }
  
}

stopwatch_end <- function(message = T, units = "secs"){

  time <- Sys.time()
  stopwatch[["end"]] <<- list(time,difftime(time,stopwatch$start, units = units))
  
  if(message == T){
    print(difftime(time,stopwatch$start, units = units))
  }
  
}
