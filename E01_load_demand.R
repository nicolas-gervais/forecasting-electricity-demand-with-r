#
# Program: load_demand.R
#
# Purpose: Load hourly electricity demand, clean data, convert to 
#          daily time series
#
# Created: 16 April 2019
#
# ------------------------------------------------------
#

# Set locale to English language
Sys.setlocale("LC_TIME", "C")

# Loading librairies
library(timeSeries)


#################
#   FUNCTIONS   #
#################

read.raw_data <- function(path="../data/hrl_load_metered.csv") {
  # This function is used to load the hourly electricity demand time 
  # series
  
  datetime <- character(); demand <- numeric()      
  dem <- read.csv(paste(path,sep=","))
  demand <- c(demand, dem$mw)
  day  <- as.Date(dem$datetime_beginning_ept, "%m/%d/%Y %H:%M:%S %p")
  datetime <- c(datetime, as.character(dem$datetime_beginning_ept))
  mylist <- remove.dst_duplicate(demand, datetime)
  mylist2 <- fill.dst_empty_row(mylist$demand, mylist$datetime)
  demand_cor <- mylist2$demand
  datetime_cor <- mylist2$datetime
  ts_hourly <- timeSeries(demand_cor, datetime_cor, 
                          format="%m/%d/%Y %I:%M:%S %p")
  colnames(ts_hourly) <- c('Demand (mw)')
  ts_daily <- hourly_to_daily(ts_hourly, day)
  return(ts_daily)
}

remove.dst_duplicate <- function(demand, datetime) {
  # This function can be used to remove the duplicated hours form the 
  # electricity demand datetime
  # resulting from the daylight saving time changes (1 hour duplicated
  # every year since 1964).
  
  demand_c <- numeric()
  datetime_c <- character()
  idx <- 1
  demand_c[idx] <- demand[1]
  datetime_c[idx] <- datetime[1]
  idx <- idx  +  1
  
  for(i in 2:length(demand)){
    if (datetime[i] == datetime[i-1]) {  
      print(datetime[i])      
    } else {
      demand_c[idx] <- demand[i]
      datetime_c[idx] <- datetime[i]
      idx <- idx  +  1
    }
  }
  mylist <- list("demand" = demand_c, "datetime" = datetime_c)
  return(mylist)
}

fill.dst_empty_row <- function(demand, datetime) {
  # This function can be used to fill the missing hours form the 
  # electricity demand datetime
  # resulting from the daylight saving time changes (1 hour missing
  # every year since 1964).
  
  demand_c <- numeric()
  datetime_c <- character()
  idx <- 1
  demand_c[idx] <- demand[1]
  datetime_c[idx] <- datetime[1]
  idx <- idx  +  1
  
  for(i in 2:length(demand)){
    now = as.POSIXct(datetime[i], format="%m/%d/%Y %H:%M:%S %p", 
                     tz="EST")
    before= as.POSIXct(datetime[i-1], format="%m/%d/%Y %H:%M:%S %p",
                     tz="EST")
    
    if ((now - before) == 2) {  #if there is a missing hour
      # cat("datetime[i-1]:",datetime[i-1], "\n")
      # cat("datetime[i]:", datetime[i], "\n")
      # cat("demand[i-1]:", demand[i-1], "\n")
      # cat("demand[i]:", demand[i], "\n")
      before= as.POSIXct(datetime[i-1], format="%m/%d/%Y %H:%M:%S %p", 
                         tz="CET")  
      datetime_c[idx]=as.character(before + 3600, 
                                   format="%m/%d/%Y %H:%M:%S %p")
      demand_c[idx] = (demand[i] + demand[i-1])/2
      idx=idx + 1
      datetime_c[idx]=datetime[i]
      demand_c[idx] = demand[i]
      # cat("datetime_c[idx-1]:",datetime_c[idx-1], "\n")
      # cat("datetime_c[idx]:", datetime_c[idx], "\n")  
      # cat("demand_c[idx-1]:", demand_c[idx-1], "\n")
      # cat("demand_c[idx]:", demand_c[idx], "\n")
      idx=idx + 1
    }else {
      datetime_c[idx]=datetime[i]
      demand_c[idx] = demand[i]
      idx=idx + 1
    }
  }
  mylist <- list("demand" = demand_c, "datetime" = datetime_c)
  return(mylist)
}

hourly_to_daily <- function(ts_hourly, day) {
  # This function is used to downsample an hourly timeseries of 
  # electricity demand to daily by
  # summing all hourly values
  d_days <- character(); d_demand <- numeric()  
  day_last=day[1]    #initialize day_last
  daily_sum=0        #initialize daily_sum
  j=1                #initialize daily index
  for(i in 1:length(ts_hourly)){
    day_i  <- day[i]    
    if (day_i == day_last) {  
      daily_sum = daily_sum  +  ts_hourly[i]   #sum up demand
    } else {
      d_days[j]=as.character(day_last)   
      d_demand[j]=daily_sum              
      daily_sum = ts_hourly[i]           #start new daily sum with i
      day_last = day_i                   #reinitialize day_last
      j=j + 1                              #daily index increases
    }
  }
  d_days[j]=as.character(day_last)
  d_demand[j]=daily_sum
  ts_daily <- timeSeries(d_demand, d_days, format="%Y-%m-%d")
  return(ts_daily)
}

########################################
#   DEMAND DATA LOADING & PROCESSING   #
########################################

# Loading electricity demand in PS and converting to a daily timeseries
ts_daily <- read.raw_data()   

# Removing 29th of February    
length(ts_daily) # supposed to be 5113
ts_daily[1155, ]
ts_daily[2616, ]
ts_daily[4077, ]
ts_daily <- ts_daily[-c(1155, 2616, 4077), ]
length(ts_daily) # supposed to be 5110

# Save csv file of daily time series without 29th february
write.table(ts_daily, file = "../data/ts_daily_del.csv", row.names=TRUE,
            col.names=c('mw'), sep=",")

# Imputing outliers
ts_daily[2857] <- 107933.86  
ts_daily[2858] <- 115177.65  
ts_daily[2859] <- 112667.77  
ts_daily[2860] <- 113232.09  
ts_daily[2861] <- 113359.93  
ts_daily[2862] <- 102053.19  
ts_daily[2863] <- 103077.70  
aberrant_2007 <- ts_daily[940] 
ts_daily[940, ] <- 130530.25   

ts_daily[2857:2863, ] # double check
ts_daily[940, ] # double check

# Exporting to csv
write.table(ts_daily, file = "../data/ts_daily_del_imp.csv", 
            row.names=TRUE, col.names=c('mw'), sep=",")
