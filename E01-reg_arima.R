#
# Program: reg_arima.r
#
# Purpose: Multiple Linear Regression with arma errors to predict
#          electricity demand (MWh) in PS
#
# Created: 16 April 2019
#
# ------------------------------------------------------
#

# Set locale to English language
Sys.setlocale("LC_TIME", "C")

# Loading librairies
library(timeSeries)
library(forecast)
library(astsa)
library(chron)
library(timeDate)


#################
#   FUNCTIONS   #
#################


read.daily_mw_data <- function(path="../data/ts_daily.csv") {
  # This function can be used to load a daily electricity demand time 
  # series, if this time series has already been generated. This enables
  # saving time as the function to convert from hourly to daily is 
  # expensive in computing time.
  dem <- read.csv(paste(path,sep=","))
  ts_daily <- timeSeries(dem, format="%Y-%m-%d")
  colnames(ts_daily) <- c('mw')
  return(ts_daily)
}


wea_hourly_to_daily <- function(ts_hourly, day, T_ref_c, T_ref_h) {
  # This function is used to to downsample an hourly timeseries of 
  # numeric variables to daily via different aggregation functions
  # T_ref_h is the reference temperature for the HDD, T_ref_c is the 
  #reference temperature for the CDD.
  # Column names: 'Temp_C', 'Precip_in', 'Humidity_pct', 'WindSpeed_MPH'
  
  # Initialize day and numeric vectors
  d_days <- character(); d_TO <- numeric(); d_Precip <- numeric(); 
  d_Hum <- numeric(); d_Wind <- numeric()
  d_Tt <- numeric(); d_HDD <- numeric(); d_CDD <- numeric(); 
  d_TE <- numeric (); d_CP <- numeric()
  d_Tmin <- numeric(); d_Tmax <-numeric()
  
  # Initialize memory variables
  day_last=day[1]    
  daily_sum_precip=0 
  daily_mean_temp=0
  daily_min_temp=9999
  daily_max_temp=-9999
  daily_mean_hum=0   
  daily_mean_wind=0
  j=1
  daily_records=0
  
  # Aggregating daily values
  for(i in 1:length(day)){
    day_i  <- day[i]    
    
    if (day_i == day_last) { 
      daily_records = daily_records  +  1  
      daily_sum_precip = daily_sum_precip  +  ts_hourly$Precip_in[i]
      daily_mean_temp = daily_mean_temp  +  ts_hourly$Temp_C[i]
      if (ts_hourly$Temp_C[i] < daily_min_temp) {
        daily_min_temp <- ts_hourly$Temp_C[i]
      }
      if (ts_hourly$Temp_C[i] > daily_max_temp) {
        daily_max_temp <- ts_hourly$Temp_C[i]
      } 
      daily_mean_hum = daily_mean_hum  +  ts_hourly$Humidity_pct[i]
      daily_mean_wind= daily_mean_wind  +  ts_hourly$WindSpeed_MPH[i]  
    } else {
      d_days[j]=as.character(day_last)  
      d_Precip[j]=daily_sum_precip       
      d_TO[j]=daily_mean_temp / daily_records       
      d_Hum[j]=daily_mean_hum / daily_records       
      d_Wind[j]=daily_mean_wind / daily_records     
      d_Tmax[j]=daily_max_temp
      d_Tmin[j]=daily_min_temp
      d_Tt[j] = (daily_min_temp + daily_max_temp) / 2  
      d_HDD[j] = max((T_ref_h - d_Tt[j]), 0)            
      d_CDD[j] = max((d_Tt[j] - T_ref_c), 0)           
      if (d_TO[j] >= T_ref_h) d_CP[j] = 0 else 
        d_CP[j] = d_Wind[j]^0.5*(T_ref_h-d_TO[j])
      if (j > 1) d_TE[j]=0.5*d_TO[j] + 0.5*d_TE_previous else
        d_TE[j]=d_TO[j]
      
      #starting a new day
      daily_records <- 1
      d_TE_previous <- d_TE[j]
      daily_sum_precip <- ts_hourly$Precip_in[i]
      daily_mean_temp <- ts_hourly$Temp_C[i]
      daily_min_temp <- ts_hourly$Temp_C[i]
      daily_max_temp <- ts_hourly$Temp_C[i]
      daily_mean_hum = ts_hourly$Humidity_pct[i]
      daily_mean_wind= ts_hourly$WindSpeed_MPH[i]  
      
      day_last = day_i                   #reinitialize day_last
      j=j + 1                              #daily index increases
    }
  }
  d_days[j]=as.character(day_last)   
  d_Precip[j]=daily_sum_precip       
  d_TO[j]=daily_mean_temp / daily_records       
  d_Hum[j]=daily_mean_hum / daily_records       
  d_Wind[j]=daily_mean_wind / daily_records     
  d_Tmax[j]=daily_max_temp
  d_Tmin[j]=daily_min_temp
  d_Tt[j] = (daily_min_temp + daily_max_temp) / 2 
  d_HDD[j] = max((T_ref_h - d_Tt[j]), 0)         #store HDD in d_HDD[j]
  d_CDD[j] = max((d_Tt[j] - T_ref_c), 0)          #store CDD in d_CDD[j]
  if (d_TO[j] >= T_ref_h) d_CP[j] = 0 else 
    d_CP[j] = d_Wind[j]^0.5*(T_ref_h-d_TO[j])
  if (j > 1) d_TE[j]=0.5*d_TO[j] + 0.5*d_TE_previous else 
    d_TE[j]=d_TO[j]
  
  # Combining all numeric vectors into a data frame prior to converting
  # to a time series
  daily_data_frame <- cbind.data.frame(d_TO, d_TE, d_Tt, d_HDD, d_CDD, 
                                       d_Tmin, d_Tmax, d_Precip, d_Hum,
                                       d_Wind, d_CP)
  
  # Finally converting date vector and numeric data matrix into 
  # timeseries and renaminc columns
  ts_daily <- timeSeries(daily_data_frame, d_days, format="%Y-%m-%d")  
  colnames(ts_daily) <- c('TO_C', 'TE_C', 'Tt_C', 'HDD_C', 'CDD_C', 
                          'Tmin_C', 'Tmax_C', 'Precip_in', 
                          'Humidity_pct', 'WindSpeed_MPH', 'CP')
  
  return(ts_daily)
}

read.daily_weather_data<-function(path="../data/weather_ts_daily.csv") {
  # This function can be used to load the daily weather time series, if
  # this time series has already been generated. This enables saving 
  # time as the function to convert from hourly to daily is expensive 
  # in computing time.
  wea_data <- read.csv(paste(path,sep=","))
  ts_daily <- timeSeries(wea_data, format="%Y-%m-%d")
  colnames(ts_daily) <- c('TO_C', 'TE_C', 'Tt_C', 'HDD_C', 'CDD_C',
                          'Tmin_C', 'Tmax_C', 'Precip_in', 
                          'Humidity_pct', 'WindSpeed_MPH', 'CP')
  return(ts_daily)
}


##############################
#     WEATHER VARIABLES      #    
##############################

# Reading the weather data, which comes in two csv files: 
# #1 for years 2005-2009  +  #2 for years 2009-2019 
wea1 <- read.csv("../data/WBAN_54743_2005_2009.csv", sep=",",
                  na.strings=c("","NA"), stringsAsFactors = FALSE) 
wea2 <- read.csv("../data/WBAN_54743_2009_2019.csv", sep=",",
                  na.strings=c("","NA"), stringsAsFactors = FALSE)

# Concatenating so that the date for the 13 years are in one vector
# (for each variable) 
nj.wea.day <- c(as.Date(wea1$DATE, "%Y-%m-%d %H:%M"), 
                as.Date(wea2$DATE, "%Y-%m-%d %H:%M"))
nj.wea.datetime <- c(as.character(wea1$DATE), as.character(wea2$DATE))
nj.wea.temp <- c(as.numeric(wea1$HOURLYDRYBULBTEMPC), 
                 as.numeric(wea2$HOURLYDRYBULBTEMPC)) 
nj.wea.precip <- c(as.numeric(wea1$HOURLYPrecip), 
                   as.numeric(wea2$HOURLYPrecip))
nj.wea.hum <- c(as.numeric(wea1$HOURLYRelativeHumidity),
                as.numeric(wea2$HOURLYRelativeHumidity)) 
nj.wea.wind <-c(as.numeric(wea1$HOURLYWindSpeed), 
                as.numeric(wea2$HOURLYWindSpeed))


# Building data frame out of numeric data to look for missing values
wea_data_frame <- cbind.data.frame(nj.wea.temp, nj.wea.precip,
                  nj.wea.hum, nj.wea.wind) 
#summary(wea_data_frame)  #5362 NAs in temp,  37935 NAs in precip,
                          #6131 NAs in hum, 5555 NAs in wind


# Replacing NAs elements in temperature and humidity with previous
# values
wea_data_frame$nj.wea.hum <- na.locf(wea_data_frame$nj.wea.hum)
wea_data_frame$nj.wea.temp <- na.locf(wea_data_frame$nj.wea.temp)
#summary(wea_data_frame)


# Replacing NAs in precipitation and wind with 0 
for(i in 1:length(wea_data_frame$nj.wea.precip)) { 
  if (is.na(wea_data_frame$nj.wea.precip[i])) {
    wea_data_frame$nj.wea.precip[i] <- 0 } 
  if (is.na(wea_data_frame$nj.wea.wind[i])) { 
    wea_data_frame$nj.wea.wind[i] <- 0 } } 
# summary(wea_data_frame) # no more NA

# Creating time series from dataframe
wea_ts_hourly <- timeSeries(wea_data_frame, nj.wea.datetime, 
                            format="%Y-%m-%d %H:%M") 
colnames(wea_ts_hourly) <- c('Temp_C', 'Precip_in', 
                             'Humidity_pct', 'WindSpeed_MPH')

# Converting hourly time series into daily time series, while also
# creating all feature variables (e.g. CP, HDD, CDD, etc)
wea_ts_daily <- wea_hourly_to_daily(wea_ts_hourly, nj.wea.day,
                                    18.3, 13.5)

# Removing 2019 to be consistent with electricity demand 
wea_ts_daily <- wea_ts_daily[-seq(5109, 5139),]

# Removing February 29th to be consistent with electricity demand
# wea_ts_daily[1155, ] wea_ts_daily[2616, ] wea_ts_daily[4072, ]
wea_ts_daily <- wea_ts_daily[-c(1155, 2616, 4072), ]
length(wea_ts_daily$TO_C) #supposed to be 5110, we are missing 5 values


# Imputing missing days from October 30th 2012 to November 4th 2012
#(inclusively) 
ts_daily <- read.daily_mw_data(path="../data/ts_daily_del_imp.csv") 
mw_df_daily <- data.frame(ts_daily)
wea_df_daily <- data.frame(wea_ts_daily) 
last_row <- wea_df_daily[2857,] 
wea_df_daily <- rbind(wea_df_daily[1:2857,], last_row, 
                      last_row, last_row, last_row, last_row, 
                      wea_df_daily[2858:5105,]) 
wea_df_daily[2855:2869,] #it worked, but we need to change the date 
row.names(wea_df_daily) <- row.names(mw_df_daily) 
wea_df_daily[2855:2869,] #it worked,

# Converting wea_df_daily back into a time series 
wea_ts_daily <- timeSeries(wea_df_daily, row.names(wea_df_daily), 
                           format="%Y-%m-%d")
length(wea_ts_daily$TO_C) 
summary(wea_ts_daily)

# Saving daily time series into csv file 
write.table(wea_ts_daily, file = "../data/weather_ts_daily.csv", 
            row.names=TRUE, col.names=c('TO_C', 'TE_C', 'Tt_C',
                                        'HDD_C', 'CDD_C', 'Tmin_C',
                                        'Tmax_C', 'Precip_in', 
                                        'Humidity_pct', 'WindSpeed_MPH',
                                        'CP'), sep=",")

# Read wea_ts_daily if this was generated already, read ts_daily, 
# create two dataframes and merge them
wea_ts_daily <- read.daily_weather_data(
                                  path="../data/weather_ts_daily.csv") 
ts_daily <- read.daily_mw_data(path="../data/ts_daily_del_imp.csv")
mw_df_daily <- data.frame(ts_daily)
wea_df_daily <- data.frame(wea_ts_daily)
combined_df <- merge(mw_df_daily, wea_df_daily, by="row.names")

# removing irrelevant feature variables
combined_df <- combined_df[c(1:5110), -c(3,4,5,8,9,10,11,12)]

# saving combined dataframe into a csv file
write.csv(combined_df, file="../data/all_var_df.csv", row.names = FALSE)



######################################
#       CREATING DUMMY VARIABLES     #
######################################

# load features
combined_df = read.csv("../data/all_var_df.csv")

# creating dummy variables for months
month_vec <- format(as.Date(combined_df$Row.names), "%B")
is_ = factor(month_vec)
dummies_month = model.matrix(~is_ + 0)
combined_df = cbind(combined_df, dummies_month)

# removing one month (i.e. December) to avoid colinear variables
combined_df <- combined_df[c(1:5110), -c(8)]

# creating dummy variables for days of week
weekday_vec <- weekdays(as.Date(combined_df$Row.names))
is_ = factor(weekday_vec)
dummies_weekday = model.matrix(~is_ + 0)
combined_df = cbind(combined_df, dummies_weekday)

# removing one weekday (i.e. Sunday) to avoid colinear variables
combined_df <- combined_df[c(1:5110), -c(20)]

# creating dummy variables for christmas, christmas  + 1,  + 2, -1, -1

xmas  <- dates(as.character(holiday(2005:2018,"USChristmasDay")),
               format="Y-m-d")
is_xmas_day <-is.holiday(as.Date(combined_df$Row.names), xmas)*1
is_xmas_min_one <- is.holiday(as.Date(combined_df$Row.names) + 1, 
                              xmas)*1
is_xmas_plus_one <- is.holiday(as.Date(combined_df$Row.names)-1, 
                               xmas)*1
is_xmas_min_two <- is.holiday(as.Date(combined_df$Row.names) + 2, 
                              xmas)*1
is_xmas_plus_two <- is.holiday(as.Date(combined_df$Row.names)-2, 
                               xmas)*1
combined_df <- cbind(combined_df, is_xmas_day, is_xmas_min_one, 
                     is_xmas_min_two, is_xmas_plus_one, 
                     is_xmas_plus_two)

# creating dummy variable for good friday
goodfriday  <- dates(as.character(holiday(2005:2018,"USGoodFriday")),
                     format="Y-m-d")
is_good_friday <-is.holiday(as.Date(combined_df$Row.names), 
                            goodfriday)*1
combined_df <- cbind(combined_df, is_good_friday)

# creating dummy variables for independance day
independance  <- dates(as.character(
  holiday(2005:2018,"USIndependenceDay")),format="Y-m-d")
is_independance_day <-is.holiday(as.Date(combined_df$Row.names), 
                                 independance)*1
is_independance_min_one <- is.holiday(as.Date(combined_df$Row.names) +
                                        1, independance)*1
is_independance_plus_one <- is.holiday(as.Date(combined_df$Row.names)-
                                         1, independance)*1
is_independance_min_two <- is.holiday(as.Date(combined_df$Row.names) +
                                        2, independance)*1
is_independance_plus_two <- is.holiday(as.Date(combined_df$Row.names)-
                                         2, independance)*1
combined_df <- cbind(combined_df, is_independance_day, 
                     is_independance_min_one, is_independance_min_two,
                     is_independance_plus_one, is_independance_plus_two)

# creating dummy variable for labor day
laborday  <- dates(as.character(holiday(2005:2018,"USLaborDay")),
                   format="Y-m-d")
is_laborday <-is.holiday(as.Date(combined_df$Row.names), laborday)*1
combined_df <- cbind(combined_df, is_laborday)

# creating dummy variable for newyearsday
newyearsday  <- dates(as.character(holiday(2005:2018,"USNewYearsDay")),
                      format="Y-m-d")
is_newyears_day <-is.holiday(as.Date(combined_df$Row.names), 
                             newyearsday)*1
is_newyears_min_one <- is.holiday(as.Date(combined_df$Row.names) +
                                    1, newyearsday)*1
is_newyears_plus_one <- is.holiday(as.Date(combined_df$Row.names)-
                                     1, newyearsday)*1
is_newyears_min_two <- is.holiday(as.Date(combined_df$Row.names) +
                                    2, newyearsday)*1
is_newyears_plus_two <- is.holiday(as.Date(combined_df$Row.names)-
                                     2, newyearsday)*1
combined_df <- cbind(combined_df, is_newyears_day, is_newyears_min_one,
                     is_newyears_min_two,
                     is_newyears_plus_one, is_newyears_plus_two)

# creating dummy variable for thanksgiving
thanksgiving  <- dates(as.character(
  holiday(2005:2018,"USThanksgivingDay")),format="Y-m-d")
is_thanksgiving_day <-is.holiday(as.Date(combined_df$Row.names), 
                                 thanksgiving)*1
is_thanksgiving_plus_one <- is.holiday(as.Date(combined_df$Row.names)-
                                         1, thanksgiving)*1
combined_df <- cbind(combined_df, is_thanksgiving_day, 
                     is_thanksgiving_plus_one)

# creating dummy variable for US elections
elections  <- dates(as.character(holiday(2005:2018,"USElectionDay")),
                    format="Y-m-d")
is_election_day <-is.holiday(as.Date(combined_df$Row.names), 
                             elections)*1
combined_df <- cbind(combined_df, is_election_day)

# creating lag variables for HDD-1, HDD-2, CDD-1 and CDD-2
combined_df$CDD_min_one <- 0
combined_df$CDD_min_two <- 0
combined_df$HDD_min_one <- 0
combined_df$HDD_min_two <- 0

combined_df$CDD_min_one[[2]] <- combined_df$CDD_C[[1]]
combined_df$HDD_min_one[[2]] <- combined_df$HDD_C[[1]]

for (i in 3:5110) {
  combined_df$CDD_min_one[[i]] <- combined_df$CDD_C[[i-1]]
  combined_df$HDD_min_one[[i]] <- combined_df$HDD_C[[i-1]]
  combined_df$CDD_min_two[[i]] <- combined_df$CDD_C[[i-2]]
  combined_df$HDD_min_two[[i]] <- combined_df$HDD_C[[i-2]]
}


# saving dataframe for later use
write.csv(combined_df, file = "../data/csv_for_regression.csv", 
          row.names=FALSE)

##############################################
#        MULTIPLE LINEAR REGRESSION          #
##############################################
# 

# output file for graphs
pdf("reg_arima.pdf")

# loading data from previous steps
combined_df = read.csv("../data/csv_for_regression.csv")
ts_daily <- read.daily_mw_data(path="../data/ts_daily_del_imp.csv")

# differencing electricity demand and X variables with lag 1
ts_diff1 <- diff(ts_daily, lag = 1, differences = 1)
#ts_diff7 <- diff(ts_daily, lag = 7, differences = 1)
HDD_diff1 <- diff(as.timeSeries(
  combined_df$HDD_C, as.Date(time(ts_diff1))),lag=1, differences=1)
CDD_diff1 <- diff(as.timeSeries(
  combined_df$CDD_C, as.Date(time(ts_diff1))),lag=1, differences=1)
CDDm1_diff1 <- diff(as.timeSeries(
  combined_df$CDD_min_one, as.Date(time(ts_diff1))),lag=1,differences=1)
HDDm1_diff1 <- diff(as.timeSeries(
  combined_df$HDD_min_one, as.Date(time(ts_diff1))),lag=1,differences=1)
CDDm2_diff1 <- diff(as.timeSeries(
  combined_df$CDD_min_two, as.Date(time(ts_diff1))),lag=1,differences=1)
HDDm2_diff1 <- diff(as.timeSeries(
  combined_df$HDD_min_two, as.Date(time(ts_diff1))),lag=1,differences=1)


# adding differenced variables to combined_df
combined_df <- cbind(combined_df, ts_diff1, ts_diff1, HDD_diff1, 
                     CDD_diff1, CDDm1_diff1, HDDm1_diff1, 
                     CDDm2_diff1,HDDm2_diff1)

# division of combined_df into a training, validation and test sets
training_df <- combined_df[9:3285,]
validation_df <- combined_df[3286:4380,]
test_df <- combined_df[4381:5110,]


# training linear regression with arma error using auto.arima()
reg8_w_arima.fit = auto.arima(training_df$mw.1, 
              xreg=cbind(training_df$is_Friday, training_df$is_Monday, 
              training_df$is_Saturday, training_df$is_Thursday, 
              training_df$is_Tuesday, training_df$is_Wednesday,
              training_df$TS.1.1.1, training_df$TS.1.2.1, 
              training_df$TS.1.1.1.3.2, training_df$TS.1.2.1.4.2, 
              training_df$TS.1.1.3.2.5.3, training_df$TS.1.2.4.2.6.3,
              training_df$is_xmas_day, training_df$is_xmas_min_one,
              training_df$is_xmas_min_two, training_df$is_xmas_plus_one, 
              training_df$is_xmas_plus_two, training_df$is_laborday, 
              training_df$is_newyears_day, 
              training_df$is_newyears_min_one, 
              training_df$is_newyears_min_two, 
              training_df$is_newyears_plus_one,
              training_df$is_newyears_plus_two, 
              training_df$is_thanksgiving_day, 
              training_df$is_thanksgiving_plus_one))


# printing a description of the model
sink("reg_arima.fit.out")
print(reg8_w_arima.fit)   # Selects ARIMA(2,0,1) with non-zero mean
sink()

# printing ACF/PACF of residuals
acf2(residuals(reg8_w_arima.fit)[-(1:2)],
    main="Structure des résidus avec auto-arima")

# plotting QQ plots
qqnorm(residuals(reg8_w_arima.fit), xlab='Quantiles théoriques', 
       ylab='Quantiles échantillonés', main='Diagrame Q-Q')
qqline(residuals(reg8_w_arima.fit))

# using trained model to predict demand(mwh) on validation set
p8_delta <- predict(reg8_w_arima.fit,n.ahead=1,newxreg=cbind(
                validation_df$is_Friday, validation_df$is_Monday, 
                validation_df$is_Saturday, validation_df$is_Thursday, 
                validation_df$is_Tuesday, validation_df$is_Wednesday,
                validation_df$TS.1.1.1, validation_df$TS.1.2.1, 
                validation_df$TS.1.1.1.3.2, validation_df$TS.1.2.1.4.2,
                validation_df$TS.1.1.3.2.5.3,
                validation_df$TS.1.2.4.2.6.3, validation_df$is_xmas_day,
                validation_df$is_xmas_min_one,
                validation_df$is_xmas_min_two, 
                validation_df$is_xmas_plus_one, 
                validation_df$is_xmas_plus_two, 
                validation_df$is_laborday, 
                validation_df$is_newyears_day,
                validation_df$is_newyears_min_one, 
                validation_df$is_newyears_min_two, 
                validation_df$is_newyears_plus_one,
                validation_df$is_newyears_plus_two, 
                validation_df$is_thanksgiving_day, 
                validation_df$is_thanksgiving_plus_one))


# undifferencing the prediction by adding the demand from yesterday
p8_undiff <- p8_delta[[1]] + combined_df$mw[3285:4379]

# Computing MAPE, bias and %biais on validation set
reg8_w_arima_bias <- mean(p8_undiff-validation_df$mw)
reg8_w_arima_pbias <- mean((p8_undiff-validation_df$mw)/
                             validation_df$mw)*100
reg8_w_arima_mape <- mean(abs((p8_undiff-validation_df$mw)/
                                validation_df$mw))*100

cat("Bias - test set: ", reg8_w_arima_bias/1000)
cat("%Bias - test set: ", reg8_w_arima_pbias)
cat("MAPE - test set: ", reg8_w_arima_mape)


# Computing bias and MAPE on validation set by seasons 
winter <- c(1:79,355:443,720:808)
spring <- c(79:171,444:536,809:901)
summer <- c(172:264,537:629,902:994)
fall <- c(265:354,630:719,995:1084)

reg8_w_arima_bias_winter <- 
                  mean(p8_undiff[winter]-validation_df$mw[winter])
reg8_w_arima_pbias_winter <- 
                  mean((p8_undiff[winter]-validation_df$mw[winter])/
                         validation_df$mw[winter])*100
reg8_w_arima_mape_winter <- 
                  mean(abs((p8_undiff[winter]-validation_df$mw[winter])/
                             validation_df$mw[winter]))*100

reg8_w_arima_bias_spring <- 
                  mean(p8_undiff[spring]-validation_df$mw[spring])
reg8_w_arima_pbias_spring <- 
                  mean((p8_undiff[spring]-validation_df$mw[spring])/
                         validation_df$mw[spring])*100
reg8_w_arima_mape_spring <- 
                  mean(abs((p8_undiff[spring]-validation_df$mw[spring])/
                             validation_df$mw[spring]))*100

reg8_w_arima_bias_summer <- 
                  mean(p8_undiff[summer]-validation_df$mw[summer])
reg8_w_arima_pbias_summer <- 
                  mean((p8_undiff[summer]-validation_df$mw[summer])/
                         validation_df$mw[summer])*100
reg8_w_arima_mape_summer <- 
                  mean(abs((p8_undiff[summer]-validation_df$mw[summer])/
                             validation_df$mw[summer]))*100

reg8_w_arima_bias_fall <- 
                  mean(p8_undiff[fall]-validation_df$mw[fall])
reg8_w_arima_pbias_fall <- 
                  mean((p8_undiff[fall]-validation_df$mw[fall])/
                         validation_df$mw[fall])*100
reg8_w_arima_mape_fall <- 
                  mean(abs((p8_undiff[fall]-validation_df$mw[fall])/
                             validation_df$mw[fall]))*100

cat("MAPE reg_arima (WINTER) =", reg8_w_arima_mape_winter)
cat("MAPE reg_arima (SPRING) =", reg8_w_arima_mape_spring)
cat("MAPE reg_arima (SUMMER) =", reg8_w_arima_mape_summer)
cat("MAPE reg_arima (FALL) =", reg8_w_arima_mape_fall)


# Plotting an example for summer 2014
plot(ts_daily[3437:3528], type='l', xaxt = 'n', 
     xlab="Date", ylab='Demande(MWh)')
lines(p8_undiff[152:242], col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))
axis(1, at=c(0,31,61,91), labels=c('01 Juin 2014', '01 Juil 2014',
                                   '01 Août 2014', '01 Sept 2014'))


# using trained model to predict demand (mwh) for test set

p8_delta_test <- predict(reg8_w_arima.fit,n.ahead=1,newxreg=cbind(
                         test_df$is_Friday, test_df$is_Monday, 
                         test_df$is_Saturday, test_df$is_Thursday, 
                         test_df$is_Tuesday, test_df$is_Wednesday,
                         test_df$TS.1.1.1, test_df$TS.1.2.1, 
                         test_df$TS.1.1.1.3.2, test_df$TS.1.2.1.4.2, 
                         test_df$TS.1.1.3.2.5.3, test_df$TS.1.2.4.2.6.3,
                         test_df$is_xmas_day, test_df$is_xmas_min_one,
                         test_df$is_xmas_min_two, 
                         test_df$is_xmas_plus_one, 
                         test_df$is_xmas_plus_two, test_df$is_laborday, 
                         test_df$is_newyears_day, 
                         test_df$is_newyears_min_one, 
                         test_df$is_newyears_min_two, 
                         test_df$is_newyears_plus_one,
                         test_df$is_newyears_plus_two, 
                         test_df$is_thanksgiving_day, 
                         test_df$is_thanksgiving_plus_one))




# undifferencing demand by adding the demand from yesterday
p8_undiff_test <- p8_delta_test[[1]] + combined_df$mw[4380:5109]

# Computing MAPE, bias and %biais on test test
reg8_w_arima_bias_test <- mean(p8_undiff_test-test_df$mw)
reg8_w_arima_pbias_test <- mean((p8_undiff_test-test_df$mw)/
                                  test_df$mw)*100
reg8_w_arima_mape_test <- mean(abs((p8_undiff_test-test_df$mw)/
                                     test_df$mw))*100


cat("Bias - whole test set: ", reg8_w_arima_bias_test/1000)
cat("%Bias - whole test set: ", reg8_w_arima_pbias_test)
cat("MAPE - whole test set: ", reg8_w_arima_mape_test)


# Computing bias and MAPE on test set by seasons
winter <- c(1:79,355:443,720:730)
spring <- c(79:171,444:536)
summer <- c(172:264,537:629)
fall <- c(265:354,630:719)

reg8_w_arima_bias_winter <- 
              mean(p8_undiff_test[winter]-test_df$mw[winter])
reg8_w_arima_pbias_winter <- 
              mean((p8_undiff_test[winter]-test_df$mw[winter])/
                     test_df$mw[winter])*100
reg8_w_arima_mape_winter <- 
              mean(abs((p8_undiff_test[winter]-test_df$mw[winter])/
                         test_df$mw[winter]))*100

reg8_w_arima_bias_spring <- 
              mean(p8_undiff_test[spring]-test_df$mw[spring])
reg8_w_arima_pbias_spring <- 
              mean((p8_undiff_test[spring]-test_df$mw[spring])/
                     test_df$mw[spring])*100
reg8_w_arima_mape_spring <- 
              mean(abs((p8_undiff_test[spring]-test_df$mw[spring])/
                         test_df$mw[spring]))*100

reg8_w_arima_bias_summer <- 
              mean(p8_undiff_test[summer]-test_df$mw[summer])
reg8_w_arima_pbias_summer <- 
              mean((p8_undiff_test[summer]-test_df$mw[summer])/
                     test_df$mw[summer])*100
reg8_w_arima_mape_summer <- 
              mean(abs((p8_undiff_test[summer]-test_df$mw[summer])/
                         test_df$mw[summer]))*100

reg8_w_arima_bias_fall <- 
              mean(p8_undiff_test[fall]-test_df$mw[fall])
reg8_w_arima_pbias_fall <- 
              mean((p8_undiff_test[fall]-test_df$mw[fall])/
                     test_df$mw[fall])*100
reg8_w_arima_mape_fall <- 
              mean(abs((p8_undiff_test[fall]-test_df$mw[fall])/
                         test_df$mw[fall]))*100

cat("MAPE - test set (WINTER) =", reg8_w_arima_mape_winter)
cat("MAPE - test set (SPRING) =", reg8_w_arima_mape_spring)
cat("MAPE - test set (SUMMER) =", reg8_w_arima_mape_summer)
cat("MAPE - test set (FALL) =", reg8_w_arima_mape_fall)



dev.off(dev.cur())

