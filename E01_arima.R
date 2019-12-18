#
# Program: e01_arima.R
#
# Purpose: Test arima methods to predict electricity demand

# Created: April 2019
##############################################################
#SARIMA 
#############################################################
 library(forecast)
 library(timeSeries)
 library(astsa)
 library(urca)
 
 df = read.csv("../data/csv_for_regression.csv")
 colnames(df)[1] = "Date"

 date_date = as.Date(df$Date) # juste les dates 
 
 #### PREPARER L'ÉCRITURE DU CSV 
 read.daily_mw_data <- function(path="../data/ts_daily.csv") {
#This function is used to load a daily electricity demand time series
   #  if this time series has already been generated. 
   #This enables saving time as the function to convert from hourly 
   #to daily is expensive in computing time.
   dem <- read.csv(paste(path,sep=","))
   ts_daily <- timeSeries(dem, format="%Y-%m-%d")
   colnames(ts_daily) <- c('mw')
   return(ts_daily)
 }
 
 read.sarima_data1 <- function(path="../data/sarima 207 211 7.csv") {
   # This function can be used to load the sarima predictions 
   # if this time series has already been generated. 
   #This enables saving time as the function to create it 
   #in the first place is expensive in computing time.
   sarima_data <- read.csv(paste(path,sep=","))
   ts_daily <- timeSeries(sarima_data, format="%Y-%m-%d")
   colnames(ts_daily) <- c('mw')
   return(ts_daily)
 }
 read.sarima_data2 <- function(path="../data/sarima 208 010 7.csv") {
   # This function can be used to load the sarima predictions 
   # if this time series has already been generated. 
   #This enables saving time as the function to create it 
   #in the first place is expensive in computing time.
   sarima_data <- read.csv(paste(path,sep=","))
   ts_daily <- timeSeries(sarima_data, format="%Y-%m-%d")
   colnames(ts_daily) <- c('mw')
   return(ts_daily)
 }

 ts_daily <- read.daily_mw_data(path="../data/ts_daily_del_imp.csv")
 
 
 #####?TUDE DE LA PARTIE ENTRAINEMENT
 training_set <- window(ts_daily, as.Date ("2005-01-01"), 
                         as.Date("2013-12-31"))  
 
 # training set (on ne garde que les 2 dernières années comme
 #conseillé lors de la pérsentation)
 training_set_sarima <- window(ts_daily, as.Date ("2012-01-01"), 
                               as.Date("2013-12-31"))
 
 # validation set
 validation_set_sarima <- window(ts_daily, as.Date ("2014-01-01"), 
                                 as.Date("2016-12-31")) 
 # test set
 test_set_sarima <- window(ts_daily, as.Date ("2017-01-01"), 
                           as.Date("2018-12-31"))
 
 #####?TUDE DE LA S?RIE AU COMPLET 
 #Rappel que non statoinnaire
 ts_mw = as.timeSeries(df$mw) # la demande comme objet 'timeSeries'
 ts_mw2 = as.ts(df$mw) # la demande comme objet 'ts'
 par(mfrow=c(1,1)) # reset le param?tre 
 acf2(ts_mw2, main="ACF et PACF de la s?rie chrono")
 plot(date_date, ts_mw/1000, type="l", xlab="Date", #graphe de la serie
      main="Demande d\'?lectricit? de 2005 ? 2018", 
      ylab='Demande en GW')


 # diff lag 7 de la partie training
 diff_ts_mw_2 = diff(training_set$mw, lag=7)
 acf2(diff_ts_mw_2, 
      main="ACF et PACF de la s?rie chrono 
      (une diff?rentiation de d?calage 7)")
 plot(diff_ts_mw_2,main="différanciation training lag =7",
      ylab="Différence MW",xlab="temps", type="l")
 #Les groupes entre saisons chaudes et 
 #saisons froides sont très présents encore
 
 diff_ts_mw_2 %>% ur.kpss(use.lag=7) %>% summary()
 
 #####AU VU DES ACF ET PACF ON COMMENCE PAR UN MA(8) de la s?rie diff
 # on essayera aussi le modèle de base (1,0,1)
 
 #UTILISER SARIMA
 try1 <- sarima(as.numeric(training_set$mw),2,0,8,0,1,0,7)

 try3 <- sarima(as.numeric(training_set$mw),2,0,7,2,1,1,7)
 #hypothèses plutot respectés sur ce modèle
 
 try4 <- sarima(as.numeric(training_set$mw),2,0,7,2,1,2,7)
 #hypothèses plutot respectés sur ce modèle
 
 try5 <- sarima(as.numeric(training_set$mw),2,0,7,3,1,2,7)
 #hypothèses plutot respectés sur ce modèle
 
 try6 <- sarima(as.numeric(training_set$mw),2,0,7,3,1,1,7)
 #hypothèses plutot respectés sur ce modèle
 
 
 #on va checker les aic et bic de tous les modèles pour 
 #choisir avec AIC/BIC
 # One can easily get AIC and BIC values for each of the models.
 cat("ARIMA(2,0,8)(0,1,0)[7] - AIC:",try1$AIC," BIC:",try1$BIC,"\n")
 cat("ARIMA(2,0,7)(2,1,1)[7] - AIC:",try3$AIC," BIC:",try3$BIC,"\n")
 cat("ARIMA(2,0,7)(2,1,2)[7] - AIC:",try4$AIC," BIC:",try4$BIC,"\n")
 cat("ARIMA(2,0,7)(3,1,2)[7] - AIC:",try5$AIC," BIC:",try5$BIC,"\n")
 cat("ARIMA(2,0,7)(3,1,1)[7] - AIC:",try6$AIC," BIC:",try6$BIC,"\n")

 ########CHOIX DU MODÈLE selon AIC/BIC : 
 #ARIMA(2,0,7)(2,1,1)[7]
 print (try3)
# ARIMA(2,0,8)(0,1,0)[7]
 print (try1)
 
# ########## ROULER TOUT LE OUT.SAMPLE... ARIMA(2,0,7)(2,1,1)[7]
 
# #Pr?vision au fur et ? mesure
 # initializaing fcast
 
# # sarima_forecast <- numeric()
# # day_fcast_sarima <- character()
#  class(day_fcast_sarima) <- "Date"
#  
#  start_time <- Sys.time()
#  
#  # adjust the in.sample to include the new observation, 
#  #and make another forecast for the next day.
#  
#  # # creating initial validation set (2014-2016), 
#  #i.e. for the first prediction
# 
#  for (i in 1:length(validation_set_sarima)) {
#    training_set_sarima <- window(ts_daily, as.Date("2012-01-01"), 
#                                  as.Date(time
#                                          (validation_set_sarima[i,]))-1)
#    in.sample.df <- data.frame(training_set_sarima)
#    #forecast
#    sarima_forecast[i] <- sarima.for(as.numeric(training_set_sarima), 
#                                     n.ahead=1,
#                                     p=2,d=0,q=7,
#                                     P=2,D=1,Q=1,S=7)[[1]][[1]]
#    day_fcast_sarima[i] <- as.Date(time(validation_set_sarima[i,]))
#  }
#  sarima_forecast_df <- data.frame(sarima_forecast)
#  day_fcast_sarima_df <- data.frame(day_fcast_sarima)
#  sarima_fcast <- timeSeries(sarima_forecast_df, 
#                             as.character(day_fcast_sarima), 
#                             format="%Y-%m-%d")
#  end_time <- Sys.time()
#  print(end_time - start_time)
#  write.table(sarima_fcast, file = "../data/sarima 207 211 7.csv", row.names=TRUE,
#              col.names=c('mw'), sep=",")

sarima_fcast1 <- read.sarima_data1()


# Computing errors on sarima
sarima_bias <- mean(sarima_fcast1-validation_set_sarima)
sarima_pbias <- mean((sarima_fcast1-validation_set_sarima)
                     /validation_set_sarima)*100
sarima_mape <- mean(abs((sarima_fcast1-validation_set_sarima)
                        /validation_set_sarima))*100

cat("BIAS modèle 207 211 7 =",sarima_bias/1000) # en GW
cat("%BIAS modèle 207 211 7 =", sarima_pbias)
cat("MAPE modèle 207 211 7=",sarima_mape)



# ########## ROULER TOUT LE OUT.SAMPLE... ARIMA(2,0,8)(0,1,0)[7]

# # #Pr?vision au fur et ? mesure
# # initializaing fcast
# 
# sarima_forecast <- numeric()
# day_fcast_sarima <- character()
# class(day_fcast_sarima) <- "Date"
# 
# start_time <- Sys.time()
# 
# # adjust the in.sample to include the new observation, 
# #and make another forecast for the next day.
# 
# # # creating initial validation set (2014-2016), 
# #i.e. for the first prediction
# 
# for (i in 1:length(validation_set_sarima)) {
#   training_set_sarima <- window(ts_daily, as.Date("2012-01-01"), 
#                                 as.Date(time
#                                         (validation_set_sarima[i,]))-1)
#   in.sample.df <- data.frame(training_set_sarima)
#   #forecast
#   sarima_forecast[i] <- sarima.for(as.numeric(training_set_sarima), 
#                                    n.ahead=1,
#                                    p=2,d=0,q=8,
#                                    P=0,D=1,Q=0,S=7)[[1]][[1]]
#   day_fcast_sarima[i] <- as.Date(time(validation_set_sarima[i,]))
# }
# sarima_forecast_df <- data.frame(sarima_forecast)
# day_fcast_sarima_df <- data.frame(day_fcast_sarima)
# sarima_fcast <- timeSeries(sarima_forecast_df, 
#                            as.character(day_fcast_sarima), 
#                            format="%Y-%m-%d")
# end_time <- Sys.time()
# print(end_time - start_time)
# write.table(sarima_fcast, file = "../data/sarima 208 010 7.csv", row.names=TRUE,
#             col.names=c('mw'), sep=",")

sarima_fcast <- read.sarima_data2()

# Computing errors on sarima
sarima_bias <- mean(sarima_fcast-validation_set_sarima)
sarima_pbias <- mean((sarima_fcast-validation_set_sarima)
                     /validation_set_sarima)*100
sarima_mape <- mean(abs((sarima_fcast-validation_set_sarima)
                        /validation_set_sarima))*100

cat("BIAS modèle 208 010 7 =",sarima_bias/1000) # en GW
cat("%BIAS modèle 208 010 7 =", sarima_pbias)
cat("MAPE modèle 208 010 7=",sarima_mape)

# Computing bias and MAPE on validation set by seasons 
#(i.e. winter, spring, summer, fall)
winter <- c(1:79,355:443,720:808)
spring <- c(79:171,444:536,809:901)
summer <- c(172:264,537:629,902:994)
fall <- c(265:354,630:719,995:1084)

sarima_bias_winter <- mean(sarima_fcast[winter]-
                             validation_set_sarima[winter])
sarima_pbias_winter <- mean((sarima_fcast[winter]-
                               validation_set_sarima[winter])
                            /validation_set_sarima[winter])*100
sarima_mape_winter <- mean(abs((sarima_fcast[winter]-
                                  validation_set_sarima[winter])
                               /validation_set_sarima[winter]))*100

sarima_bias_spring <- mean(sarima_fcast[spring]-
                             validation_set_sarima[spring])
sarima_pbias_spring <- mean((sarima_fcast[spring]-
                               validation_set_sarima[spring])
                            /validation_set_sarima[spring])*100
sarima_mape_spring <- mean(abs((sarima_fcast[spring]-
                                  validation_set_sarima[spring])
                               /validation_set_sarima[spring]))*100

sarima_bias_summer <- mean(sarima_fcast[summer]-
                             validation_set_sarima[summer])
sarima_pbias_summer <- mean((sarima_fcast[summer]-
                               validation_set_sarima[summer])
                            /validation_set_sarima[summer])*100
sarima_mape_summer <- mean(abs((sarima_fcast[summer]-
                                  validation_set_sarima[summer])
                               /validation_set_sarima[summer]))*100

sarima_bias_fall <- mean(sarima_fcast[fall]-
                           validation_set_sarima[fall])
sarima_pbias_fall <- mean((sarima_fcast[fall]-
                             validation_set_sarima[fall])
                          /validation_set_sarima[fall])*100
sarima_mape_fall <- mean(abs((sarima_fcast[fall]-
                                validation_set_sarima[fall])
                             /validation_set_sarima[fall]))*100

cat("MAPE modèle 208 010 7 (WINTER) =", sarima_mape_winter)
cat("MAPE modèle 208 010 7 (SPRING) =", sarima_mape_spring)
cat("MAPE modèle 208 010 7 (SUMMER) =", sarima_mape_summer)
cat("MAPE modèle 208 010 7 (FALL) =", sarima_mape_fall)









