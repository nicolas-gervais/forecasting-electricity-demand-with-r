#
# Program: dshw_tbats.R
#
# Purpose: Using exponential smoothing to predict electricity demand 
# (MWh) in PS
# Created: 16 April 2019
#
# ------------------------------------------------------
#

# Set locale to English language
Sys.setlocale("LC_TIME", "C")

# Loading librairies
library(timeSeries)
library(forecast)

#################
#   FUNCTIONS   #
#################

read.daily_mw_data <- function(path="../data/ts_daily.csv") {
  # This function can be used to load a daily electricity demand 
  # time series, if this time series has already been generated. This
  # enables saving time as the function to convert from hourly to daily
  # is expensive in computing time.
  dem <- read.csv(paste(path,sep=","))
  ts_daily <- timeSeries(dem, format="%Y-%m-%d")
  colnames(ts_daily) <- c('mw')
  return(ts_daily)
}

read.dshw_data <- function(path="../data/dshw.csv") {
  # This function can be used to load the dshw smoothing predictions for daily electricity demand.
  # if this time series has already been generated. This enables saving time as the 
  # function to create it in the first place is expensive in computing time.
  dshw_data <- read.csv(paste(path,sep=","))
  ts_daily <- timeSeries(dshw_data, format="%Y-%m-%d")
  colnames(ts_daily) <- c('mw')
  return(ts_daily)
}

read.tbats_data <- function(path="../data/tbats.csv") {
  # This function can be used to load the dshw smoothing predictions for daily electricity demand.
  # if this time series has already been generated. This enables saving time as the 
  # function to create it in the first place is expensive in computing time.
  dshw_data <- read.csv(paste(path,sep=","))
  ts_daily <- timeSeries(dshw_data, format="%Y-%m-%d")
  colnames(ts_daily) <- c('mw')
  return(ts_daily)
}


#############################
#   EXPONENTIAL SMOOTHING   #
#############################
# 
# 
pdf("e01-dshw.pdf")

ts_daily <- read.daily_mw_data(path="../data/ts_daily_del_imp.csv")

# creating training set (2005-2013)
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2013-12-31"))
in.sample.df <- data.frame(in.sample)


# ### Double Seasonal Holt-Winters Forecasting Method ###
#
# Creating msts objects so that dshw recognizes these two periods
# automatically.
# Periods: 7 for week since we have daily data
#          365 for year since we have daily data
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,364))

# Training a dshw smoothing model on the in.sample
start_time <- Sys.time()
fit <- dshw(in.sample.msts, h=1)
end_time <- Sys.time()
print(end_time - start_time) # 2.793 min

# Printing description of fitted model
sink("dshw.fit.out")
summary(fit)
sink()


# Plotting several graphs to see whether the seasonal pattern is well
# captured in a year forecast and for several weeks. 
# This is just a verification exercise, as this project is not 
# interested in forecasting in mid/long term horizon, only
# forecasting the next day.

# EXEMPLE 2013 À 2015
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2013-12-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,364))

fcast_dshw_example_2014 <- dshw(in.sample.msts, h=364, model=fit)

validation_2014 <- window(ts_daily, as.Date("2014-01-01"), 
                          as.Date("2014-12-30"))
# Plot forecast and add observed data to compare (for one year).
plot(fcast_dshw_example_2014, xlim=c(9,11),
     ylab="Demande(MWh)", main="Méthode de lissage DSHW (2013-2015)",
     xaxt = "n")
lines(seq((10 + 9/364),(10 + 373/364),length=364),validation_2014,
      col="red")
axis(1, at=c(9, 10, 11), labels=c('2013', '2014', '2015'))
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","red"))

# EXEMPLE 2014 À 2016
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2014-12-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,364))

fcast_dshw_example_2015 <- dshw(in.sample.msts, h=364, model=fit)

validation_2015 <- window(ts_daily, as.Date("2015-01-01"), 
                          as.Date("2015-12-30"))
# Plot forecast and add observed data to compare (for one year).
plot(fcast_dshw_example_2015, xlim=c(10,12), xaxt = "n",
     ylab="Demande(MWh)", main="Méthode de lissage DSHW (2014-2016)", 
     ylim=c(80000, 200000))
lines(seq((11 + 9/364),(11 + 373/364),length=364),validation_2015,
      col="red")
axis(1, at=c(10, 11, 12), labels=c('2014', '2015', '2016'))
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","red"))

# EXEMPLE HIVER 2014
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2013-12-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,364))

fcast_dshw_example_hiv_2014 <- dshw(in.sample.msts, h=364, model=fit)

validation_hiv_2014 <- window(ts_daily, as.Date("2014-01-01"), 
                              as.Date("2014-12-30"))
# Plot forecast and add observed data to compare (for one year).
plot(fcast_dshw_example_hiv_2014, xlim=c(10-(31-9)/364, 10 + 
                                           (31 + 9)/364), xaxt = "n",
     ylab="Demande(MWh)", 
     main="Méthode de lissage DSHW (Dec 2013 - Jan 2014)", 
     ylim=c(100000, 160000))
lines(seq((10 + 9/364),(10 + 373/364),length=364),
      validation_hiv_2014,col="red")
axis(1, at=c(9.95, 10.025, 10.10), 
     labels=c('01 Dec 2013', '01 Jan 2014', '01 Fev 2014'))
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","red"))

# EXEMPLE ETE 2014
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2014-07-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,364))

fcast_dshw_example_ete_2014 <- dshw(in.sample.msts, h=364, model=fit)

validation_ete_2014 <- window(ts_daily, as.Date("2014-08-01"), 
                              as.Date("2014-09-30"))
# Plot forecast and add observed data to compare (for one year).
plot(fcast_dshw_example_ete_2014, xlim=c(10 + (181 + 10)/364, 10 + 
                                           (242 + 10)/364),xaxt = 'n',
     ylab="Demande(MWh)", 
     main="Méthode de lissage DSHW (Juil 2014 - Août 2014)")
lines(seq((10 + (212 + 9)/364), (10 + (273 + 9)/364),length=61),
      validation_ete_2014,col="red")
axis(1, at=c((10 + (181 + 10)/364), (10 + (212 + 10)/364), (10 + 
                      (242 + 10)/364)), 
     labels=c('01 Juil 2014', '01 Août 2014', '01 Sept 2014'))
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","red"))

# The dshw model will now be used to make predictions on day ahead, 
# for all days in the validation set. This procedure will be to march 
# forward one step after each forecast, adjust the in.sample to include
# the new observation, and make another forecast for the next day.

out.sample <- window(ts_daily, as.Date("2014-01-01"), 
                     as.Date("2016-12-31"))

# initializaing fcast
dshw_forecast <- numeric()
day_fcast <- character()
class(day_fcast) <- "Date"

start_time <- Sys.time()

for (i in 1:length(out.sample)) {
  in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                      as.Date(time(out.sample[i,]))-1)
  in.sample.df <- data.frame(in.sample)
  # Creating msts objects so that dshw recognizes these two periods
  # automatically.
  # Periods: 7 for week since we have daily data
  #          365 for year since we have daily data
  in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,364))
  dshw_forecast[i] <- dshw(in.sample.msts, h=1, model=fit)[[1]][[1]]
  day_fcast[i] <- as.Date(time(out.sample[i,]))
}

dshw_forecast_df <- data.frame(dshw_forecast)
day_fcast_df <- data.frame(day_fcast)

ts_fcast <- timeSeries(dshw_forecast_df, as.character(day_fcast), 
                       format="%Y-%m-%d")

end_time <- Sys.time()
print(end_time - start_time)

write.table(ts_fcast, file = "../data/dshw.csv", row.names=TRUE,
            col.names=c('mw'), sep=",")

ts_fcast <- read.dshw_data()

# Computing errors on dshw
dshw_bias <- mean(ts_fcast-out.sample)
dshw_pbias <- mean((ts_fcast-out.sample)/out.sample)*100
dshw_mape <- mean(abs((ts_fcast-out.sample)/out.sample))*100

dshw_bias/1000 # en GW
dshw_pbias
dshw_mape

# Plotting several graphs to compare observation to dshw prediction

# EXEMPLE WIN 2014
ts.to.plot <- window(ts_daily, as.Date("2014-01-01"), 
                     as.Date("2014-03-01"))
fcast.to.plot <- window(ts_fcast, as.Date("2014-01-01"), 
                        as.Date("2014-03-01"))

plot(ts.to.plot, xlab='Date', ylab='Demande(MWh)', 
     main='DSHW prévision vs observation (Hiver 2014)')
lines(fcast.to.plot, col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))

# EXEMPLE ETE 2014
ts.to.plot <- window(ts_daily, as.Date("2014-06-01"), 
                     as.Date("2014-09-01"))
fcast.to.plot <- window(ts_fcast, as.Date("2014-06-01"), 
                        as.Date("2014-09-01"))

plot(ts.to.plot, xlab='Date', ylab='Demande(MWh)', 
     main='DSHW prévision vs observation (Été 2014)')
lines(fcast.to.plot, col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))

# EXEMPLE WIN 2015
ts.to.plot <- window(ts_daily, as.Date("2015-01-01"), 
                     as.Date("2015-03-01"))
fcast.to.plot <- window(ts_fcast, as.Date("2015-01-01"), 
                        as.Date("2015-03-01"))

plot(ts.to.plot, xlab='Date', ylab='Demande(MWh)', 
     main='DSHW prévision vs observation (Hiver 2015)')
lines(fcast.to.plot, col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))

# EXEMPLE ETE 2015
ts.to.plot <- window(ts_daily, as.Date("2015-06-01"), 
                     as.Date("2015-09-01"))
fcast.to.plot <- window(ts_fcast, as.Date("2015-06-01"), 
                        as.Date("2015-09-01"))

plot(ts.to.plot, xlab='Date', ylab='Demande(MWh)', 
     main='DSHW prévision vs observation (Été 2015)')
lines(fcast.to.plot, col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))


dev.off(dev.cur())

### TBATS ###

# output file for graphs
pdf("e01-tbats.pdf")


# Creating msts
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2013-12-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,365))

# Training a tbats smoothing model on the in.sample
start_time <- Sys.time()
tbats.fit <- tbats(in.sample.msts)
end_time <- Sys.time()
print(end_time - start_time)

# Printing description of fitted model
sink("tbats.fit.out")
print(tbats.fit)
sink()



# Plotting several graphs to see whether the seasonal pattern is well 
# captured in a year forecast and for several weeks.  This is just a 
# verification exercise, as this project is not interested in <
# forecasting in mid/long term horizon, only
# forecasting the next day.

# EXEMPLE 2013 À 2015
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2013-12-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,365))

fcast_tbats_example_2014 <- forecast(in.sample.msts, h=365, 
                                     model=tbats.fit)
plot(fcast_tbats_example_2014)

validation_2014 <- window(ts_daily, as.Date("2014-01-01"), 
                          as.Date("2014-12-31"))
# Plot forecast and add observed data to compare (for one year).
plot(fcast_tbats_example_2014, xlim=c(9,11),
     ylab="Demande(MWh)", main="Méthode de lissage tbats (2013-2015)", 
     xaxt = "n")
lines(seq(10,11,length=365),validation_2014,col="red")
axis(1, at=c(9, 10, 11), labels=c('2013', '2014', '2015'))
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","red"))

# EXEMPLE 2014 À 2016
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2014-12-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,365))

fcast_tbats_example_2015 <- forecast(in.sample.msts, h=365, 
                                     model=tbats.fit)

validation_2015 <- window(ts_daily, as.Date("2015-01-01"), 
                          as.Date("2015-12-31"))
# Plot forecast and add observed data to compare (for one year).
plot(fcast_tbats_example_2015, xlim=c(10,12), xaxt = "n",
     ylab="Demande(MWh)", main="Méthode de lissage tbats (2014-2016)", 
     ylim=c(80000, 200000))
lines(seq(11,12,length=365),validation_2015,col="red")
axis(1, at=c(10, 11, 12), labels=c('2014', '2015', '2016'))
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","red"))

# EXEMPLE HIVER 2014
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2013-12-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,365))

fcast_tbats_example_hiv_2014 <- forecast(in.sample.msts, h=365, 
                                         model=tbats.fit)

validation_hiv_2014 <- window(ts_daily, as.Date("2014-01-01"), 
                              as.Date("2014-12-31"))
# Plot forecast and add observed data to compare (for one year).
plot(fcast_tbats_example_hiv_2014, xlim=c(10-(31)/365, 10 + (31)/365), 
     xaxt = "n",
     ylab="Demande(MWh)", 
     main="Méthode de lissage tbats (Dec 2013 - Jan 2014)", 
     ylim=c(100000, 160000))
lines(seq((10),(11),length=365),validation_hiv_2014,col="red")
axis(1, at=c(9.95, 10.025, 10.10), 
     labels=c('01 Dec 2013', '01 Jan 2014', '01 Fev 2014'))
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","red"))

# EXEMPLE ETE 2014
in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                    as.Date("2014-07-31"))
in.sample.df <- data.frame(in.sample)
in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,365))

fcast_tbats_example_ete_2014 <- forecast(in.sample.msts, h=365, 
                                         model=tbats.fit)

validation_ete_2014 <- window(ts_daily, as.Date("2014-08-01"), 
                              as.Date("2014-09-30"))
# Plot forecast and add observed data to compare (for one year).
plot(fcast_tbats_example_ete_2014, xlim=c(10 + (181)/365, 10 +
                                            (242)/365),xaxt = 'n',
     ylab="Demande(MWh)", 
     main="Méthode de lissage tbats (Juil 2014 - Août 2014)")
lines(seq((10 + (212)/365), (10 + (273)/365),length=61),
      validation_ete_2014,col="red")
axis(1, at=c((10 + (181)/365), (10 + (212)/365), (10 + (242)/365)), 
     labels=c('01 Juil 2014', '01 Août 2014', '01 Sept 2014'))
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","red"))

# Moving the rolling window of in.sample to predict one step ahead 
# (i.e. one day ahead). i.e. forecasting only one day ahead. Then
# marching forward one step (i.e. one day)
# adjusting the in.sample and making another forecast for the next day.


# initializaing fcast
TBATS_forecast <- numeric()
day_fcast <- character()
class(day_fcast) <- "Date"

start_time <- Sys.time()

for (i in 1:length(out.sample)) {
  in.sample <- window(ts_daily, as.Date("2005-01-01"), 
                      as.Date(time(out.sample[i,]))-1)
  in.sample.df <- data.frame(in.sample)
  # Creating msts objects so that dshw recognizes these two periods
  # automatically.
  # Periods: 7 for week since we have daily data
  #          365 for year since we have daily data
  in.sample.msts <- msts(in.sample.df,seasonal.periods=c(7,365))
  TBATS_forecast[i] <- forecast(in.sample.msts, h=1, 
                                model=tbats.fit)[[2]][[1]]
  day_fcast[i] <- as.Date(time(out.sample[i,]))
}

TBATS_forecast_df <- data.frame(TBATS_forecast)
day_fcast_df <- data.frame(day_fcast)

ts_fcast_tbats <- timeSeries(TBATS_forecast_df, as.character(day_fcast),
                             format="%Y-%m-%d")

end_time <- Sys.time()
print(end_time - start_time)

write.table(ts_fcast_tbats, file = "../data/tbats.csv", row.names=TRUE,
            col.names=c('mw'), sep=",")

ts_fcast_tbats <- read.tbats_data()

# Computing errors on tbats
tbats_bias <- mean(ts_fcast_tbats-out.sample)
tbats_pbias <- mean((ts_fcast_tbats-out.sample)/out.sample)*100
tbats_mape <- mean(abs((ts_fcast_tbats-out.sample)/out.sample))*100

tbats_bias/1000 # en GW
tbats_pbias
tbats_mape

# Computing bias and MAPE on validation set by seasons
winter <- c(1:79,355:443,720:808)
spring <- c(79:171,444:536,809:901)
summer <- c(172:264,537:629,902:994)
fall <- c(265:354,630:719,995:1084)

tbats_bias_winter <- mean(ts_fcast_tbats[winter]-out.sample[winter])
tbats_pbias_winter <- mean((ts_fcast_tbats[winter]-out.sample[winter])/
                             out.sample[winter])*100
tbats_mape_winter <- mean(abs((ts_fcast_tbats[winter]-
                                 out.sample[winter])/
                                out.sample[winter]))*100

tbats_bias_spring <- mean(ts_fcast_tbats[spring]-out.sample[spring])
tbats_pbias_spring <- mean((ts_fcast_tbats[spring]-out.sample[spring])/
                             out.sample[spring])*100
tbats_mape_spring <- mean(abs((ts_fcast_tbats[spring]-
                                 out.sample[spring])/
                                out.sample[spring]))*100

tbats_bias_summer <- mean(ts_fcast_tbats[summer]-out.sample[summer])
tbats_pbias_summer <- mean((ts_fcast_tbats[summer]-out.sample[summer])/
                             out.sample[summer])*100
tbats_mape_summer <- mean(abs((ts_fcast_tbats[summer]-
                                 out.sample[summer])/
                                out.sample[summer]))*100

tbats_bias_fall <- mean(ts_fcast_tbats[fall]-out.sample[fall])
tbats_pbias_fall <- mean((ts_fcast_tbats[fall]-out.sample[fall])/
                           out.sample[fall])*100
tbats_mape_fall <- mean(abs((ts_fcast_tbats[fall]-out.sample[fall])/
                              out.sample[fall]))*100

cat("MAPE TBATS (WINTER) =", tbats_mape_winter)
cat("MAPE TBATS (SPRING) =", tbats_mape_spring)
cat("MAPE TBATS (SUMMER) =", tbats_mape_summer)
cat("MAPE TBATS (FALL) =", tbats_mape_fall)



# EXEMPLE WIN 2014
ts.to.plot <- window(ts_daily, as.Date("2014-01-01"), 
                     as.Date("2014-03-01"))
fcast.to.plot <- window(ts_fcast_tbats, as.Date("2014-01-01"), 
                        as.Date("2014-03-01"))

plot(ts.to.plot, xlab='Date', ylab='Demande(MWh)', 
     main='TBATS prévision vs observation (Hiver 2014)')
lines(fcast.to.plot, col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))

# EXEMPLE ETE 2014
ts.to.plot <- window(ts_daily, as.Date("2014-06-01"), 
                     as.Date("2014-09-01"))
fcast.to.plot <- window(ts_fcast_tbats, as.Date("2014-06-01"), 
                        as.Date("2014-09-01"))

plot(ts.to.plot, xlab='Date', ylab='Demande(MWh)', 
     main='TBATS prévision vs observation (Été 2014)')
lines(fcast.to.plot, col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))

# EXEMPLE WIN 2015
ts.to.plot <- window(ts_daily, as.Date("2015-01-01"), 
                     as.Date("2015-03-01"))
fcast.to.plot <- window(ts_fcast_tbats, as.Date("2015-01-01"), 
                        as.Date("2015-03-01"))

plot(ts.to.plot, xlab='Date', ylab='Demande(MWh)', 
     main='TBATS prévision vs observation (Hiver 2015)')
lines(fcast.to.plot, col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))

# EXEMPLE ETE 2015
ts.to.plot <- window(ts_daily, as.Date("2015-06-01"), 
                     as.Date("2015-09-01"))
fcast.to.plot <- window(ts_fcast_tbats, as.Date("2015-06-01"), 
                        as.Date("2015-09-01"))

plot(ts.to.plot, xlab='Date', ylab='Demande(MWh)', 
     main='TBATS prévision vs observation (Été 2015)')
lines(fcast.to.plot, col='blue')
legend("topleft",legend=c("prévision","observation"),lty=c(1,1),
       col=c("blue","black"))

dev.off(dev.cur())
