# Q1: Import 'USA Firm Sales' DATA
usadata <- read.csv(file.choose(), header = T)
head(usadata)
str(usadata)

# Q2: Create Time Series objects of the data
# BU1 Time Series
usaseries_bu1 <- ts(usadata$BU1, start = c(2015,2), end = c(2018,1), frequency = 12)

# Simple Plot function
plot(usaseries_bu1, col='red', main='Sales Time Series (Simple Plot)')

# ACF plot
acf(usaseries_bu1, col='blue') 
# Comment: ACF plot indicates non-stationarity as it shows slow decay

# Dickey-Fuller test
library(urca)
df1 <- ur.df(usaseries_bu1, lag=0)
summary(df1)
# Comment: BU1 Time Series is non-Stationary, given DF output of -0.0605 > -1.95 (i.e, 5% critical value).

# Creating Time Series for BU2 and checking stationarity
usaseries_bu2 <- ts(usadata$BU2,start = c(2015,2), end = c(2018,1), frequency = 12)

# Simple Plot function
plot(usaseries_bu2, col='red', main='Sales Time Series (Simple Plot)')

# ACF plot
acf(usaseries_bu2, col='blue') 
# Comment: ACF plot indicates non-stationarity as it shows slow decay

# Dickey-Fuller test
library(urca)
df2 <- ur.df(usaseries_bu2, lag=0)
summary(df2)
# Comment: TS for BU2 is non-Stationary, given DF output of -0.0639 > -1.95 (i.e, 5% critical value).

# Creating TS for BU3 and check its stationarity
usaseries_bu3 <- ts(usadata$BU3,start = c(2015,2), end = c(2018,1), frequency = 12)

# Simple Plot function
plot(usaseries_bu3, col='red', main='Sales Time Series (Simple Plot)')

# ACF plot
acf(usaseries_bu3, col='blue') 
# Comment: ACF plot indicates non-stationarity as it shows slow decay

# Dickey-Fuller test
library(urca)
df3 <- ur.df(usaseries_bu3, lag=0)
summary(df3)
# Comment: BU3 Time Series is non-Stationary, given DF output of -0.0573 > -1.95 (i.e, 5% critical value).

# Q4: Obtain best model for each BU
# Making each BU to be Stationary using the Order of Differencing function
library(forecast)

usadiff1 <- diff(usaseries_bu1, differences = 3)
summary(ur.df(usadiff1,lags=0)) # Stationarity was achieved after 3rd Order

usadiff2 <- diff(usaseries_bu2, differences = 1)
summary(ur.df(usadiff2,lags=0)) # Stationarity was achieved after 1st Order

usadiff3 <- diff(usaseries_bu3, differences = 2)
summary(ur.df(usadiff3,lags=0)) # Stationarity was achieved after 2nd Order

# Model Identification and Parameter Estimation  for each BU using auto.arima()
usa_buI_model <- auto.arima(usaseries_bu1, trace=TRUE, ic='aic')
summary(usa_buI_model) # Best model for BU1 is ARIMA(2,1,2)

usa_bu2_model <- auto.arima(usaseries_bu2, trace=TRUE, ic='aic')
summary(usa_bu2_model) # Best model for BU2 is ARIMA(0,1,0)

usa_bu3_model <- auto.arima(usaseries_bu3, trace=TRUE, ic='aic')
summary(usa_bu3_model) # Best model for BU3 is ARIMA(0,1,0)

# Q5: Predict sales for each BU for January 2018, February 2018, March 2018(i.e., next 3 months)

forecast_bu1 <- predict(usa_buI_model, n.ahead = 3) 

forecast_bu2 <- predict(usa_bu2_model, n.ahead = 3)

forecast_bu3 <- predict(usa_bu3_model, n.ahead = 3)

# Forecasted data for each BU (example data)
forecast_bu1 <- c(109.28302, 99.71218, 99.08795)
forecast_bu2 <- c( 115.5, 115.5, 115.5)
forecast_bu3 <- c(113.8, 113.8, 113.8)

# Create a data frame with forecasted data
forecast_table <- data.frame(Year = rep(2018, 3),
                             Month = c("February", "March", "April"),
                             BU1 = forecast_bu1,
                             BU2 = forecast_bu2,
                             BU3 = forecast_bu3)

# Print the table of forecasts
print("Table of forecasts")
print(forecast_table)
