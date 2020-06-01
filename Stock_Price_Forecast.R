#----------------------------------------------------------------
#*********R File for ETF Stock Price Prediction******************
#----------------------------------------------------------------
#Installing Packages and Loading Libraries

install.packages("tseries")
install.packages("forecast")
install.packages("ggplot2")

library(tseries)
library(forecast)
library(ggplot2)

#Reading Data
Stock<-read.csv(file.choose(),sep=',',header= TRUE)
View(Stock)
head(Stock)

#Converting List into a Data Frame for Data Visualization
daily <- data.frame(Stock$Date,Stock$Open)
colnames(daily) <- c("Date","Open")
head(daily)

daily[rowSums(is.na(daily))==0,]
plot(daily$Date,daily$Open,typ='l')

#Plotting Stock Prices per Date

library(ggplot2)
ggplot(daily, aes(x=daily$Date, y= daily$Open,color="blue")) + geom_line() +
  scale_x_date('Daily')  +
  ylab("Stock Prices") +
  xlab("")

## ------------------------------------------------------
## Predicting the market for Open values
##-------------------------------------------------------
#Removing outliers from the market
count_ts = ts(daily[,c('Open')])
daily$clean_open = tsclean(count_ts)
ggplot() +geom_line(data = daily, aes(x = daily$Date, y = daily$clean_open,color='blue')) +
  ylab('Cleaned Prices') +xlab('Date')

#Calculating Moving Averages
daily$Open_ma7 = ma(daily$Open,order=7) #Weekly Moving Average
daily$Open_ma30 = ma(daily$Open,order=30) #Monthly Moving Average

View(daily$Open_ma7)
View(daily$Open_ma30)
tail(daily$Open_ma7)

#Plotting Weekly and Monthly Moving Averages
ggplot() +
  geom_line(data = daily, aes(x = daily$Date, y = daily$Open_ma7)) +
  ylab('Weekly Moving Average') + xlab('Date')

ggplot()+geom_line(data = daily, aes(x = daily$Date, y = daily$Open_ma30)) +
  ylab('Monthly Moving Average')+ xlab('Date')

#Plotting MOving Averages along with the Open Values
ggplot() +
  geom_line(data = daily, aes(x = daily$Date, y = daily$Open,
                              colour = "Open Values"))+
  geom_line(data = daily, aes(x = daily$Date, y = daily$Open_ma7,
                              colour = "Weekly Moving Average"))+
  geom_line(data = daily, aes(x = daily$Date, y = daily$Open_ma30,
                              colour = "Monthly Moving Average"))+
  ylab('Stock Price with averages')+xlab('Time')

#Decompose data for Seasonal, trend and other components using loess

count_ma = ts(na.omit(daily$Open_ma7), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

count_ma30 = ts(na.omit(daily$Open_ma30), frequency=30)
decomp30 = stl(count_ma, s.window="periodic")
deseasonal_cnt30 <- seasadj(decomp)
plot(decomp30)

#ADF Test for Open Values
adf.test(daily$Open, alternative = "stationary")

#ADF Test for Monthly Moving Average
adf.test(count_ma30, alternative = "stationary")

#ADF Test for weekly moving average
adf.test(count_ma, alternative = "stationary")

acf(count_ma,main = "")
pacf(count_ma )

#ADF Test for Differences of weekly moving average
count_d1 = diff(deseasonal_cnt, differences = 2)

adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#ARIMA model for c(1,1,1)
try <- arima(deseasonal_cnt ,order = c(1,1,1))
summary(try)

#Using auto.arima to pick the best ARIMA model
auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(2,1,3) Model residuals')

#Since we observe that there is high value for lag=15

fit2 = arima(count_d1, order=c(2,1,3))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model residuals')

#Forecasting the value
fcast <- forecast(fit, h=200)
plot(fcast)
head(fcast)
View(fcast)

#Compare with actual values
hold <- window(ts(deseasonal_cnt), start=700)
tail(hold)
tail(data)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=100)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt),col="red")

#with Seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality)
plot(seas_fcast)
lines(ts(deseasonal_cnt),col="red")
tail(seas_fcast)

save(list = ls(all.names = TRUE), file = ".RData", envir = .GlobalEnv)

############################################################################
