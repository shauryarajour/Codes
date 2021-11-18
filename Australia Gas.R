getwd()
setwd('C:/users/SHAURYA/Documents/R')
dir()
gas <-(gas)
head(gas)
str(gas)
print(gas)
plot(gas)
is.na(gas)
tsoutliers(gas)
ts.plot(gas)
ggseasonplot(gas)
ggsubseriesplot(gas)
gglagplot(gas)
ts_cor(gas)
plot(decompose(gas))
periodicity(gas)
train<-window(gas, start=c(1970,1),end=c(1993,12),frequency=12)
test<-window(gas,start=c(1994,1),frequency=12)
plot(train)
plot(test)
library(tseries)
adf.test(gas)
adf.test(train)
adf.test(diff(log(train)))
#Hypothesis for ADF test is#
#H0: Data is not stationary while Ha: Data is stationary#
plot(diff(log(train)))
ts_cor(train)
ts_cor(diff(log(train)))
AutoARIMA <- auto.arima(log(train), d=1)
library(Metrics)
Predict <- forecast(AutoARIMA,h = 20)
Predict
Predict2 <- forecast(AutoARIMA,h = 32)
Predict2
PredConv <- 2.718^Predict$mean
PredConv
PredConv2 <- 2.718^Predict2$mean
PredConv2
rmse(PredConv,test)
rmse(PredConv2, test)
mape(PredConv, test)
mape(PredConv2, test)
plot_forecast(Predict)
plot_forecast(Predict2)
forecast::accuracy(PredConv, test)
forecast::accuracy(PredConv2, test)
ManualARIMA <- arima(log(train), order = c(8,1,2), seasonal=list(order=c(0,1,2)))
Predict3 <- forecast(ManualARIMA,h = 20)
Predict3
Predict4 <- forecast(ManualARIMA,h = 32)
Predict4
PredConv3 <- 2.718^Predict3$mean
PredConv3
PredConv4 <- 2.718^Predict4$mean
PredConv4
Predict412P <- window(PredConv4, start=c(1995,9), end=c(1996,8), frequency = 12)
rmse(PredConv3,test)
rmse(PredConv4, test)
mape(PredConv3, test)
mape(PredConv4, test)
plot_forecast(Predict3)
plot_forecast(Predict4)
forecast::accuracy(PredConv3, test)
forecast::accuracy(PredConv4, test)
library(tseries)
seqplot.ts(gas,Predict412P)
