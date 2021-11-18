## Group Assignment ##

library("readxl")
library("forecast")
fancy<-read.csv(file.choose())
fancy
str(fancy)
class(fancy)


View(fancy)
plot(fancy)
summary(fancy$Sales)

modelLR_fancy<-lm(fancy$Sales~fancy$ï..Time, data = fancy)
summary(modelLR_fancy)

##Test for Linearity
plot(modelLR_fancy$fitted, modelLR_fancy$residuals)

#to check if errors are normally related
shapiro.test(modelLR_fancy$residuals)

## to check co-relation of errors
library(car)
library(lmtest)
durbinWatsonTest(modelLR_fancy)
dwtest(modelLR_fancy)

fancy_ts<-ts(fancy$Sales, start =c(1987,1) ,  end =c(1993,12) , frequency = 12)
View(fancy_ts)
fancy_ts
plot(fancy_ts)
class(fancy_ts)

##Decomposing data
decom_fancy<-decompose(fancy_ts)
plot(decom_fancy)

plot(diff(fancy_ts))

plot(diff(log(fancy_ts)))
library("tseries")
kpss.test(fancy_ts)
kpss.test(diff(fancy_ts))

kpss.test(diff(log(fancy_ts)))


## classifying into test and train

fancy_train<-window(fancy_ts, start=c(1987,1) , end = c(1992,12) , frequency =12 )
fancy_test<-window(fancy_ts, start=c(1993,1) , frequency =12 )
fancy_train
fancy_test
plot(fancy_test)
plot(fancy_train)

##making the training data stationary

plot(diff(fancy_train))
plot(diff(log(fancy_ts)))
abline(h=0)

library("xts")
periodicity(fancy_ts)
periodicity(fancy_train)


## Auto ARIMA
## Diff Log
autoa_fancy<-auto.arima(log(fancy_train), d=1, seasonal = TRUE)
autoa_fancy
preda_fancy<-predict(autoa_fancy, n.ahead = 12)
preda_fancy
preda_f_fancy<-2.718^(preda_fancy$pred)
preda_f_fancy
fancy_test

class(preda_fancy$pred)

class(fancy_test)

forecast::accuracy(preda_f_fancy,fancy_test)


###################Manual ARIMA ###################
plot(sin(fancy_train))
kpss.test(sin(fancy_train))

plot(diff(sin(fancy_train)))
abline(h=0)
kpss.test(diff(sin(fancy_train)))


plot(acf(sin(fancy_train))) #q=0
plot(pacf(sin(fancy_train))) #p=0

manual_a_sin<-arima((sin(fancy_train)), c(0,0,0), seasonal = list(order=c(0,0,0)))
manual_a_sin
pred_m_sin<-predict(manual_a_sin, n.ahead = 12)
pred_m_sin
write.csv(pred_m_sin, file = "sin.csv", sep = ",")

plot(acf(diff(sin(fancy_train)))) # q=1
plot(pacf(diff(sin(fancy_train)))) #p=3

manual_a_diffsin<-arima((sin(fancy_train)), c(2,1,1), seasonal = list(order=c(2,1,1)))
manual_a_diffsin
pred_m_diffsin<-predict(manual_a_diffsin, n.ahead = 12)
pred_m_diffsin
write.csv(pred_m_diffsin, file = "sin.csv", sep = ",")


##Diff Log


plot(acf(diff(log(fancy_train)))) ## q value is 2

plot(pacf(diff(log(fancy_train)))) ## p value is 2

Manual_A_fancy<-arima(log(fancy_train), c(2,1,2), seasonal = list(order=c(2,1,2)))
Manual_A_fancy
##prediction for 12 periods
pred_m_fancy<-predict(Manual_A_fancy, n.ahead = 12)
pred_m_fancy

## taking exponential values
predf_m_fancy<-2.718^pred_m_fancy$pred
predf_m_fancy

forecast::accuracy(predf_m_fancy,fancy_test)

