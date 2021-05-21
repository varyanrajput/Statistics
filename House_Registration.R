install.packages('fpp2')
install.packages('tseries')
library(fpp2)
library(tseries)
library(ggplot2)
#attaching data_set
data=read.csv('C:\\Users\\varya\\OneDrive\\Desktop\\Stats TA\\NewHouseRegistrations_Ireland.csv')
data
tdata <- ts(data['NewHouseRegistrations'], start=1978)

autoplot(tdata)


#mean model
fcast.mean<-meanf(tdata,h=3)
summary(fcast.mean)
plot(fcast.mean)

#naive model
fcast.naive<-naive(tdata,h=3)
summary(fcast.naive)
plot(fcast.naive)

#SES Model
fcast.ses<-ses(tdata, h=3)
plot(fcast.ses)
summary(fcast.ses)

#Arima
ndiffs(tdata)
adf.test(tdata)
kpss.test(tdata)

acf(tdata)
pacf(tdata)
ggtsdisplay(tdata)
#Fit the model using R and examine the residuals. Is the model satisfactory?
fit <- Arima(tdata, c(2,0,0))
fcast <- forecast(fit, h=3)
fcast
plot(fcast)
accuracy(fit)
summary(fit)
checkresiduals(fit)
Box.test(fit$residuals, type="Ljung-Box")
qqnorm(fit$residuals)
qqline(fit$residuals)


#Use auto.arima to fit model and check RMSE
aamodel<-auto.arima(tdata)
aamodel
tdata%>%auto.arima()%>%accuracy


#Smoothening
autoplot(tdata)+
  autolayer(ma(tdata,3))+
  autolayer(ma(tdata,5))
ndata <- ma(tdata,3)
ndiffs(ndata)

fit <- auto.arima(ndata)
fcast <- forecast(fit, h=3)
fcast
plot(fcast)
accuracy(fit)
summary(fit)
checkresiduals(fit)
Box.test(aamodel$residuals, type="Ljung-Box")
qqnorm(fit$residuals)
qqline(fit$residuals)