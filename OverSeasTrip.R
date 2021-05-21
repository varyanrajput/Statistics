library(fpp2)
library(tseries)
library(ggplot2)

tripdata=read.csv('C:\\Users\\varya\\OneDrive\\Desktop\\Stats TA\\OverseasTrips.csv')
tripdata
tripdata <- ts(tripdata['Trips.Thousands.'], start=c(2012, 1), frequency=4)
autoplot(tripdata)

monthplot(tripdata) 
seasonplot(tripdata)
plot(decompose(tripdata, type = "multiplicative"))
adf.test(tripdata)
kpss.test(tripdata)


ggseasonplot(tripdata)+
  ylab("Trips in Thousands")+
  ggtitle("Season Plot of Overseas Trips data")


#seasonal naive model
fcast.seasonalnaive<-snaive(tripdata,h=3)
summary(fcast.seasonalnaive)
accuracy(fcast.seasonalnaive)
plot(fcast.seasonalnaive)
Box.test(fcast.seasonalnaive$residuals, type="Ljung-Box")


adf.test(tripdata)
kpss.test(tripdata)

#Computation For Arima
ndiffs(tripdata)
nsdiffs(tripdata)
difdata1<-diff(tripdata)
plot(difdata1)
adf.test(difdata1)
ggtsdisplay(tripdata)
#Implementing Arima
aamodel<-auto.arima(tripdata)
aamodel
tripdata%>%auto.arima()%>%accuracy
fcast <- forecast(aamodel, h=3)
fcast
plot(fcast)

plot(forecast(aamodel))

accuracy(aamodel)
summary(aamodel)
checkresiduals(aamodel)
Box.test(aamodel$residuals, type="Ljung-Box")
qqnorm(aamodel$residuals)
qqline(aamodel$residuals)

#ETS-MNM
fit4 = ets(tripdata,model = 'MNM')
plot(fit4)
summary(fit4)

#ETS-ZZZ
fit5 = ets(tripdata,model = 'ZZZ')
plot(fit5)
summary(fit5)
forecast(fit5, 3)
plot(gg)
Box.test(fit5$residuals, type="Ljung-Box")
checkresiduals(fit5)
qqnorm(fit5$residuals)
qqline(fit5$residuals)

cbind('Residuals' = residuals(fit5),
      'Forecast errors' = residuals(fit5,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")
