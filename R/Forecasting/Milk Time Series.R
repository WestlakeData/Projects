#Install Packages
#install.packages("forecast")

#Load Libraries
library(forecast)

#Load Data set
milk <- read.csv("./data/monthly-milk-production-pounds-p.csv")
milk <- na.omit(milk)

#Convert to Time Series
milk <- ts(milk[,2]) 

#Examine Plot
plot(milk, xlab = "Period (months)",
     ylab = "Monthly Milk Production (pounds per cow)")

#Examine removing upward trend
plot(diff(milk))
abline(a=0, b=0)

# examine ACF and PACF of stationary time series
acf(diff(milk), xaxp = c(0, 48, 4), lag.max=48, main="")
pacf(diff(milk), xaxp = c(0, 48, 4), lag.max=48, main="")

#ACF indicates seasonal periodicity of 12 months

#Construct ARIMA Model
arima_1 <- arima (milk,
                  order=c(0,1,0),
                  seasonal = list(order=c(1,0,0),period=12))
arima_1

acf(arima_1$residuals, xaxp = c(0, 48, 4), lag.max=48, main="")
pacf(arima_1$residuals, xaxp = c(0, 48, 4), lag.max=48, main="")

#PACF shows a remaining period at lag = 12

#Construct 2nd ARIMA Model to deal with PACF period
arima_2 <- arima (milk,
                  order=c(0,1,1),
                  seasonal = list(order=c(0,1,1),period=12))
arima_2

acf(arima_2$residuals, xaxp = c(0, 48, 4), lag.max=48, main="")
pacf(arima_2$residuals, xaxp = c(0, 48, 4), lag.max=48, main="")

#Examine the distribution of the residuals
plot(arima_2$residuals, ylab = "Residuals")
abline(a=0, b=0)

hist(arima_2$residuals, xlab="Residuals", xlim=c(-20,20))

qqnorm(arima_2$residuals, main="")
qqline(arima_2$residuals)

#Forcast the Time Series forward 20 periods
arima_2.predict <- predict(arima_2,n.ahead=20)
matrix(c(arima_2.predict$pred-1.96*arima_2.predict$se,
         arima_2.predict$pred,
         arima_2.predict$pred+1.96*arima_2.predict$se), 12,3,
       dimnames=list( c(241:252) ,c("LB","Pred","UB")) )

jpeg("./images/forecast.jpg")
#Base Plot
plot(milk, xlab = "Time (months)", ylab = "Monthly Milk Production (pounds per cow)", xlim = c(0,185), ylim = c(500, 1100))
#Forcast Line
lines(arima_2.predict$pred)
#SE Lines
lines(arima_2.predict$pred+1.96*arima_2.predict$se, col=4, lty=2)
lines(arima_2.predict$pred-1.96*arima_2.predict$se, col=4, lty=2)

dev.off()

#Zoom in on forecast
jpeg("./images/forecast_zoom.jpg")
#Base Plot
plot(milk, xlab = "Time (months)", ylab = "Monthly Milk Production (pounds per cow)", xlim = c(150,190), ylim = c(700, 1100))
#Forcast Line
lines(arima_2.predict$pred)
#SE Lines
lines(arima_2.predict$pred+1.96*arima_2.predict$se, col=4, lty=2)
lines(arima_2.predict$pred-1.96*arima_2.predict$se, col=4, lty=2)

dev.off()
