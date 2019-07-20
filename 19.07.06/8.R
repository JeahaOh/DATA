install.packages("TTR") 
library(TTR)
data(ttrc) 
class(ttrc)
str(ttrc)
sma.20 <- SMA(ttrc[,"Close"], 20)
ema.20 <- EMA(ttrc[,"Close"], 20) 
wma.20 <- WMA(ttrc[,"Close"], 20)  



(x <- seq(-2 * pi, 2*pi, 0.1) )
amp.1 <- 2 
amp.2 <- 2 
amp.3 <- 5 
amp.4 <- 5
# 주기(wave-length, cycle) 1
wav.1 <- 1 
wav.2 <- 2 
wav.3 <- 3 
wav.4 <- 7 
signal.1 <- amp.1*sin(wav.1*x) 
signal.2 <- amp.2*sin(wav.2*x) 
signal.3 <- amp.3*sin(wav.3*x) 
signal.4 <- amp.4*sin(wav.4*x) 
par(mfrow = c(1,4))

plot(x, signal.1, type='l', ylim=c(-5,5)); abline(h=0, lty=3) 
plot(x, signal.2, type='l', ylim=c(-5,5)); abline(h=0, lty=3) 
plot(x, signal.3, type='l', ylim=c(-5,5)); abline(h=0, lty=3) 
plot(x, signal.4, type='l', ylim=c(-5,5)); abline(h=0, lty=3)


a <- ts(1:20, frequency = 12, start = c(2011, 3))
print(a)
str(a)
attributes(a)
kings <- scan( "kings.dat")   
class(kings)
kingstimeseries <- ts(kings)
kingstimeseries
births <- scan("nybiths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
souvenir <- scan("fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries
plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)
kingsSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingsSMA3)
kingsSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingsSMA8)


library(dtw)
idx <- seq(0, 2 * pi, len = 100)
(a <- sin(idx) + runif(100)/10)
(b <- cos(idx))

align <- dtw(a, b, step = asymmetricP1, keep = T) 
print(align)
dtwPlotTwoWay(align)
plot(align$index1,align$index2,main="Warping function")
align$distance



################### 비행기 승객 데이터 
data(AirPassengers)
class(AirPassengers) 
start(AirPassengers) # 1949-1960
end(AirPassengers)
frequency(AirPassengers)  
summary(AirPassengers)
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers) 
plot(aggregate(AirPassengers,FUN=mean))
boxplot(AirPassengers~cycle(AirPassengers))
 
adf.test(AirPassengers, alternative="stationary", k=0)
adf.test(log(AirPassengers), alternative="stationary", k=0)
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)
acf(log(AirPassengers))
acf(diff(log(AirPassengers))) 
pacf(diff(log(AirPassengers))) 
auto.arima(diff(log(AirPassengers))) 
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))


pred <- predict(fit, n.ahead = 10*12)

ts.plot(AirPassengers, 2.718^pred$pred, log = "y", lty = c(1,3))
# 1949 ~ 1960 
air.model <- Arima(window(AirPassengers,end=1956+11/12),order=c(0,1,1),
                   seasonal=list(order=c(0,1,1),period=12),lambda=0)
plot(forecast(air.model, h=48))