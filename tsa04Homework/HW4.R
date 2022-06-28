setwd('~/Schreibtisch/Time Series Analysis R/')
data <- 'tsa03/cbe.dat'
data <- read.table(data, header = TRUE)

xt <- ts(data$elec, start = c(1958,1), end = c(1990,12), frequency = 12)
yt <- log(xt)

plot(xt, main="Time Series of xt")
plot(yt, main ="Time Series of yt = log xt")

library(tseries)
adf.test(diff(xt, lag = 12))
kpss.test(diff(xt, lag=12))

#p-value is smaller than 0.05. =>  assume that xt comes from integrated process
#choosing d = 1

#Then choose p, q, P, Q ∈ {0, 1} according to the best AIC for the logarithm of the original series

AIC(arima(yt, order = c(0,1,0), seasonal = list(order = c(1,1,0), 12)))
AIC(arima(yt, order = c(0,1,0), seasonal = list(order = c(0,1,1), 12)))
AIC(arima(yt, order = c(0,1,0), seasonal = list(order = c(0,1,0), 12)))
AIC(arima(yt, order = c(0,1,0), seasonal = list(order = c(1,1,1), 12)))

AIC(arima(yt, order = c(1,1,0), seasonal = list(order = c(1,1,0), 12)))
AIC(arima(yt, order = c(1,1,0), seasonal = list(order = c(0,1,1), 12)))
AIC(arima(yt, order = c(1,1,0), seasonal = list(order = c(0,1,0), 12)))
AIC(arima(yt, order = c(1,1,0), seasonal = list(order = c(1,1,1), 12)))


AIC(arima(yt, order = c(0,1,1), seasonal = list(order = c(1,1,0), 12)))
AIC(arima(yt, order = c(0,1,1), seasonal = list(order = c(0,1,1), 12)))
AIC(arima(yt, order = c(0,1,1), seasonal = list(order = c(0,1,0), 12)))
AIC(arima(yt, order = c(0,1,1), seasonal = list(order = c(1,1,1), 12)))


AIC(arima(yt, order = c(1,1,1), seasonal = list(order = c(1,1,0), 12)))
AIC(arima(yt, order = c(1,1,1), seasonal = list(order = c(0,1,1), 12)))
AIC(arima(yt, order = c(1,1,1), seasonal = list(order = c(0,1,0), 12)))
AIC(arima(yt, order = c(1,1,1), seasonal = list(order = c(1,1,1), 12)))

#smallest AIC = - 1873.532 at ARIMA (0,1,1)(1,1,1)

fittet.model <- arima(yt, order=c(0,1,1), seasonal = list(order=c(1,1,1), 12))

fittet.model

fittet.model.resids <- fittet.model$residuals

predict(fittet.model)

#lot the correlogram of the residuals of the best fitted ARIMA process.
#Comment on that.
acf(fittet.model.resids)
#the residuals of fitted model are approximately white noise. We can assume that it could
#be the best fitting ARIMA model.