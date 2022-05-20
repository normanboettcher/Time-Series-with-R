
global <- scan('global.dat')

# (1) plot the time series
global.ts <- ts(global, start = c(1856,1), end = c(2005,12), frequency = 12)
plot(global.ts, ylab= 'temperature', xlab = 'Time',
     main = 'Mean monthly temperature anomalies from January 1856 to December 2005')

#(2) Use the aggregate function to remove any seasonal effects within each
# year and plot an annual series (called {xt}) of mean temperatures for
# the period 1856-2005

global.ts.annual <- aggregate(global.ts) / 12
plot(global.ts.annual, ylab = 'temperature', xlab = 'time',
     main = 'Mean annual temperature anomalies series from January 1856 to
     December 2005')
# (3)
t <- time(global.ts.annual)
global.lm <- lm(global.ts.annual ~ t)
coef(global.lm)
# (4) Use the command abline to draw a regression line to the existing plot
#in step (2).
abline(global.lm, col = 'red')

#(5) plot the time series rt
plot(resid(global.lm), type = 'l', main='Time series rt')

#(6)

library(randtests)
rt <- resid(global.lm)
turning.point.test(rt)

#Box-Pierce and Ljung-Box Test
for(i in 1:24){
  box_pierce <- Box.test(rt, lag = i, type = 'Box-Pierce')
  print(box_pierce)
}


for(i in 1:24) {
  box_ljung <- Box.test(rt, lag = i, type = 'Ljung-Box')
  print(box_ljung)
}


# difference sign test
difference.sign.test(rt)

