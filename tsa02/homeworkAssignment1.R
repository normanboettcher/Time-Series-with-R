library(xlsx)
#(a) read the data and plot the time series
mobile.data <- read.xlsx('statistic-mobile-communications.xlsx',
                         sheetName = 'Data', header=T)
attach(mobile.data)
ts.mobile.data <- ts(mobile.data$CPI, start=c(2005,1), end=c(2018,12), frequency = 12)
plot(ts.mobile.data, main="Consumer price for mobile communication in
     germany from 2005 to 2018", ylab = "CPI")

#(b) decompose the time series into three parts: estimating trends, seasonal
#effects, and random series.
ts.mobile.data.decompose <- decompose(ts.mobile.data)
plot(ts.mobile.data.decompose)

#(c)Use the aggregate function to remove any seasonal effects within each
#year and produce an annual series of mean CPI for the period 2005-2018
ts.mobile.annual <- aggregate(ts.mobile.data) / 12
plot(ts.mobile.annual, main ="Annual Consumer price for mobile communication in germany
     from 2005 to 2018", ylab="CPI")

#(d) Use the window function to plot the data from January 2005 to December 2017
help("window")
ts.mobile.window <- window(ts.mobile.data, start=c(2005,1), end=c(2017,12))
plot(ts.mobile.window, main="data from january 2005 to december 2017", ylab="CPI")

#(e) Use the command lm to estimate the parameters ˆα and ˆβ in the simple
#linear regression model.
#(f) Use commands summary and abline to add lines to existing plots in
#step (e)
linearRegressionModel <- function(data) {
  t <- time(data)
  reg <- lm(data~t)
  print("========coefficients for Linear Regression Model:")
  print(reg)
  print("===========Summary of Linear Regression Model:")
  print(summary(reg))

  plot(data, ylab="CPI", main ="Regression Plot for CPI in germany from january
       2005 to december 2017")
  abline(reg, col="red")
}
#apply regression model
linearRegressionModel(ts.mobile.window)

########################
#PREDICTION BY HAND (g)
########################

#######################JUNE 2018###################
#ŷ(2018) = a + bt = 5694.604 -2.782 * (2018+5/12) = 34,369

#######################JUNE 2019###################
#ŷ(2019) = a + bt = 5694.604 -2.782 * (2019+5/12) = 31,587

#######################JUNE 2020###################
#ŷ(2020) = a + bt = 5694.604 -2.782 * (2020+5/12) = 28,805

#(h)

#Holt-Winters
mobile.hw <- HoltWinters(ts.mobile.window)
plot(mobile.hw$fitted)
summary(mobile.hw)

#(j) make predictions using holt-winters
predict(mobile.hw, n.ahead = 36)

