#Exercise 1

#(a) import time series and plot
library(xlsx)
library(timeDate)
library(timeSeries)
corona <- read.xlsx('cases of coronavirus Germany_long.xlsx', sheetName = 'Daten',
                          header=TRUE)

attach(corona)
xt_corona <- corona$NewCases
corona_dates <- corona$Date
length(xt_corona)
n <- 760
plot(xt_corona, ylab ="Neuinfektionen", xlab ="Time",
     main = "New Infections of corona in germany from January 28th 2020
     to October 31th 2021", type = "l", xaxt="n")
axis(1, at=(seq(n)), labels = corona_dates)
#Exercise 2 Linear Regression
cbe <- read.table('../tsa01/cbe.dat', header = TRUE)
attach(cbe)
#create time series objects according to the columns
ts_choc <- ts(cbe$choc, start =c(1958,1), end=c(1990,12), frequency = 12)
ts_beer <- ts(cbe$beer, start =c(1958,1), end=c(1990,12), frequency = 12)
ts_elec <- ts(cbe$elec, start =c(1958,1), end=c(1990,12), frequency = 12)
#plot the three time series in one figure
plot(cbind(ts_beer, ts_elec, ts_choc))

#(a) Use the aggregate function to remove any seasonal effects within each
#year and produce the annual series for the period 1958-1990
choc_agg <- aggregate(ts_choc) / 12
beer_agg <- aggregate(ts_beer) / 12
elec_agg <- aggregate(ts_elec) / 12
plot(cbind(choc_agg, beer_agg, elec_agg),
     main="annual series for the period 1958 - 1990 without seasonal effects")

#(b) Use the window function to plot the data from January 1958 to December 1988
beer_58_88 <- window(ts_beer, start=c(1958,1), end = c(1988,12))
elec_58_88 <- window(ts_elec, start=c(1958,1), end = c(1988,12))
choc_58_88 <- window(ts_choc, start=c(1958,1), end=c(1988,12))
plot(cbind(beer_58_88, elec_58_88, choc_58_88),
     main = "plot from 1958 to 1988 with window function")

#(c) and (d)Using the command lm to estimate the parameters ˆα and ˆβ in the
#simple linear regression model.
#use summary and abline function

#define time parameters for each time series
time.beer <- time(beer_58_88)
time.elec <- time(elec_58_88)
time.choc <- time(choc_58_88)

#apply the lm model to each time series to estimate the coefficents
lm.beer <- lm(beer_58_88~time.beer)
lm.elec <- lm(elec_58_88~time.elec)
lm.choc <- lm(choc_58_88~time.choc)

#now print the estimated coefficients
summary(lm.beer)
summary(lm.elec)
summary(lm.choc)

#plot the time series with corresponding regression line
plot(cbind(beer_58_88, elec_58_88, choc_58_88))

plot(beer_58_88)
abline(lm.beer, col='red')

plot(elec_58_88)
abline(lm.elec, col ='red')

plot(choc_58_88)
abline(lm.choc, col ='red')

