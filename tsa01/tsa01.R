#Exercise 01
#read cbe data
data = 'cbe.dat'
cbe <- read.table(data, header=TRUE)
attach(cbe)

#create time series objects according to the columns
ts_choc <- ts(cbe$choc, start =c(1958,1), end=c(1990,12), frequency = 12)
ts_beer <- ts(cbe$beer, start =c(1958,1), end=c(1990,12), frequency = 12)
ts_elec <- ts(cbe$elec, start =c(1958,1), end=c(1990,12), frequency = 12)
#plot the three time series in one figure
plot(cbind(ts_beer, ts_elec, ts_choc))

#(b) decompose time series into three parts estimating trends, seasonal effects,
#random series
dec_choc <- decompose(ts_choc)
plot(dec_choc)

dec_elec <- decompose(ts_elec)
plot(dec_elec)

dec_beer <- decompose(ts_beer)
plot(dec_beer)

#Exercise 2
global_data <- scan('global.dat')
ts_global <- ts(global_data, start = c(1856,1), end=c(2005,12), frequency = 12)
plot(ts_global, ylab = "temperature", main ="Mean annual temperature series from
     January 1856 to December 2005")

#(b)
#Use the "aggregate" function to remove any seasonal effects within
#each year and produce an annual series of mean temperatures for the
#period 1856-2005.
global.annual <- aggregate(ts_global) / 12
plot(global.annual, ylab = "temperature", main ="Mean annual temperature series from
     January 1856 to December 2005")

#Exercise 3
library(xlsx)
library(stats)
library(timeSeries)

path <- 'cases of coronavirus.xlsx'
data.corona <- read.xlsx(path, sheetName = 'Data', header = T)
attach(data.corona)

cases <- data.corona$Number
cases.asDates <- as.Date(as.character(data.corona$Date), "%m/%d/%y")
ts_corona <- timeSeries(cases, cases.asDates)
plot(ts_corona, ylab='Number', xlab='time', main='Daily Increase of new cases
     of corona from January 20, 2020 to April 8, 2022, USA')

cases.increase <- diff(ts_corona, lag = 1)
plot(cases.increase, ylab = "Number", xlab="Time",
     main = "Daily increase of new cases in US
     from January 21, 2020 to April 8, 2022")
#(c) calc moving average of order 7 and incidence
order7 <- rep(1/7, 7)
order7

#moving average
mut <- filter(cases.increase, order7, method = "c", sides = 1)


cases.inc.ma<-cbind(cases.increase,mut)

plot(cases.inc.ma,plot.type="single")
incidence <- mut/331002651 * 100000 * 7

# incidence per 100.000
plot(incidence, ylab = "Incidence", xlab="Time",
     main = "Incidence of cases in the past seven days
             per 100,000 population")


