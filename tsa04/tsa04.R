
### Exercise 1
### (1) 
CBE <- read.table("cbe.dat", header = TRUE)


choc.ts <- ts(CBE$choc, start = c(1958,1), 
              end = c(1990,12), frequency = 12)

plot(choc.ts, main = "Australian chocolate production 
     from January 1958 to December 1990")

##(2) 
choc.annual <- aggregate(choc.ts)

plot(choc.annual, 
     main = "Annual chocolate production from 1958 to 1990")

## (3) 
choc.annual.log <- log(choc.annual)

plot(choc.annual.log, 
     main = "Logarithm of the annual chocolate production 
     from 1958 to 1990")

## (4)
TIME <- time(choc.annual.log)

choc.log.lm <- lm(choc.annual.log~TIME)

coef(choc.log.lm)

abline(choc.log.lm)

## (5)

library(randtests)

turning.point.test(resid(choc.log.lm))

for(i in 1:10){ print(Box.test(resid(choc.log.lm), lag = i, type = "Box-Pierce"))}  ## print-command needed to ensure printing

for(i in 1:10){ print(Box.test(resid(choc.log.lm), lag = i, type = "Ljung-Box"))}  

difference.sign.test(choc.annual.log)

## (6)
acf(resid(choc.log.lm), lag.max = 32)

