setwd('/home/norman/Schreibtisch/Time Series Analysis R/tsa03/')
www <- 'cbe.dat'
CBE <- read.table(www, header = TRUE)
class(CBE)
choc.ts <- ts(CBE$choc, start = c(1958, 1), end = c(1990,12), frequency = 12)
choc.decom <- decompose(choc.ts, type = 'multiplicative')
plot(choc.decom)

plot(ts(choc.decom$random[13:396]))

choc.ts.58.89 <- ts(CBE$choc, start = c(1958,1), end=c(1989,12), frequency = 12)
sd(choc.ts.58.89)
sd(choc.ts[13:384] - choc.decom$trend[13:384])
sd(choc.decom$random[13:384])
