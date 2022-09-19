## The example of decayed pulse function

#libraries
rm(list=ls())
library(foreign)
library(TSA)
library(UsingR) # for BushApproval
data(BushApproval)
str(BushApproval)
head(BushApproval)
plot(y=bush$approve, x=bush$t, type='l')

#load data & view series
bush <- read.dta("BUSHJOB.DTA")
names(bush)
print(bush)

plot(y=bush$approve, x=bush$t, type='l')

#identify arima process
acf(bush$approve)
pacf(bush$approve)

#estimate arima model
mod.1 <- arima(bush$approve, order=c(0,1,0))
mod.1

#diagnose arima model
acf(mod.1$residuals)
pacf(mod.1$residuals)
Box.test(mod.1$residuals)

#Looks like I(1)
#estimate intervention analysis
mod.2 <- arimax(bush$approve, order=c(0,1,0), xtransf=bush$s11, transfer=list(c(1,0)))
mod.2
summary(mod.2)

#Our parameter estimates look good, no need to drop delta or switch to a step function.

#Graph the intervention model
y.diff <- diff(bush$approve)
t.diff <- bush$t[-1]
y.pred <- 24.3741*bush$s11 + 24.3741*(.9639^(bush$t-9))*as.numeric(bush$t>9)
y.pred <- y.pred[-1]
plot(y=y.diff, x=t.diff, type='l')
lines(y=y.pred, x=t.diff, lty=2)

#suppose an AR(1) process
mod.2b <- arimax(bush$approve, order=c(1,0,0), xtransf=bush$s11, transfer=list(c(1,0))); 
mod.2b
y.pred <- 58.2875 + 23.6921*bush$s11 + 23.2921*(.8915^(bush$t-9))*as.numeric(bush$t>9)
plot(y=bush$approve, x=bush$t, type='l')
lines(y=y.pred, x=bush$t, lty=2)