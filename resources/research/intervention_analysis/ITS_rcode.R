#### 1. loading data
setwd("G:/Study/2019년 1-1/고급통계분석/R 시계열 분석 입문/data")
final <- read.table("GyungBu.txt", header=T) # 1호선 순서대로 정리된 자료.

colnames(final) <- c("1_서울","2_남영","3_용산","4_노량진","5_대방","6_신길",
                     "7_영등포","8_신도림","9_구로","10_가산디지털단지","11_독산",
                     "12_금천구청.시흥.","13_석수","14_광명","15_관악","16_안양","17_명학",
                     "18_금정","19_군포","20_당정","21_의왕","22_성균관대","23_화서","24_수원",
                     "25_세류","26_병점","27_서동탄","28_세마","29_오산대","30_오산",
                     "31_진위","32_송탄","33_서정리","34_지제","35_평택",
                     "36_성환","37_직산","38_두정","39_천안","합계")

#### 2. 개입분석을 고려해볼만한 역들 중 3개정도를 뽑음.

### (1) 서울역
ts.plot(ts(final[,"1_서울"],start=c(2005,1),freq=12), 
        main = "서울역 - 2015년쯤 급격히 하락", ylab="수송량")
abline(v=2015, col="red")
arrows(2014, 800000, 2015, 800000, col="red")
text(2013.3,800000,"2015년 급격히 하락",col="red")
abline(v=2015.45, col="violet")
arrows(2013.45, 900000, 2015.45, 900000, col="violet")
text(2012.5,900000,"2015년 5월쯤부터 증가추세 ",col="violet")


### (2) 석수역
ts.plot(ts(final[,"13_석수"],start=c(2005,1),freq=12), 
        main = "석수역 - 2010년쯤이후로 급격히 증가", ylab="수송량")
abline(v=2010, col="red")
arrows(2009, 475000, 2010, 475000, col="red")
text(2008,475000,"2010년쯤부터 급격히 증가",col="red")


### (3) 광명역
ts.plot(ts(final[,"14_광명"],start=c(2005,1),freq=12), 
        main = "광명역 - 급격히 하락후, 다시 급격히 증가", 
        ylab="수송량")
abline(v=2013.8, col="red")
arrows(2012.8, 100000, 2013.8, 100000, col="red")
text(2011.8,100000,"2013년 9월쯤부터 급격히 하락",col="red")
abline(v=2016.8, col="violet")
arrows(2015.8, 80000, 2016.8, 80000, col="violet")
text(2014.8,80000,"2016년 9월쯤부터 급격히 증가",col="violet")



#### 3. 예측및 모형평가
library(forecast) # For Arima(), forecast(). Arima() allows a drift term
library(strucchange) # For breakpoints().
library(lmtest) # For coeftest().

### (1) 서울역
index <- which(colnames(final)=="1_서울")
Seoul <- ts(final[,index], start=c(2005,1), frequency = 12)
cut <- window(Seoul, start=c(2015,5), freq=12) # 잘라서 예측
dec.ord <- window(Seoul, end=c(2014,12), freq=12) # to decide order.

## Arima models
fit1.Seoul <- auto.arima(Seoul,ic="aicc")
summary(fit1.Seoul)

fit2.Seoul <- auto.arima(cut, ic="aicc")
summary(fit2.Seoul)

## with Intervention Analysis
summary(lm(Seoul ~ 1)) # the intercept is reported as significative.
# Let`s see if there are any structural breaks
break_point <- breakpoints(Seoul ~ 1)
summary(break_point)
plot(break_point) # The BIC minimum value is reached when m=2

par(mfrow=c(2,1))

ts.plot(Seoul, main = "서울역- break points 2개")
fitted.ts <- fitted(break_point, breaks=2)
lines(fitted.ts, col = "red")
lines(confint(break_point, breaks=2))

ts.plot(Seoul, main = "서울역- break point 1개") # I guess m=1 is the best.
fitted.ts <- fitted(break_point, breaks=1)
lines(fitted.ts, col = "red")
lines(confint(break_point, breaks=1))

par(mfrow=c(1,1))

# The fitted multiple level shifts(as determined by the structural breaks analysis) can be used
# as intervention variable to fit an ARIMA model, as shown below
# autoplot(fitted.ts)
fit3.Seoul <- auto.arima(Seoul, xreg=fitted.ts, ic="aicc")
summary(fit3.Seoul)

temp <- auto.arima(dec.ord, ic="aicc")
summary(temp)
fit4.Seoul <- Arima(Seoul, order=c(2,0,0), seasonal=list(order=c(2,1,1)), include.drift = TRUE,
                    xreg=fitted.ts)
summary(fit4.Seoul)
round(coeftest(fit3.Seoul),3)
round(coeftest(fit4.Seoul),3)

## plot
par(mfrow=c(4,1))
round(accuracy(forecast(fit1.Seoul)),4)
plot(forecast(fit1.Seoul,12*10),ylim=c(700000,2100000),
     main="서울역 향후 10년치 예측 \nARIMA(1,1,0)(2,0,0)[12]")

round(accuracy(fit2.Seoul),4)
plot(forecast(fit2.Seoul,12*10), ylim=c(700000,2100000), xlim=c(2005,2029),
     main="서울역 향후 10년치 예측 \nARIMA(0,0,0)(1,1,0)[12] with drift")
abline(v=2015.4, col="red")
arrows(2014, 1600000, 2015.2, 1600000, col="red")
text(2012,1600000,"2015년 5월자료부터 이용",col="red")

round(accuracy(forecast(fit3.Seoul, xreg = fitted.ts)),4)
plot(forecast(fit3.Seoul, xreg=rep(fitted.ts[168],12*10), h=12*10),ylim=c(700000,2100000),
     main="서울역 10년 예측 \nRegression with ARIMA(1,0,2)(2,1,0)[12] errors")

round(accuracy(forecast(fit4.Seoul, xreg = fitted.ts)),4)
plot(forecast(fit4.Seoul, xreg=rep(fitted.ts[168],12*10), h=12*10),ylim=c(700000,2100000),
     main="서울역 10년 예측 \nRegression with ARIMA(2,0,0)(2,1,1)[12] errors")

par(mfrow=c(1,1))


### (2) 석수역
index <- which(colnames(final)=="13_석수")
Seoksu <- ts(final[,index], start=c(2005,1), frequency = 12)
cut <- window(Seoksu, start=c(2012), freq=12) # 잘라서 예측
dec.order <- window(Seoksu, end=c(2009,12), freq=12) # to decide order.

## Arima models
fit1.Seoksu <- auto.arima(Seoksu,ic="aicc")
summary(fit1.Seoksu)

fit2.Seoksu <- auto.arima(cut, ic="aicc")
summary(fit2.Seoksu)

## with Intervention Analysis
summary(lm(Seoksu ~ 1)) # the intercept is reported as significative.
# Let`s see if there are any structural breaks
break_point <- breakpoints(Seoksu ~ 1)
summary(break_point)
plot(break_point) # The BIC minimum value is reached when m=2

par(mfrow=c(2,1))
ts.plot(Seoksu, main = "석수역의 break points")
fitted.ts <- fitted(break_point, breaks=2)
lines(fitted.ts, col = "red")
lines(confint(break_point, breaks=2))

ts.plot(Seoksu, main = "석수역의 break points") # I guess m=1 is the best.
fitted.ts <- fitted(break_point, breaks=1)
lines(fitted.ts, col = "red")
lines(confint(break_point, breaks=1))
par(mfrow=c(1,1))

# The fitted multiple level shifts(as determined by the structural breaks analysis) can be used
# as intervention variable to fit an ARIMA model, as shown below
# autoplot(fitted.ts)
fit3.Seoksu <- auto.arima(Seoksu, xreg=fitted.ts, ic="aicc")
summary(fit3.Seoksu)

temp <- auto.arima(dec.order, ic="aicc")
summary(temp)
fit4.Seoksu <- Arima(Seoksu, xreg=fitted.ts, order=c(0,1,2), seasonal=list(order=c(1,1,0)))
summary(fit4.Seoksu)

round(coeftest(fit3.Seoksu),3)
round(coeftest(fit4.Seoksu),3)

## plot
par(mfrow=c(3,1))
round(accuracy(forecast(fit1.Seoksu)),4)
plot(forecast(fit1.Seoksu,12*10),ylim=c(250000,750000),
     main="석수역 향후 10년치 예측 \nARIMA(1,1,2)(2,1,0)[12]")

round(accuracy(fit2.Seoksu),4)
plot(forecast(fit2.Seoksu,12*10), ylim=c(250000,750000), xlim=c(2005,2029),
     main="석수역 향후 10년치 예측 \nARIMA(1,0,2)(1,1,0)[12]")
abline(v=2012, col="red")
arrows(2011, 475000, 2012, 475000, col="red")
text(2008.5,475000,"2012년 자료부터 이용",col="red")

round(accuracy(forecast(fit3.Seoksu, xreg = fitted.ts)),4)
plot(forecast(fit3.Seoksu, xreg=rep(fitted.ts[168],12*10), h=12*10), ylim=c(250000,750000),
     main="석수역 향후 10년치 예측 \nRegression with ARIMA(2,0,2)(2,1,0)[12] errors")

round(accuracy(forecast(fit4.Seoksu, xreg = fitted.ts)),4)
plot(forecast(fit4.Seoksu, xreg=rep(fitted.ts[168],12*10), h=12*10), ylim=c(250000,750000),
     main="석수역 향후 10년치 예측 \nRegression with ARIMA(0,1,2)(1,1,0)[12] errors")

par(mfrow=c(1,1))


### (3) 광명역 : 2006년 12월 개통.
index <- which(colnames(final)=="14_광명")
GwangMyung <- ts(final[,index], start=c(2005,1), frequency = 12)
GwangMyung <- window(GwangMyung, start=c(2006,12), frequency = 12)
cut <- window(GwangMyung, start=c(2016,9), freq=12) # 잘라서 예측
dec.order <- window(GwangMyung, end=c(2013, 9), freq=12) # to decide order

## Arima models
fit1.GwangMyung <- auto.arima(GwangMyung,ic="aicc")
summary(fit1.GwangMyung)

fit2.GwangMyung <- auto.arima(cut, ic="aicc")
summary(fit2.GwangMyung)

## with Intervention Analysis
summary(lm(GwangMyung ~ 1)) # the intercept is reported as significative.
# Let`s see if there are any structural breaks
break_point <- breakpoints(GwangMyung ~ 1)
summary(break_point)
plot(break_point) # The BIC minimum value is reached when m=4

par(mfrow=c(2,1))
ts.plot(GwangMyung, main = "광명역- break points 4개")
fitted.ts <- fitted(break_point, breaks=4)
lines(fitted.ts, col = "red")
lines(confint(break_point, breaks=4))

ts.plot(GwangMyung, main = "광명역- break points 2개")  # i guess m=2 is the best.
fitted.ts <- fitted(break_point, breaks=2)
lines(fitted.ts, col = "red")
lines(confint(break_point, breaks=2))
par(mfrow=c(1,1))

# The fitted multiple level shifts(as determined by the structural breaks analysis) can be used
# as intervention variable to fit an ARIMA model, as shown below
# autoplot(fitted.ts)
fit3.GwangMyung <- auto.arima(GwangMyung, xreg=fitted.ts, ic="aicc")
summary(fit3.GwangMyung)

temp <- auto.arima(dec.order, ic="aicc")
summary(temp)
fit4.GwangMyung <- Arima(GwangMyung, xreg=fitted.ts, order=c(1,1,0), seasonal=list(order=c(2,0,0)))
summary(fit4.GwangMyung)

round(coeftest(fit3.GwangMyung),3)
round(coeftest(fit4.GwangMyung),3)

## plot
par(mfrow=c(3,1))
round(accuracy(forecast(fit1.GwangMyung)),4)
plot(forecast(fit1.GwangMyung,12*10), ylim=c(0,430000),
     main="광명역 향후 10년치 예측 \nARIMA(0,1,0) with drift")

round(accuracy(fit2.GwangMyung),4)
plot(forecast(fit2.GwangMyung,12*10), ylim=c(0,430000), xlim=c(2005,2029),
     main="광명역 향후 10년치 예측 \nARIMA(0,1,0) with drift")
abline(v=2016.8, col="red")
arrows(2015.8, 200000, 2016.8, 200000, col="red")
text(2013,200000,"2016년 9월자료부터 이용",col="red")

round(accuracy(forecast(fit3.GwangMyung, xreg = fitted.ts)),4)
plot(forecast(fit3.GwangMyung, xreg=rep(fitted.ts[145],12*10), h=12*10), ylim=c(0,430000),
     main="광명역 향후 10년치 예측 \nRegression with ARIMA(0,1,0) errors")


round(accuracy(forecast(fit4.GwangMyung, xreg = fitted.ts)),4)
plot(forecast(fit4.GwangMyung, xreg=rep(fitted.ts[145],12*10), h=12*10), ylim=c(0,430000),
     main="광명역 향후 10년치 예측 \nRegression with ARIMA(1,1,0)(2,0,0)[12] errors")
par(mfrow=c(1,1))

