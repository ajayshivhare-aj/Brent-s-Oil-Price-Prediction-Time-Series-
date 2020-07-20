library(fpp)
library(fpp2)
library(data.table)
library(GGally)

bop<-BrentOilPrices

bop$Date<-as.Date(bop$Date, format = c("%b %d, %Y"))
bop1<- ts(bop$Price, frequency = 5)

setDT(bop)

bop$YEAR<-format(as.Date(bop$Date, format="%B %d, %Y"), "%Y")
bop$MONTH<-format(as.Date(bop$Date, format="%B %d, %Y"), "%B")
bop$MONTH<-factor(bop$MONTH,levels=month.name)

bopy <- bop[,.(Price, YEAR)]
bopy <- bopy[,.(mean(Price)), by = YEAR]
names(bopy) = c('Year', 'Avg_Price')  
bop2<- ts(bopy$Avg_Price)

bopm <- bop[,.(Price, MONTH)]
bopm <- bopm[,.(mean(Price)), by = MONTH]
names(bopm) = c('Month', 'Avg_Price')
bop3<- ts(bopm$Avg_Price, frequency = 12)

#Oil price 
ggplot(bop, aes(x=Date, y=Price, group=1)) + stat_summary(geom="line", col="navy") + 
  ggtitle("brent_oil_Price") + ylab("Price") + xlab("Date") +
  theme(axis.text.x = element_text(angle = 40)) 

#Average Price Over Years
ggplot(bop, aes(x=YEAR, y=Price, group=1)) + stat_summary(fun="mean", geom="line", col="navy") + 
  ggtitle("Average Price Over Years") + ylab("Price") + xlab("Year") +
  theme(axis.text.x = element_text(angle = 40)) 


#Average Price per Month
ggplot(bop, aes(x=MONTH, y=Price, group=1)) + stat_summary(fun="mean", geom="line", col="darkorchid4") + 
  ggtitle("Average Price per Month") + ylab("Price") + xlab("Month") +
  theme(axis.text.x = element_text(angle = 40))

##############################################################################################################
##Simple Forecasting methods
train <- window(bop1, start=1, end=1120)
test<-window(bop1, start=1121, end=1644)

m<-meanf(train, h=523)
accuracy(m,test)

n<-naive(train, h=523)
accuracy(n,test)

s<-snaive(train, h=523)
accuracy(s,test)

d<-rwf(train, h=523, drift = TRUE)
accuracy(d,test)
#############################################################################################################

#Avg Method
meanf(bop1, h=5)

#Naive method(Random Walk Forecast)
naive(bop1, h=5)

#seasonal Naive
snaive(bop1, h=5)

#drift method
rwf(bop1, h=5, drift = TRUE)

#plotting
autoplot(bop1) +
  autolayer(meanf(bop1, h=500),
            series="Mean", PI=FALSE) +
  autolayer(rwf(bop1, h=500),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(bop1, drift=TRUE, h=500),
            series="Drift", PI=FALSE) +
  ggtitle("brent_oil_prices(simple forecasting methods)") +
  xlab("week") + ylab("Prices") +
  guides(colour=guide_legend(title="Forecast"))


##Simple Forecasting methods by year

trainy <- window(bop2, start=1, end=23)
testy<-window(bop2, start=24, end=33)

m1<-meanf(trainy, h=10)
accuracy(m1,testy)

n1<-naive(trainy, h=10)
accuracy(n1,testy)

s1<-snaive(trainy, h=10)
accuracy(s1,testy)

d1<-rwf(trainy, h=10, drift = TRUE)
accuracy(d1,testy)

#Avg Method
meanf(bop2, h=5)

#Naive method(Random Walk Forecast)
naive(bop2, h=5)

#seasonal Naive
snaive(bop2, h=5)

#drift method
rwf(bop2, h=5, drift = TRUE)

#plotting
autoplot(bop2) +
  autolayer(meanf(bop2, h=5),
            series="Mean", PI=FALSE) +
  autolayer(rwf(bop2, h=5),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(bop2, drift=TRUE, h=5),
            series="Drift", PI=FALSE) +
  ggtitle("brent_oil_prices(simple forecasting methods by year)") +
  xlab("year") + ylab("Prices") +
  guides(colour=guide_legend(title="Forecast"))


##Simple Forecasting methods by month

trainm <- window(bop3, start=1, end=1.344)
testm<-window(bop3, start=1.35)

m2<-meanf(trainm, h=4)
accuracy(m2,testm)

n2<-naive(trainm, h=4)
accuracy(n2,testm)

s2<-snaive(trainm, h=4)
accuracy(s2,testm)

d2<-rwf(trainm, h=4, drift = TRUE)
accuracy(d2,testm)

#Avg Method
meanf(bop3, h=5)

#Naive method(Random Walk Forecast)
naive(bop3, h=5)

#seasonal Naive
snaive(bop3, h=5)

#drift method
rwf(bop3, h=5, drift = TRUE)

#plotting
autoplot(bop3) +
  autolayer(meanf(bop3, h=5),
            series="Mean", PI=FALSE) +
  autolayer(rwf(bop3, h=5),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(bop3, drift=TRUE, h=5),
            series="Drift", PI=FALSE) +
  ggtitle("brent_oil_prices(simple forecasting methods by month)") +
  xlab("month") + ylab("Prices") +
  guides(colour=guide_legend(title="Forecast"))



###################################################################################################
#ses

plot(bop2, ylab="Oil Price",xlab="Year")
se1 <-ses(bop2, alpha=0.2, initial="simple", h=10)
se2 <-ses(bop2, alpha=0.6, initial="simple", h=10)
se3 <-ses(bop2, h=10)
se3$model #0.999 is the alpha
plot(se1, plot.conf=FALSE, main="oil Price", ylab="Oil", xlab="Year", fcol="white", type="o")
lines(fitted(se1), col="blue", type="o")
lines(fitted(se2), col="red", type="o")
lines(fitted(se3), col="green", type="o")
lines(se1$mean, col="blue", type="o")
lines(se2$mean, col="red", type="o")
lines(se3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("data", expression(lambda == 0.2), expression(lambda == 0.6),
         expression(lambda == 0.9999)),pch=1)


###########################################################################################################
#Holts linear trend method weekly
fc <- holt(bop1, h=5)
fc
fc$model

fcw <- holt(bop1, h=500)
autoplot(bop1) +
  autolayer(fcw, series="Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("week") +
  ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))


#Holts linear trend method yearly
fcy <- holt(bop2, h=3)
fcy
fcy$model


autoplot(bop2) +
  autolayer(fcy, series="Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("year") +
  ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))


#Holts linear trend method monthly
fcm <- holt(bop3, h=3)
fcm
fcm$model


autoplot(bop3) +
  autolayer(fcm, series="Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("month") +
  ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))


#########################################################################################################
#holts winter method by week 
#additive method
fita <- hw(bop1,seasonal="additive", h=5)
fita
fita$model

fitaw <- hw(bop1,seasonal="additive", h=500)
autoplot(bop1) +
  autolayer(fitaw, series="HW additive", PI=FALSE) +
  ggtitle("Forecasts from Holt's winter method additive") + xlab("week") +
  ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))


#multiplicative method
fitm <- hw(bop1,seasonal="multiplicative", h=5)
fitm
fitm$model

fitmw <- hw(bop1,seasonal="multiplicative", h=500)
autoplot(bop1) +
  autolayer(fitmw, series="HW multiplicative", PI=FALSE) +
  ggtitle("Forecasts from Holt's winter method multiplicative") + xlab("week") +
  ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))



#holts winter method by year not applicable because frequency should greater than 1
#holts winter method by month not applicable because observation has to be greater than 15


#########################################################################################################
#ARIMA

#weekly data
bopweekly<-data.frame(bop1)

fit13<-auto.arima(bopweekly, seasonal = FALSE) #not gives best but better
fit13

fit14<-forecast(fit13, h=5)
fit14
plot(bopweekly, col = "red")
lines(fitted(fit13), col = "blue")
lines(fit14$mean, col = "black")
fit14$mean


#yearly data
bopyear<- data.frame(bop2)

fit11<-auto.arima(bopyear, seasonal = FALSE) #not gives best but better
fit11

fit12<-forecast(fit11, h=3)
fit12
plot(bopyear, col = "red")
lines(fitted(fit11), col = "blue")
lines(fit12$mean, col = "black")
fit12$mean


#monthly data
bopmonth<-data.frame(bop3)

fit15<-auto.arima(bopmonth, seasonal = FALSE) #not gives best but better
fit15

ndiffs(bopyear) #gives the differencing same as auto.arima.
fit16<-forecast(fit15, h=3)
fit16
plot(bopmonth, col = "red")
lines(fitted(fit15), col = "blue")
lines(fit16$mean, col = "black")
fit16$mean

########################################################################################################
#AUtocorrelation
acf(bopyear)
acf(bopmonth)
acf(bopweekly)
par(mfrow=c(2,2))

#########################################################################################################
#Regression

bopyear1<-ts(bopy)
bopmon2<-ts(bopm)
#on yearly data
View(bopyear1)
reg <- tslm(Avg_Price~Year, data = bopyear1)
summary(reg)

plot(jitter(Avg_Price) ~ jitter(Year),xlab="YEAR",
     ylab="PRICEs",data= bopyear1)
abline(reg, col = "red")
res<-residuals(reg)
plot(res)
predicted<-predict(fit, newdata = data.frame(Year=2020:2030))

predicted
CV(reg)

#regression on monthly model

View(bopmon2)
reg1 <- tslm(Avg_Price~Month, data = bopmon2)
summary(reg1)

plot(jitter(Avg_Price) ~ jitter(Month),xlab="Month",
     ylab="PRICEs",data= bopmon2)
abline(reg1, col = "red")
res1<-residuals(reg1)
plot(res)

CV(reg1)
