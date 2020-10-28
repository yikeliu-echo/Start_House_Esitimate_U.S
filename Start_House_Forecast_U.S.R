library(forecast)
#Housing Starts: Total: New Privately Owned Housing Units Started (not seasonally adjusted)
#dataset website: https://fred.stlouisfed.org/series/HOUSTNSA
houseNSA <- read.csv("C:/Users/yikel/Desktop/Forcasting/data/HOUSTNSA.csv")

#to have a general look at this time series
houseNSA.ts <- ts(houseNSA$HOUSTNSA, start=c(1959,1), freq=12)
plot(houseNSA.ts)

#observe data after 2008.12
houseNSA.ts.zoom <- window(houseNSA.ts,start=c(2009,1))
plot(houseNSA.ts.zoom, main = "Housing Starts from 2009")
grid()

#use Holt/Winters seasonal filter model
train.ts <- window (houseNSA.ts.zoom,end=c(2019,9))
valid.ts <- window (houseNSA.ts.zoom,start=c(2019,10),end=c(2020,9))
nValid <- length (valid.ts)
nTest <- 12

#see the model validation
ses.1 <- ets(train.ts, model = "AAA" )
ses.1.pred <- forecast(ses.1, h = nValid, level =0.0)
plot(ses.1.pred)
lines(valid.ts)
grid()

#find the forecast
ses.2 <- ets(houseNSA.ts.zoom, model = "AAA" )
ses.2.pred <- forecast(ses.2, h = nTest, level =0.0)
ses.2.pred$mean