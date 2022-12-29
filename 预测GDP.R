library(zoo)
library(tseries)
library(forecast)
library(TTR)
Data = read.csv("C:/Users/WangDan/Desktop/OECD/Tendency/Predict GDP.csv",header=T,sep=",")
# The first line is used as the line name 
row.names(Data) <- Data$sector
Data = Data[,-1]
Data
y1 = unlist(Data[1,])
y2 = unlist(Data[2,])
# Time series section
y1_ts = ts(y1) 
y2_ts = ts(y2)
# y_1
auto.arima(y1_ts)
pre_y1 = arima(y1_ts,order=c(1,1,0)) 
wine_y1 <- forecast(pre_y1,h=6,level=c(99.5))
auto.arima(y2_ts) 
pre_y2 = arima(y2_ts,order=c(1,1,0)) 
wine_y2 <- forecast(pre_y2,h=6,level=c(99.5)) 
wine_y2
# Linear regression section
x = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)
new <- data.frame(x = c(2019,2020,2021,2022,2023,2024,2025,2026))  
summary(lm(y1~x))
GDP = predict(lm(y1~x), new)
GDP
summary(lm(y2~x))
Predict_GDP = predict(lm(y2~x), new)
Predict_GDP
# Comparative analysis
# time series
var( c(y1,wine_y1$mean) ) 
var( c(y2,wine_y2$mean) )
# linear regression
var( c(y1,GDP) ) 
var( c(y2,Predict_GDP) ) 





