library(readr)
library(zoo)
library(tseries)
library(forecast)
library(TTR)
# Input data
table2000 <- read_csv("C:/Users/WangDan/Desktop/OECD/2000/2000.csv",col_names = F)
data2000 = as.data.frame(lapply(table2000[2:51,2:55],as.numeric))#The table which conclude all data.

table2001 <- read_csv("C:/Users/WangDan/Desktop/OECD/2001/2001.csv",col_names = F)
data2001 = as.data.frame(lapply(table2001[2:51,2:55],as.numeric))#The table which conclude all data.

table2002 <- read_csv("C:/Users/WangDan/Desktop/OECD/2002/2002.csv",col_names = F)
data2002 = as.data.frame(lapply(table2002[2:51,2:55],as.numeric))#The table which conclude all data.

table2003 <- read_csv("C:/Users/WangDan/Desktop/OECD/2003/2003.csv",col_names = F)
data2003 = as.data.frame(lapply(table2003[2:51,2:55],as.numeric))#The table which conclude all data.

table2004 <- read_csv("C:/Users/WangDan/Desktop/OECD/2004/2004.csv",col_names = F)
data2004 = as.data.frame(lapply(table2004[2:51,2:55],as.numeric))#The table which conclude all data.

table2005 <- read_csv("C:/Users/WangDan/Desktop/OECD/2005/2005.csv",col_names = F)
data2005 = as.data.frame(lapply(table2005[2:51,2:55],as.numeric))#The table which conclude all data.

table2006 <- read_csv("C:/Users/WangDan/Desktop/OECD/2006/2006.csv",col_names = F)
data2006 = as.data.frame(lapply(table2006[2:51,2:55],as.numeric))#The table which conclude all data.

table2007 <- read_csv("C:/Users/WangDan/Desktop/OECD/2007/2007.csv",col_names = F)
data2007 = as.data.frame(lapply(table2007[2:51,2:55],as.numeric))#The table which conclude all data.

table2008 <- read_csv("C:/Users/WangDan/Desktop/OECD/2008/2008.csv",col_names = F)
data2008 = as.data.frame(lapply(table2008[2:51,2:55],as.numeric))#The table which conclude all data.

table2009 <- read_csv("C:/Users/WangDan/Desktop/OECD/2009/2009.csv",col_names = F)
data2009 = as.data.frame(lapply(table2009[2:51,2:55],as.numeric))#The table which conclude all data.

table2010 <- read_csv("C:/Users/WangDan/Desktop/OECD/2010/2010.csv",col_names = F)
data2010 = as.data.frame(lapply(table2010[2:51,2:55],as.numeric))#The table which conclude all data.

table2011 <- read_csv("C:/Users/WangDan/Desktop/OECD/2011/2011.csv",col_names = F)
data2011 = as.data.frame(lapply(table2011[2:51,2:55],as.numeric))#The table which conclude all data.

table2012 <- read_csv("C:/Users/WangDan/Desktop/OECD/2012/2012.csv",col_names = F)
data2012 = as.data.frame(lapply(table2012[2:51,2:55],as.numeric))#The table which conclude all data.

table2013 <- read_csv("C:/Users/WangDan/Desktop/OECD/2013/2013.csv",col_names = F)
data2013 = as.data.frame(lapply(table2013[2:51,2:55],as.numeric))#The table which conclude all data.

table2014 <- read_csv("C:/Users/WangDan/Desktop/OECD/2014/2014.csv",col_names = F)
data2014 = as.data.frame(lapply(table2014[2:51,2:55],as.numeric))#The table which conclude all data.

table2015 <- read_csv("C:/Users/WangDan/Desktop/OECD/2015/2015.csv",col_names = F)
data2015 = as.data.frame(lapply(table2015[2:51,2:55],as.numeric))#The table which conclude all data.

table2016 <- read_csv("C:/Users/WangDan/Desktop/OECD/2016/2016.csv",col_names = F)
data2016 = as.data.frame(lapply(table2016[2:51,2:55],as.numeric))#The table which conclude all data.

table2017 <- read_csv("C:/Users/WangDan/Desktop/OECD/2017/2017.csv",col_names = F)
data2017 = as.data.frame(lapply(table2017[2:51,2:55],as.numeric))#The table which conclude all data.

table2018 <- read_csv("C:/Users/WangDan/Desktop/OECD/2018/2018.csv",col_names = F)
data2018 = as.data.frame(lapply(table2018[2:51,2:55],as.numeric))#The table which conclude all data.
#Inflation rate from 2000 to 2021
Inflation_rate_table <- read_csv("C:/Users/WangDan/Desktop/OECD/Inflation rate.csv",col_names = F)
#function
Dynamic <- function(data,X_next,W,ONT,GDP_next,real_CP,Real_GDP_next,Real_GDP) {
  Q = data[1:45,1:45]
  Yy = data[1:45,c(46,47,48,49,50,51,52,53,54)]
  Y_i = data.frame(Yy[,1]+Yy[,2]+Yy[,3]+Yy[,4]+Yy[,5]+Yy[,6]+Yy[,7]+Yy[,8]+Yy[,9]) #Total output - total intermediate consumption. 
  TXS_IMP_FIN_j = data[46,1:45]
  TXS_IMP_FIN_n = data[46,46:54]
  TXS_INT_FIN_j = data[47,1:45] 
  TXS_INT_FIN_n = data[47,46:54]
  GFCF_j = data[1:45,49]
  INVNT_j = data[1:45,50]
  X_i = data[50,1:45]
  VALU = data[49,1:45]
  GDP = sum(VALU)+sum(TXS_INT_FIN_j)+sum(TXS_INT_FIN_n)
  R_q  = (sum(X_i)-sum(VALU)-sum(TXS_INT_FIN_j))/sum(X_i)
  td = sum(TXS_INT_FIN_j)/sum(X_i)
  rw =  W/sum(VALU)
  rt = ONT/sum(VALU)
  Prh_q = (1-R_q-td)*(1-rw-rt)*sum(X_i)
  W+ONT+Prh_q
  Y_r = sum(Y_i)/GDP
  td_1 =sum(TXS_INT_FIN_n)/GDP
  CP = sum(GFCF_j)+sum(INVNT_j)
  rn = CP/Prh_q
  rn_n = CP/GDP
  Fe_q = CP/(X_next-sum(X_i))
  Fe_n = CP/(GDP_next-GDP)
  Real_Fe_n = real_CP/(Real_GDP_next-Real_GDP)
  return( data.frame(sum(X_i),GDP,R_q,td,rw,rt,Y_r,td_1,CP,rn,rn_n,Fe_q,Fe_n,Real_Fe_n) )
}

Predict_dases <- function(Year) {
  X_new = (Year[10]*(1-Year[3]-Year[4])*(1-Year[5]-Year[6])*(Year[3]*Year[1]+Year[7]*Year[2]))/Year[12]
  X_new_n= (Year[11]*((1-Year[3])*Year[1]+Year[8]*Year[2]))/Year[13]
 
  return( data.frame(X_new,X_new_n) )
}

Predict <- function(FUN,X) {
  A = 0
  for (x in X){
    y = FUN$coefficients[1] + FUN$coefficients[2]*x
    A = data.frame(A,y)
  }
  A = A[,-1]
  return( A )
}

Year2000 = Dynamic(data2000,X_next=3263648.5,W=424217.6,ONT=176999.0742,GDP_next=1339244.1,real_CP=138773.9117,Real_GDP_next=366001.979,Real_GDP=369050.5234)
Year2001 = Dynamic(data2001,X_next=3422153.8,W=528140.6,ONT=176130.3182,GDP_next=1470395.9,real_CP=137627.5689,Real_GDP_next=398964.6812,Real_GDP=366001.979)
Year2002 = Dynamic(data2002,X_next=3992536.3,W=622688.8,ONT=210705.6858,GDP_next=1659995.6,real_CP=150022.5198,Real_GDP_next=439482.2466,Real_GDP=398964.6812)
Year2003 = Dynamic(data2003,X_next=4917977.0,W=629335.1 ,ONT=220661.5667,GDP_next=1955327.3,real_CP=165258.3227,Real_GDP_next=484011.0865,Real_GDP=439482.2466)
Year2004 = Dynamic(data2004,X_next=6026144.5,W=736902.6 ,ONT=257646.5536,GDP_next=2292171.7,real_CP=182002.4835,Real_GDP_next=549196.4376,Real_GDP=484011.0865)
Year2005 = Dynamic(data2005,X_next=7446680.9,W=902045.1 ,ONT=311819.279,GDP_next=2751360.5,real_CP=206514.1034,Real_GDP_next=630702.598,Real_GDP=549196.4376)
Year2006 = Dynamic(data2006,X_next=9791092.1,W=1063297.9 ,ONT=356238.6725,GDP_next=3549303,real_CP=237162.8303,Real_GDP_next=766593.8266,Real_GDP=630702.598)
Year2007 = Dynamic(data2007,X_next=12416638.4,W=1375392.1 ,ONT=513880.0993,GDP_next=4593223.8,real_CP=288261.9513,Real_GDP_next=906556.9112,Real_GDP=766593.8266)
Year2008 = Dynamic(data2008,X_next=13465305.4,W=1882515.1 ,ONT=594367.746,GDP_next=5102086.4,real_CP=340892.2107,Real_GDP_next=1009114.802,Real_GDP=906556.9112)
Year2009 = Dynamic(data2009,X_next=15488547.6,W=2156133.3 ,ONT=648354.2649,GDP_next=6085587.5,real_CP=379457.0107,Real_GDP_next=1149776.11,Real_GDP=906556.9112)
Year2010 = Dynamic(data2010,X_next=19793987.2,W=2494559.6 ,ONT=903241.9843,GDP_next=7549565.4,real_CP=432349.8229,Real_GDP_next=1292664.974,Real_GDP=1149776.11)
Year2011 = Dynamic(data2011,X_next=23036557.6,W=3270237.3 ,ONT=1008440.9,GDP_next=8530114.3,real_CP=486080.2617,Real_GDP_next=1431962.849,Real_GDP=1292664.974)
Year2012 = Dynamic(data2012,X_next=26231179.4,W=3828047.7 ,ONT=1169652.226,GDP_next=9567400.9,real_CP=538460.3825,Real_GDP_next=1566924.53,Real_GDP=1431962.849)
Year2013 = Dynamic(data2013,X_next=28981681.5,W=4465408.6 ,ONT=1260697.209,GDP_next=10472445.9,real_CP=589209.9661,Real_GDP_next=1697627.006,Real_GDP=1566924.53)
Year2014 = Dynamic(data2014,X_next=31155156.7,W=5007700.3 ,ONT=1367060.117,GDP_next=11093006,real_CP=638357.9628,Real_GDP_next=1821029.52,Real_GDP=1697627.006)
Year2015 = Dynamic(data2015,X_next=29543294.1,W=5376963.3 ,ONT=1341667.185,GDP_next=11232760.4,real_CP=684760.9577,Real_GDP_next=1795667.781,Real_GDP=1821029.52)
Year2016 = Dynamic(data2016,X_next=31022989.8,W=5340727.9 ,ONT=1404989.32,GDP_next=12307537.6,real_CP=675224.1935,Real_GDP_next=1907792.584,Real_GDP=1795667.781)
Year2017 = Dynamic(data2017,X_next=35015787.5,W=5686280.6 ,ONT=1419983.53,GDP_next=13891803.5,real_CP=717386.4357,Real_GDP_next=2058535.612,Real_GDP=1907792.584)
#Fe2018 = dynamic(data2018,X_next=   ,W=6534891.6 ,ONT=1535381.705,GDP_next=14630600)
# Forecasting incremental values of total output and GDP
Predict_dases(Year2000)
Predict_dases(Year2001)
Predict_dases(Year2002)
Predict_dases(Year2003)
Predict_dases(Year2004)
Predict_dases(Year2005)
Predict_dases(Year2006)
Predict_dases(Year2007)
Predict_dases(Year2008)
Predict_dases(Year2009)
Predict_dases(Year2010)
Predict_dases(Year2011)
Predict_dases(Year2012)
Predict_dases(Year2013)
Predict_dases(Year2014)
Predict_dases(Year2015)
Predict_dases(Year2016)
Predict_dases(Year2017)
#Predict_dases(Year2018)


# linear regression to predict.
# We take inflation into account.
FE_Q = data.frame(Year2000[12],Year2001[12],Year2002[12],Year2003[12],Year2004[12],Year2005[12],Year2006[12],Year2007[12],Year2008[12],Year2009[12],Year2010[12],Year2011[12],Year2012[12],Year2013[12],Year2014[12],Year2015[12],Year2016[12],Year2017[12])
FE_N = data.frame(Year2000[13],Year2001[13],Year2002[13],Year2003[13],Year2004[13],Year2005[13],Year2006[13],Year2007[13],Year2008[13],Year2009[13],Year2010[13],Year2011[13],Year2012[13],Year2013[13],Year2014[13],Year2015[13],Year2016[13],Year2017[13])
Real_FE_N = data.frame(Year2000[14],Year2001[14],Year2002[14],Year2003[14],Year2004[14],Year2005[14],Year2006[14],Year2007[14],Year2008[14],Year2009[14],Year2010[14],Year2011[14],Year2012[14],Year2013[14],Year2014[14],Year2015[14],Year2016[14],Year2017[14])
Training_set = c(0.0206,0.0204,0.006,0.026,0.069,0.039,0.03928,0.07749,0.07792,-0.00211,0.0688,0.0807,0.00233,0.0216,0.01032,-0.00003,0.01407,0.0423)
prediction_set <- c(0.0349,0.0128,0.0049,0.0437,0.02)
FE_Q_predict = lm(unlist(FE_Q)~unlist(Training_set))
Predict(FE_Q_predict,prediction_set)
FE_N_predict = lm(unlist(Real_FE_N )~unlist(Training_set))
Predict(FE_N_predict,prediction_set)
# We don't take inflation into account.
Training_set_time = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
prediction_set_time <- c(2018,2019,2020,2021,2022,2023,2024,2025)
FE_Q_predict_1 = lm(unlist(FE_Q)~unlist(Training_set_time))
Predict(FE_Q_predict_1,prediction_set_time)
FE_N_predict_1 = lm(unlist(FE_N)~unlist(Training_set_time))
Predict(FE_N_predict_1,prediction_set_time)





