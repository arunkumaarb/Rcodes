setwd("C:/Users/gsstseka/Box Sync/Greatlakes/Capstone Project/BALIC/Data")

library(dplyr)
library(forecast)
library(reshape2)
library(data.table)
library(foreach)
library(date)
library(lubridate)

#install.packages("doMC")
library(doMC)

#reading the file
I1=read.csv("Policy issued for TSF.csv",header=TRUE)

I1_ts= ts(I1[,(3:8)],frequency = 12)

fcst_period = 1  # 1 month of forecast
fcst_matrix <- matrix(NA,nrow=nrow(I1_ts),ncol=fcst_period)

registerDoMC(detectCores()-1)
fcst_matrix <- foreach(i=1:nrow(I1_ts),.combine=rbind, .packages=c("forecast")) %dopar% { 
  fcst_matrix <- forecast(ets(I1_ts[i,]),h=fcst_period)$mean
}

fcst_matrix[fcst_matrix < 0] <- 0

fcst_matrix=ceiling(fcst_matrix)

I1$forecast=fcst_matrix[,1]

#RMSE
sqrt(sum((I1$Aug-I1$forecast)^2))

write.csv(I1,"Issued_forecast.csv")