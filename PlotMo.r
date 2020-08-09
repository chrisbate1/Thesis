rm(list=ls())
setwd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
#install.packages(c("plyr", "tidyr", "dplyr", "lubridate", "xts", "PerformanceAnalytics",
#                   "lmtest", "olsrr", "robustbase", "car", "stargazer", "plot3D"))
library(plyr); library(tidyr); library(dplyr); library(lubridate); library(xts);
library(PerformanceAnalytics); library(lmtest); library(olsrr); library(robustbase);
library(car); library(stargazer); library(ggplot2); library(plot3D)
data1 <- read.csv('AllUS.csv');# data2 <- read.csv('AllWorld.csv')
data1copy <- data1;# data2copy <- data2
# data1 <- data1copy;# data2 <- data2copy

data1 <- data1 %>% drop_na()
data1$date <- as.Date(data1$date, format='%d/%m/%Y')
data1$PRC[data1$PRC<0] <- data1$PRC[data1$PRC<0] * (-1)
data1$adjP <- data1$PRC / data1$CFACPR
data1$ret <- log(data1$adjP / lag(data1$adjP))
data1$xsRet <- (1 + data1$ret) / (1 + data1$sprtrn) - 1
data1 <- data1 %>% group_by(PERMNO) %>% slice(-1) %>% drop_na()

smry <- data1 %>% summarise(days = length(ret), avRet = mean(ret), var = var(ret), skw=skewness(ret, method='moment'), kurt=kurtosis(ret, method='moment'))
smry <- smry[smry$days==max(smry$days),] %>% drop_na()
plot(smry$skw/smry$kurt)
scatter3D(smry$avRet, smry$var, smry$skw, colvar=smry$kurt, theta=60, phi=30)

model = lm(smry$days[1]*smry$avRet ~ smry$var + smry$skw + smry$kurt)
summary(model)

rets <- data1 %>% group_by(PERMNO) %>% select(date, PERMNO, ret) 
rets <- rets %>% spread(PERMNO, ret)
rets <- rets[,(colSums(is.na(rets)) == 0)]

w <- rep(0, nrow(smry))
w[1] <- 1
w <- as.data.frame(w)
wsq <- w
while(dim(wsq)[2] < dim(rets)[1]){
  wsq <- cbind(wsq, w[,1])
}
wsq <- t(wsq)

portRet <- rowSums(wsq*rets[,2:ncol(rets)])

getPortRets = function(weights){
  weights <- weights / sum(weights)
  while(dim(weights)[2] < dim(rets)[1]){
    weights <- cbind(weights, weights[,1])
  }
  weights <- t(weights)
  return(rowSums(weights*rets[,2:ncol(rets)]))
}

# w nrow 6339 
eqPort <- getPortRets(as.data.frame(rep(1, nrow(smry))))
mean(eqPort)
sd(eqPort)
skewness(eqPort, method = 'moment')
kurtosis(eqPort, method = 'moment')




