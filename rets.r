rm(list=ls())
setwd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
install.packages(c("plyr", "tidyr", "dplyr", "lubridate", "xts", "PerformanceAnalytics",
                   "lmtest", "olsrr", "robustbase", "car", "stargazer", "plot3D"))
library(plyr); library(tidyr); library(dplyr); library(lubridate); library(xts);
library(PerformanceAnalytics); library(lmtest); library(olsrr); library(robustbase);
library(car); library(stargazer); library(ggplot2); library(plot3D)
data1 <- read.csv('data2000.csv')

rets <- data1 %>% group_by(PERMNO) %>% select(date, PERMNO, Ret) 
rets <- rets %>% spread(PERMNO, Ret)

print(length(is.na(rets)))
View(rets)

rets <- rets[,colSums(is.na(rets))==0]
write.csv(rets, "rets.csv", row.names = FALSE)
