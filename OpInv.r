rm(list=ls())
setwd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
data <- read.csv('allOpData.csv'); datacopy <- data
# install.packages(c("plyr", "tidyr", "dplyr", "lubridate", "xts", "PerformanceAnalytics",
#                   "lmtest", "olsrr", "robustbase", "car", "stargazer", "MASS)); y
library(plyr); library(tidyr); library(dplyr); library(lubridate); library(xts);
library(PerformanceAnalytics); library(lmtest); library(olsrr); library(robustbase);
library(car); library(stargazer); library(RND); library(nortest); 

data <- datacopy
data$Ticker <- trimws(data$Ticker)
data$OPTION_ID <- trimws(data$OPTION_ID)
data$PUTCALLIND <- trimws(data$PUTCALLIND)
data$Date <- as.Date(data$Date, format = '%Y-%m-%d')
data$EXPIR_DATE <- as.Date(data$EXPIR_DATE, format = '%Y-%m-%d')
data <- data[order(data$OPTION_ID, data$Date),]
data$ORet <- log(data$CLOSE / lag(data$CLOSE))
data <- data %>% group_by(OPTION_ID) %>% slice(-1)
data$DTE <- as.numeric(data$EXPIR_DATE - data$Date)
# Optional for dividend adjustment
# data$STRIKE_PRC <- data$STRIKE_PRC * exp(0.04 * data$DTE / 365)

calls <- data[data$PUTCALLIND == 'CALL',]
calls <- calls[order(calls$Ticker, calls$Date, calls$EXPIR_DATE, calls$STRIKE_PRC),
               ] %>% group_by(Ticker, Date, EXPIR_DATE)
calls$PDF <- 2/(calls$STRIKE_PRC - lag(calls$STRIKE_PRC)) * (lag(calls$CLOSE) - ((lead(calls$STRIKE_PRC)-lag(calls$STRIKE_PRC)) / (lead(calls$STRIKE_PRC) - calls$STRIKE_PRC))*calls$CLOSE + 
                ((calls$STRIKE_PRC-lag(calls$STRIKE_PRC)) / (lead(calls$STRIKE_PRC) - calls$STRIKE_PRC))*lead(calls$CLOSE))
# Still needs to be sliced
calls <- calls[order(calls$Ticker, calls$Date, calls$EXPIR_DATE, calls$STRIKE_PRC),]
calls <- calls %>% group_by(Ticker, Date, EXPIR_DATE) %>% slice(c(-1,-n())) %>% drop_na()
sum(calls$PDF < 0, na.rm = TRUE)
summary(calls$PDF)
plot(calls$PDF, type='l')
plot(calls$PDF, cex=0.1)

puts <- data[data$PUTCALLIND == 'PUT',]
puts <- puts[order(puts$Ticker, puts$Date, puts$EXPIR_DATE, puts$STRIKE_PRC),
               ] %>% group_by(Ticker, Date, EXPIR_DATE)
puts$PDF <- 2/(puts$STRIKE_PRC - lag(puts$STRIKE_PRC)) * (lag(puts$CLOSE) - ((lead(puts$STRIKE_PRC)-lag(puts$STRIKE_PRC)) / (lead(puts$STRIKE_PRC) - puts$STRIKE_PRC))*puts$CLOSE + 
                                                               ((puts$STRIKE_PRC-lag(puts$STRIKE_PRC)) / (lead(puts$STRIKE_PRC) - puts$STRIKE_PRC))*lead(puts$CLOSE))
# Still needs to be sliced
puts <- puts[order(puts$Ticker, puts$Date, puts$EXPIR_DATE, puts$STRIKE_PRC),]
puts <- puts %>% group_by(Ticker, Date, EXPIR_DATE) %>% slice(c(-1,-n())) %>% drop_na()
sum(puts$PDF < 0, na.rm = TRUE)
summary(puts$PDF)
plot(puts$PDF, type='l')
plot(puts$PDF, cex=0.1)

distData <- rbind(calls, puts)
sze <- mutate(distData, sumPDF=sum(PDF))$sumPDF
summary(sze)
plot(1:length(sze), sze[order(sze)], type='l')
summary(as.numeric(distData$DTE))

sPrices <- read.csv('sPrices.csv')
sPrices$Date <- as.Date(sPrices$Date, format = '%Y-%m-%d')
combData <- merge(distData, sPrices, by=c('Date','Ticker')) %>% drop_na()
combData <- merge(combData, select(data, OPTION_ID, Date, STRIKE_PRC, PUTCALLIND, ORet, PUTCALLIND))

plot(combData$SRet, combData$ORet, cex=0.2)
corr <- cor(combData[,sapply(combData, is.numeric)])
summary(matrix(corr[corr!=1], ncol = ncol(corr), dimnames = list(1:(ncol(corr)-1), colnames(corr))))

combData$impRet <- log(combData$STRIKE_PRC / combData$Price) * 365 / combData$DTE
summary(combData$impRet)
plot(log(combData$STRIKE_PRC / combData$Price)[log(combData$STRIKE_PRC / combData$Price)>-4], cex=0.01)
impData <- combData[log(combData$STRIKE_PRC / combData$Price)>-4,]

combData$zScore <- (combData$SRet - combData$SMean) / combData$SD

combData <- combData %>% group_by(Ticker, Date, EXPIR_DATE, PUTCALLIND) %>% mutate(meanPDF = sum(impRet*PDF), varPDF = sum(PDF*(impRet-sum(impRet*PDF))^2), 
                  skewPDF = sum(PDF * ((impRet - sum(impRet*PDF)) / sqrt(abs(sum(PDF*(impRet-sum(impRet*PDF))^2))))^3),
                  kurtPDF = sum(PDF * ((impRet - sum(impRet*PDF))^2 / sum(PDF*(impRet-sum(impRet*PDF))^2))^2))
summary(combData$meanPDF); summary(combData$varPDF); summary(combData$skewPDF); summary(combData$kurtPDF); 

# Unnecessary if saving the lag data 
# write.csv(combData, 'CombOpData.csv', row.names = FALSE)

summary(lm(combData$meanPDF ~ combData$SRet))
summary(lm(combData$varPDF ~ combData$SRet))
summary(lm(combData$skewPDF ~ combData$SRet))
summary(lm(combData$kurtPDF ~ combData$SRet))
retReg <- lm(combData$SRet ~ combData$meanPDF + combData$varPDF +
                              combData$skewPDF + combData$kurtPDF)
residualPlot(retReg, cex=0.1)
summary(retReg)
ad.test(retReg$residuals)
qqnorm(retReg$residuals)

smrisd <- combData %>% group_by(Ticker, Date, EXPIR_DATE,
            PUTCALLIND) %>% summarise(meanPDF = meanPDF[1],
            varPDF = varPDF[1], skewPDF = skewPDF[1], kurtPDF = kurtPDF[1])

smrisd <- smrisd[order(smrisd$PUTCALLIND, smrisd$Ticker, smrisd$EXPIR_DATE, smrisd$Date),]
smrisd$lagMean <- lag(smrisd$meanPDF)
smrisd$lagVar <- lag(smrisd$varPDF)
smrisd$lagSkew <- lag(smrisd$skewPDF)
smrisd$lagKurt <- lag(smrisd$kurtPDF)

smrisd$l2Mean <- lag(smrisd$meanPDF, n=2)
smrisd$l2Var <- lag(smrisd$varPDF, n=2)
smrisd$l2Skew <- lag(smrisd$skewPDF, n=2)
smrisd$l2Kurt <- lag(smrisd$kurtPDF, n=2)

smrisd$l3Mean <- lag(smrisd$meanPDF, n=3)
smrisd$l3Var <- lag(smrisd$varPDF, n=3)
smrisd$l3Skew <- lag(smrisd$skewPDF, n=3)
smrisd$l3Kurt <- lag(smrisd$kurtPDF, n=3)

smrisd$leadMean <- lead(smrisd$meanPDF)
smrisd$leadVar <- lead(smrisd$varPDF)
smrisd$leadSkew <- lead(smrisd$skewPDF)
smrisd$leadKurt <- lead(smrisd$kurtPDF)

# smrisd$lead2Mean <- lead(smrisd$meanPDF, n=2)
# smrisd$lead2Var <- lead(smrisd$varPDF, n=2)
# smrisd$lead2Skew <- lead(smrisd$skewPDF, n=2)
# smrisd$lead2Kurt <- lead(smrisd$kurtPDF, n=2)
# 
# smrisd$lead3Mean <- lead(smrisd$meanPDF, n=3)
# smrisd$lead3Var <- lead(smrisd$varPDF, n=3)
# smrisd$lead3Skew <- lead(smrisd$skewPDF, n=3)
# smrisd$lead3Kurt <- lead(smrisd$kurtPDF, n=3)

smrisd$l10Mean <- lead(smrisd$meanPDF, n=10)
smrisd$l10Var <- lead(smrisd$varPDF, n=10)
smrisd$l10Skew <- lead(smrisd$skewPDF, n=10)
smrisd$l10Kurt <- lead(smrisd$kurtPDF, n=10)

smrisd <- smrisd %>% group_by(Ticker, EXPIR_DATE, PUTCALLIND) %>% slice(-1)
smrisd <- select(smrisd, -meanPDF, -varPDF, -skewPDF, -kurtPDF)
lagData <- merge(combData, smrisd, by = c('Ticker', 'Date', 'EXPIR_DATE', 'PUTCALLIND'))

write.csv(lagData, 'LagOpData.csv', row.names = FALSE)

