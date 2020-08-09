rm(list=ls())
setwd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
lagData <- read.csv('LagOpData.csv'); datacopy <- lagData
# install.packages(c("plyr", "tidyr", "dplyr", "lubridate", "xts", "PerformanceAnalytics",
#                   "lmtest", "olsrr", "robustbase", "car", "stargazer", "MASS)); y
library(plyr); library(tidyr); library(dplyr); library(lubridate); library(xts);
library(PerformanceAnalytics); library(lmtest); library(olsrr); library(robustbase);
library(car); library(stargazer); library(RND); library(nortest); 

lagData <- datacopy

gLagData <- lagData %>% group_by(Ticker, Date, EXPIR_DATE, PUTCALLIND) %>% summarise(
              meanPDF=meanPDF[1], lagMean=lagMean[1], varPDF=varPDF[1], lagVar=lagVar[1], skewPDF=skewPDF[1],
              lagSkew=lagSkew[1], kurtPDF=kurtPDF[1], lagKurt=lagKurt[1], leadMean=leadMean[1],
              leadVar=leadVar[1], leadSkew=leadSkew[1], leadKurt=leadKurt[1], zScore=zScore[1],
              l2Mean=l2Mean[1], l2Var=l2Var[1], l2Skew=l2Skew[1], l2Kurt=l2Kurt[1],
              l3Mean=l3Mean[1], l3Var=l3Var[1], l3Skew=l3Skew[1], l3Kurt=l3Kurt[1],
              l10Mean=l10Mean[1], l10Var=l10Var[1], l10Skew=l10Skew[1], l10Kurt=l10Kurt[1],
              .groups='keep') %>% drop_na()
gLagData <- ungroup(gLagData)
gLagData$Date <- as.Date(gLagData$Date)
gLagData$EXPIR_DATE <- as.Date(gLagData$EXPIR_DATE)

# Optional removal of outliers, mostly matter for skew and kurtosis 
# Can also comment out rows if bothered about certain lag or lead 
dim(gLagData)
removeOutliers <- function(l = 50){
  return(gLagData[abs(gLagData$meanPDF) < l & abs(gLagData$varPDF) < l & abs(gLagData$skewPDF) < l & abs(gLagData$kurtPDF) < l & 
                    abs(gLagData$lagMean) < l & abs(gLagData$lagVar) < l & abs(gLagData$lagSkew) < l & abs(gLagData$lagKurt) < l & 
                    abs(gLagData$l10Mean) < l & abs(gLagData$l10Var) < l & abs(gLagData$l10Skew) < l & abs(gLagData$l10Kurt) < l & 
                    abs(gLagData$leadMean) < l & abs(gLagData$leadVar) < l & abs(gLagData$leadSkew) < l & abs(gLagData$leadKurt) < l,])
}
gLagData <- removeOutliers() %>% drop_na()
dim(removeOutliers())

write.csv(gLagData, 'GroupedOpData.csv', row.names = FALSE)

plot(select(gLagData, meanPDF, lagMean), cex=0.1)
plot(gLagData$meanPDF - gLagData$lagMean, cex=0.1)
plot(gLagData$meanPDF / gLagData$lagMean, cex=0.1)
plot(log(gLagData$meanPDF / gLagData$lagMean), cex=0.1)

png('AutoMean.png', width = 500, height = 500)
plot(gLagData$meanPDF, gLagData$leadMean, cex=0.2, 
     xlab='Mean (t)', ylab='Mean (t+1)', col='darkblue')
dev.off()
png('AutoVar.png', width = 500, height = 500)
plot(gLagData$varPDF, gLagData$leadVar, cex=0.2, 
     xlab='Variance (t)', ylab='Variance (t+1)', col='darkblue')
dev.off()
png('AutoSkew.png', width = 500, height = 500)
plot(gLagData$skewPDF, gLagData$leadSkew, cex=0.2, 
     xlab='Skew (t)', ylab='Skew (t+1)', col='darkblue')
dev.off()
plot(gLagData$kurtPDF, gLagData$leadKurt, cex=0.1)
png('AutoKurt.png', width = 500, height = 500)
plot(abs(gLagData$kurtPDF), abs(gLagData$leadKurt), cex=0.2, 
     xlab='Kurtosis (t)', ylab='Kurtosis (t+1)', col='darkblue')
dev.off()
png('AutoMom.png', width = 1000, height = 1000)
par(mfrow=c(2,2))
plot(gLagData$meanPDF, gLagData$leadMean, cex=0.2, 
     xlab='Mean (t)', ylab='Mean (t+1)', col='darkblue')
plot(gLagData$varPDF, gLagData$leadVar, cex=0.2, 
     xlab='Variance (t)', ylab='Variance (t+1)', col='darkblue')
plot(gLagData$skewPDF, gLagData$leadSkew, cex=0.2, 
     xlab='Skew (t)', ylab='Skew (t+1)', col='darkblue')
plot(abs(gLagData$kurtPDF), abs(gLagData$leadKurt), cex=0.2, 
     xlab='Kurtosis (t)', ylab='Kurtosis (t+1)', col='darkblue')
dev.off()

plot(gLagData$zScore, log(gLagData$meanPDF/gLagData$lagMean), cex=0.1)
plot(gLagData$zScore, log(gLagData$varPDF/gLagData$lagVar), cex=0.1)
plot(gLagData$zScore, log(gLagData$leadVar/gLagData$varPDF), cex=0.1)
plot(gLagData$zScore, log(gLagData$skewPDF/gLagData$lagSkew), cex=0.1)
# plot(gLagData$zScore, gLagData$skewPDF/gLagData$lagSkew, cex=0.1)
plot(gLagData$zScore, gLagData$skewPDF - gLagData$lagSkew, cex=0.1)
plot(gLagData$zScore, gLagData$leadSkew - gLagData$skewPDF, cex=0.1)
plot(gLagData$zScore, log(gLagData$kurtPDF/gLagData$lagKurt), cex=0.1)

plot(gLagData$zScore, gLagData$meanPDF, cex=0.1)
plot(gLagData$zScore, gLagData$varPDF, cex=0.1)
plot(gLagData$zScore, gLagData$skewPDF, cex=0.1)
plot(gLagData$zScore, gLagData$kurtPDF, cex=0.1)
summary(lm((gLagData$zScore ~ gLagData$meanPDF + gLagData$varPDF + gLagData$skewPDF + gLagData$kurtPDF)))
residualPlot(lm((gLagData$zScore ~ gLagData$meanPDF + gLagData$varPDF + gLagData$skewPDF + gLagData$kurtPDF)))

plot(gLagData$zScore, gLagData$meanPDF - gLagData$lagMean, cex=0.1)
summary(lm((gLagData$zScore ~ I(gLagData$meanPDF - gLagData$lagMean))))
plot(gLagData$zScore, gLagData$varPDF - gLagData$lagVar, cex=0.1)
summary(lm((gLagData$zScore ~ I(gLagData$varPDF - gLagData$lagVar))))
plot(gLagData$zScore, gLagData$skewPDF - gLagData$lagSkew, cex=0.1)
summary(lm((gLagData$zScore ~ I(gLagData$skewPDF - gLagData$lagSkew))))
plot(gLagData$zScore, gLagData$kurtPDF - gLagData$lagKurt, cex=0.1)
summary(lm((gLagData$zScore ~ I(gLagData$kurtPDF - gLagData$lagKurt))))
residualPlot(lm((gLagData$zScore ~ I(gLagData$meanPDF - gLagData$lagMean) + I(gLagData$varPDF  - gLagData$lagVar) +
                   I(gLagData$skewPDF - gLagData$lagSkew) + I(gLagData$kurtPDF - gLagData$lagKurt))))
summary(lm((gLagData$zScore ~ I(gLagData$meanPDF - gLagData$lagMean) + I(gLagData$varPDF  - gLagData$lagVar) +
              I(gLagData$skewPDF - gLagData$lagSkew) + I(gLagData$kurtPDF - gLagData$lagKurt))))

plot(gLagData$zScore[gLagData$zScore >= quantile(gLagData$zScore,0.75)],
     (gLagData$meanPDF / gLagData$lagMean)[gLagData$zScore >= quantile(gLagData$zScore,0.75)], cex=0.2)

plot(gLagData$meanPDF, gLagData$varPDF, cex=0.1)
plot(gLagData$meanPDF, gLagData$skewPDF, cex=0.1, ylim=c(-10,10))
plot(gLagData$meanPDF, gLagData$kurtPDF, cex=0.1, ylim=c(-10,10))
plot(gLagData$varPDF, gLagData$skewPDF, cex=0.1, ylim=c(-10,10))
plot(gLagData$varPDF, gLagData$kurtPDF, cex=0.1, ylim=c(-100,100))
plot(gLagData$skewPDF, gLagData$kurtPDF, cex=0.1, ylim=c(-100,100))
plot(gLagData$skewPDF ^ 2, gLagData$kurtPDF, cex=0.1, ylim=c(-100,100))

summary(lm(gLagData$meanPDF ~ gLagData$lagMean))
residualPlot(lm(gLagData$meanPDF ~ gLagData$lagMean))
summary(lm(gLagData$varPDF ~ gLagData$lagVar))
residualPlot(lm(gLagData$varPDF ~ gLagData$lagVar))
summary(lm(gLagData$skewPDF ~ gLagData$lagSkew))
residualPlot(lm(gLagData$skewPDF ~ gLagData$lagSkew))
summary(lm(gLagData$kurtPDF ~ gLagData$lagKurt))
residualPlot(lm(gLagData$kurtPDF ~ gLagData$lagKurt))

summary(lm(gLagData$meanPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND)))
summary(lm(gLagData$varPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND)))
summary(lm(gLagData$skewPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND)))
summary(lm(gLagData$kurtPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND)))

summary(lm(gLagData$meanPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                               -leadMean, -leadVar, -leadSkew, -leadKurt,
                                               -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(gLagData$varPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                               -leadMean, -leadVar, -leadSkew, -leadKurt,
                                               -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(gLagData$skewPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                               -leadMean, -leadVar, -leadSkew, -leadKurt,
                                               -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(gLagData$kurtPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                               -leadMean, -leadVar, -leadSkew, -leadKurt,
                                               -l10Mean, -l10Var, -l10Skew, -l10Kurt)))

cor(cbind(gLagData$meanPDF - gLagData$lagMean, gLagData$varPDF - gLagData$lagVar, 
          gLagData$skewPDF - gLagData$lagSkew, gLagData$kurtPDF - gLagData$lagKurt), use='complete.obs')
cor(cbind(gLagData$meanPDF, gLagData$varPDF, gLagData$skewPDF, gLagData$kurtPDF), use='complete.obs')

summary(lm(gLagData$leadMean - gLagData$meanPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                               -leadMean, -leadVar, -leadSkew, -leadKurt,
                                               -l10Mean, -l10Var, -l10Skew, -l10Kurt)))

summary(lm(gLagData$leadVar - gLagData$varPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                              -leadMean, -leadVar, -leadSkew, -leadKurt,
                                              -l10Mean, -l10Var, -l10Skew, -l10Kurt)))

summary(lm(gLagData$leadSkew - gLagData$skewPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                               -leadMean, -leadVar, -leadSkew, -leadKurt,
                                               -l10Mean, -l10Var, -l10Skew, -l10Kurt)))

summary(lm(gLagData$leadKurt - gLagData$kurtPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                               -leadMean, -leadVar, -leadSkew, -leadKurt,
                                               -l10Mean, -l10Var, -l10Skew, -l10Kurt)))

summary(lm(gLagData$l10Mean - gLagData$meanPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                                                   -leadMean, -leadVar, -leadSkew, -leadKurt,
                                                                   -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(gLagData$l10Var - gLagData$varPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                                                  -leadMean, -leadVar, -leadSkew, -leadKurt,
                                                                  -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(gLagData$l10Skew - gLagData$skewPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                                                   -leadMean, -leadVar, -leadSkew, -leadKurt,
                                                                   -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(gLagData$l10Kurt - gLagData$kurtPDF ~ ., data = select(gLagData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                                                   -leadMean, -leadVar, -leadSkew, -leadKurt,
                                                                   -l10Mean, -l10Var, -l10Skew, -l10Kurt)))

summary(lm(gLagData$leadMean ~ gLagData$varPDF + gLagData$skewPDF + gLagData$kurtPDF))
summary(lm(gLagData$leadVar ~ gLagData$meanPDF + gLagData$skewPDF + gLagData$kurtPDF))
summary(lm(gLagData$leadSkew ~ gLagData$meanPDF + gLagData$varPDF + gLagData$kurtPDF))
summary(lm(gLagData$leadKurt ~ gLagData$meanPDF + gLagData$varPDF + gLagData$skewPDF))

summary(lm(gLagData$leadMean ~ gLagData$meanPDF + gLagData$varPDF + gLagData$skewPDF + gLagData$kurtPDF))
summary(lm(gLagData$leadVar ~ gLagData$meanPDF + gLagData$varPDF + gLagData$skewPDF + gLagData$kurtPDF))
summary(lm(gLagData$leadSkew ~ gLagData$meanPDF + gLagData$varPDF + gLagData$skewPDF + gLagData$kurtPDF))
summary(lm(gLagData$leadKurt ~ gLagData$meanPDF + gLagData$varPDF + gLagData$skewPDF + gLagData$kurtPDF))

summary(lm(gLagData$leadMean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean)))
residualPlot(lm(gLagData$leadMean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean)), cex=0.1)
summary(lm(gLagData$leadVar - gLagData$varPDF ~ I(gLagData$varPDF - gLagData$lagVar)))
residualPlot(lm(gLagData$leadVar - gLagData$varPDF ~ I(gLagData$varPDF - gLagData$lagVar)), cex=0.1)
summary(lm(gLagData$leadSkew - gLagData$skewPDF ~ I(gLagData$skewPDF - gLagData$lagSkew)))
residualPlot(lm(gLagData$leadSkew - gLagData$skewPDF ~ I(gLagData$skewPDF - gLagData$lagSkew)), cex=0.1)
summary(lm(gLagData$leadKurt - gLagData$kurtPDF ~ I(gLagData$kurtPDF - gLagData$lagKurt)))
residualPlot(lm(gLagData$leadKurt - gLagData$kurtPDF ~ I(gLagData$kurtPDF - gLagData$lagKurt)), cex=0.1)

summary(lm(gLagData$leadMean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt)))
residualPlot(lm(gLagData$leadMean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt)))
100*sum(sign(gLagData$leadMean - gLagData$meanPDF) == 
          sign(predict(lm(gLagData$leadMean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
                            I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
                            I(gLagData$kurtPDF - gLagData$lagKurt))))) / 
  length(gLagData$leadMean - gLagData$meanPDF)

summary(lm(gLagData$leadVar - gLagData$varPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt)))
residualPlot(lm(gLagData$leadVar - gLagData$varPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt)))
100*sum(sign(gLagData$leadVar - gLagData$varPDF) == 
          sign(predict(lm(gLagData$leadVar - gLagData$varPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
                            I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
                            I(gLagData$kurtPDF - gLagData$lagKurt))))) / 
  length(gLagData$leadMean - gLagData$meanPDF)

summary(lm(gLagData$leadSkew - gLagData$skewPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt)))
residualPlot(lm(gLagData$leadSkew - gLagData$skewPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt)))
100*sum(sign(gLagData$leadSkew - gLagData$skewPDF) == 
          sign(predict(lm(gLagData$leadSkew - gLagData$skewPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
                            I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
                            I(gLagData$kurtPDF - gLagData$lagKurt))))) / 
  length(gLagData$leadMean - gLagData$meanPDF)

summary(lm(gLagData$leadKurt - gLagData$kurtPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt)))
residualPlot(lm(gLagData$leadKurt - gLagData$kurtPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt)))
100*sum(sign(gLagData$leadKurt - gLagData$kurtPDF) == 
          sign(predict(lm(gLagData$leadKurt - gLagData$kurtPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
                            I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
                            I(gLagData$kurtPDF - gLagData$lagKurt))))) / 
  length(gLagData$leadMean - gLagData$meanPDF)

summary(lm(gLagData$leadMean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt) + I(gLagData$lagMean - gLagData$l2Mean) + 
             I(gLagData$lagVar - gLagData$l2Var) + I(gLagData$lagSkew - gLagData$l2Skew) +
             I(gLagData$lagKurt - gLagData$l2Kurt) + I(gLagData$l2Mean - gLagData$l3Mean) + 
             I(gLagData$l2Var - gLagData$l3Var) + I(gLagData$l2Skew - gLagData$l3Skew) +
             I(gLagData$l2Kurt - gLagData$l3Kurt)))
summary(lm(gLagData$leadVar - gLagData$varPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt) + I(gLagData$lagMean - gLagData$l2Mean) + 
             I(gLagData$lagVar - gLagData$l2Var) + I(gLagData$lagSkew - gLagData$l2Skew) +
             I(gLagData$lagKurt - gLagData$l2Kurt) + I(gLagData$l2Mean - gLagData$l3Mean) + 
             I(gLagData$l2Var - gLagData$l3Var) + I(gLagData$l2Skew - gLagData$l3Skew) +
             I(gLagData$l2Kurt - gLagData$l3Kurt)))
summary(lm(gLagData$leadSkew - gLagData$skewPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt) + I(gLagData$lagMean - gLagData$l2Mean) + 
             I(gLagData$lagVar - gLagData$l2Var) + I(gLagData$lagSkew - gLagData$l2Skew) +
             I(gLagData$lagKurt - gLagData$l2Kurt) + I(gLagData$l2Mean - gLagData$l3Mean) + 
             I(gLagData$l2Var - gLagData$l3Var) + I(gLagData$l2Skew - gLagData$l3Skew) +
             I(gLagData$l2Kurt - gLagData$l3Kurt)))
summary(lm(gLagData$leadKurt - gLagData$kurtPDF ~ I(gLagData$meanPDF - gLagData$lagMean) + 
             I(gLagData$varPDF - gLagData$lagVar) + I(gLagData$skewPDF - gLagData$lagSkew) +
             I(gLagData$kurtPDF - gLagData$lagKurt) + I(gLagData$lagMean - gLagData$l2Mean) + 
             I(gLagData$lagVar - gLagData$l2Var) + I(gLagData$lagSkew - gLagData$l2Skew) +
             I(gLagData$lagKurt - gLagData$l2Kurt) + I(gLagData$l2Mean - gLagData$l3Mean) + 
             I(gLagData$l2Var - gLagData$l3Var) + I(gLagData$l2Skew - gLagData$l3Skew) +
             I(gLagData$l2Kurt - gLagData$l3Kurt)))

summary(lm(gLagData$l10Mean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean)))
residualPlot(lm(gLagData$l10Mean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean)), cex=0.1)
summary(lm(gLagData$l10Var - gLagData$varPDF ~ I(gLagData$varPDF - gLagData$lagVar)))
residualPlot(lm(gLagData$l10Var - gLagData$varPDF ~ I(gLagData$varPDF - gLagData$lagVar)), cex=0.1)
summary(lm(gLagData$l10Skew - gLagData$skewPDF ~ I(gLagData$skewPDF - gLagData$lagSkew)))
residualPlot(lm(gLagData$l10Skew - gLagData$skewPDF ~ I(gLagData$skewPDF - gLagData$lagSkew)), cex=0.1)
summary(lm(gLagData$l10Kurt - gLagData$kurtPDF ~ I(gLagData$kurtPDF - gLagData$lagKurt)))
residualPlot(lm(gLagData$l10Kurt - gLagData$kurtPDF ~ I(gLagData$kurtPDF - gLagData$lagKurt)), cex=0.1)

plot(gLagData$leadMean - gLagData$meanPDF, type='l')
lines(predict(lm(gLagData$leadMean - gLagData$meanPDF ~ I(gLagData$meanPDF - gLagData$lagMean))), col='blue')
plot(gLagData$leadVar - gLagData$varPDF, type='l')
lines(predict(lm(gLagData$leadVar - gLagData$varPDF ~ I(gLagData$varPDF - gLagData$lagVar))), col='blue')
plot(gLagData$leadSkew - gLagData$skewPDF, type='l')
lines(predict(lm(gLagData$leadSkew - gLagData$skewPDF ~ I(gLagData$skewPDF - gLagData$lagSkew))), col='blue')
plot(gLagData$leadKurt - gLagData$kurtPDF, type='l')
lines(predict(lm(gLagData$leadKurt - gLagData$kurtPDF ~ I(gLagData$kurtPDF - gLagData$lagKurt))), col='blue')

# Top decile
quantile(gLagData$zScore, 0.9, na.rm = TRUE)
# Bottom decile
quantile(gLagData$zScore, 0.1, na.rm = TRUE)
topDec <- gLagData[gLagData$zScore >= quantile(gLagData$zScore,0.99,na.rm=TRUE,names=FALSE),]
botDec <- gLagData[gLagData$zScore <= quantile(gLagData$zScore,0.01,na.rm=TRUE,names=FALSE),]
topDec <- gLagData[gLagData$zScore >= quantile(gLagData$zScore,0.9,na.rm=TRUE,names=FALSE),]
botDec <- gLagData[gLagData$zScore <= quantile(gLagData$zScore,0.1,na.rm=TRUE,names=FALSE),]

residualPlot(lm((botDec$zScore ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF  - botDec$lagVar) +
                   I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt))))
summary(lm((botDec$zScore ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF  - botDec$lagVar) +
              I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt))))
residualPlot(lm((topDec$zScore ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF  - topDec$lagVar) +
                   I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt))))
summary(lm((topDec$zScore ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF  - topDec$lagVar) +
              I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt))))

plot((botDec$leadMean - botDec$meanPDF) / (botDec$meanPDF - botDec$lagMean), cex=0.1, ylim=c(-20,20))
qqnorm(((botDec$leadMean - botDec$meanPDF) / (botDec$meanPDF - botDec$lagMean))[(botDec$leadMean - botDec$meanPDF)!=0], ylim=c(-200,200))
plot((botDec$leadVar - botDec$varPDF) / (botDec$varPDF - botDec$lagVar), cex=0.1, ylim=c(-20,20))
qqnorm(((botDec$leadVar - botDec$varPDF) / (botDec$varPDF - botDec$lagVar))[(botDec$leadVar - botDec$varPDF)!=0], ylim=c(-200,200))
plot((botDec$leadSkew - botDec$skewPDF) / (botDec$skewPDF - botDec$lagSkew), cex=0.1, ylim=c(-20,20))
qqnorm(((botDec$leadSkew - botDec$skewPDF) / (botDec$skewPDF - botDec$lagSkew))[(botDec$leadSkew - botDec$skewPDF)!=0], ylim=c(-200,200))
plot((botDec$leadKurt - botDec$kurtPDF) / (botDec$kurtPDF - botDec$lagKurt), cex=0.1, ylim=c(-20,20))
qqnorm(((botDec$leadKurt - botDec$kurtPDF) / (botDec$kurtPDF - botDec$lagKurt))[(botDec$leadKurt - botDec$kurtPDF)!=0], ylim=c(-200,200))

summary(lm(botDec$leadMean - botDec$meanPDF ~ I(botDec$meanPDF - botDec$lagMean)))
summary(lm(botDec$leadVar - botDec$varPDF ~ I(botDec$varPDF - botDec$lagVar)))
summary(lm(botDec$leadSkew - botDec$skewPDF ~ I(botDec$skewPDF - botDec$lagSkew)))
summary(lm(botDec$leadKurt - botDec$kurtPDF ~ I(botDec$kurtPDF - botDec$lagKurt)))

summary(lm(botDec$l10Mean - botDec$meanPDF ~ I(botDec$meanPDF - botDec$lagMean)))
summary(lm(botDec$l10Var - botDec$varPDF ~ I(botDec$varPDF - botDec$lagVar)))
summary(lm(botDec$l10Skew - botDec$skewPDF ~ I(botDec$skewPDF - botDec$lagSkew)))
summary(lm(botDec$l10Kurt - botDec$kurtPDF ~ I(botDec$kurtPDF - botDec$lagKurt)))

plot(botDec$l10Mean - botDec$meanPDF, type='l')
lines(predict(lm(botDec$l10Mean - botDec$meanPDF ~ I(botDec$meanPDF - botDec$lagMean))), col='blue')
plot(botDec$l10Var - botDec$varPDF, type='l')
lines(predict(lm(botDec$l10Var - botDec$varPDF ~ I(botDec$varPDF - botDec$lagVar))), col='blue')
plot(botDec$l10Skew - botDec$skewPDF, type='l')
lines(predict(lm(botDec$l10Skew - botDec$skewPDF ~ I(botDec$skewPDF - botDec$lagSkew))), col='blue')
plot(botDec$l10Kurt - botDec$kurtPDF, type='l')
lines(predict(lm(botDec$l10Kurt - botDec$kurtPDF ~ I(botDec$kurtPDF - botDec$lagKurt))), col='blue')


plot((topDec$leadMean - topDec$meanPDF) / (topDec$meanPDF - topDec$lagMean), cex=0.1, ylim=c(-20,20))
qqnorm(((topDec$leadMean - topDec$meanPDF) / (topDec$meanPDF - topDec$lagMean))[(topDec$leadMean - topDec$meanPDF)!=0], ylim=c(-200,200))
plot((topDec$leadVar - topDec$varPDF) / (topDec$varPDF - topDec$lagVar), cex=0.1, ylim=c(-20,20))
qqnorm(((topDec$leadVar - topDec$varPDF) / (topDec$varPDF - topDec$lagVar))[(topDec$leadVar - topDec$varPDF)!=0], ylim=c(-200,200))
plot((topDec$leadSkew - topDec$skewPDF) / (topDec$skewPDF - topDec$lagSkew), cex=0.1, ylim=c(-20,20))
qqnorm(((topDec$leadSkew - topDec$skewPDF) / (topDec$skewPDF - topDec$lagSkew))[(topDec$leadSkew - topDec$skewPDF)!=0], ylim=c(-200,200))
plot((topDec$leadKurt - topDec$kurtPDF) / (topDec$kurtPDF - topDec$lagKurt), cex=0.1, ylim=c(-20,20))
qqnorm(((topDec$leadKurt - topDec$kurtPDF) / (topDec$kurtPDF - topDec$lagKurt))[(topDec$leadKurt - topDec$kurtPDF)!=0], ylim=c(-200,200))

summary(lm(topDec$leadMean - topDec$meanPDF ~ I(topDec$meanPDF - topDec$lagMean)))
summary(lm(topDec$leadVar - topDec$varPDF ~ I(topDec$varPDF - topDec$lagVar)))
summary(lm(topDec$leadSkew - topDec$skewPDF ~ I(topDec$skewPDF - topDec$lagSkew)))
residualPlot(lm(topDec$leadSkew - topDec$skewPDF ~ I(topDec$skewPDF - topDec$lagSkew)))
summary(lm(topDec$leadKurt - topDec$kurtPDF ~ I(topDec$kurtPDF - topDec$lagKurt)))
residualPlot(lm(topDec$leadKurt - topDec$kurtPDF ~ I(topDec$kurtPDF - topDec$lagKurt)))
summary(lm((topDec$leadKurt - topDec$kurtPDF)[abs(topDec$leadKurt - topDec$kurtPDF) < 5000] ~ 
             I((topDec$kurtPDF - topDec$lagKurt)[abs(topDec$leadKurt - topDec$kurtPDF) < 5000])))
residualPlot(lm((topDec$leadKurt - topDec$kurtPDF)[abs(topDec$leadKurt - topDec$kurtPDF) < 5000] ~ 
                  I((topDec$kurtPDF - topDec$lagKurt)[abs(topDec$leadKurt - topDec$kurtPDF) < 5000])))

summary(lm(topDec$leadMean - topDec$meanPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
             I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
summary(lm(topDec$leadVar - topDec$varPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
             I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
summary(lm(topDec$leadSkew - topDec$skewPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
             I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
residualPlot(lm(topDec$leadSkew - topDec$skewPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
                  I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
summary(lm(topDec$leadKurt - topDec$kurtPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
             I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
residualPlot(lm(topDec$leadKurt - topDec$kurtPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
                  I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))

summary(lm(topDec$l10Mean - topDec$meanPDF ~ I(topDec$meanPDF - topDec$lagMean)))
summary(lm(topDec$l10Var - topDec$varPDF ~ I(topDec$varPDF - topDec$lagVar)))
summary(lm(topDec$l10Skew - topDec$skewPDF ~ I(topDec$skewPDF - topDec$lagSkew)))
residualPlot(lm(topDec$l10Skew - topDec$skewPDF ~ I(topDec$skewPDF - topDec$lagSkew)))
summary(lm(topDec$l10Kurt - topDec$kurtPDF ~ I(topDec$kurtPDF - topDec$lagKurt)))
residualPlot(lm(topDec$l10Kurt - topDec$kurtPDF ~ I(topDec$kurtPDF - topDec$lagKurt)))

plot(topDec$l10Mean - topDec$meanPDF, type='l')
lines(predict(lm(topDec$l10Mean - topDec$meanPDF ~ I(topDec$meanPDF - topDec$lagMean))), col='blue')
plot(topDec$l10Var - topDec$varPDF, type='l')
lines(predict(lm(topDec$l10Var - topDec$varPDF ~ I(topDec$varPDF - topDec$lagVar))), col='blue')
plot(topDec$l10Skew - topDec$skewPDF, type='l')
lines(predict(lm(topDec$l10Skew - topDec$skewPDF ~ I(topDec$skewPDF - topDec$lagSkew))), col='blue')
plot(topDec$l10Kurt - topDec$kurtPDF, type='l')
lines(predict(lm(topDec$l10Kurt - topDec$kurtPDF ~ I(topDec$kurtPDF - topDec$lagKurt))), col='blue')


summary(lm(botDec$leadMean - botDec$meanPDF ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF - botDec$lagVar) + 
             I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt)))
residualPlot(lm(botDec$leadMean - botDec$meanPDF ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF - botDec$lagVar) + 
                  I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt)))
100*sum(sign(botDec$leadMean - botDec$meanPDF) == 
          sign(predict(lm(botDec$leadMean - botDec$meanPDF ~ I(botDec$meanPDF - botDec$lagMean) + 
                            I(botDec$varPDF - botDec$lagVar) + I(botDec$skewPDF - botDec$lagSkew) +
                            I(botDec$kurtPDF - botDec$lagKurt))))) / 
  length(botDec$leadMean - botDec$meanPDF)
summary(lm(botDec$leadVar - botDec$varPDF ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF - botDec$lagVar) + 
             I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt)))
residualPlot(lm(botDec$leadVar - botDec$varPDF ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF - botDec$lagVar) + 
                  I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt)))
summary(lm(botDec$leadSkew - botDec$skewPDF ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF - botDec$lagVar) + 
             I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt)))
residualPlot(lm(botDec$leadSkew - botDec$skewPDF ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF - botDec$lagVar) + 
                  I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt)))
summary(lm(botDec$leadKurt - botDec$kurtPDF ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF - botDec$lagVar) + 
             I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt)))
residualPlot(lm(botDec$leadKurt - botDec$kurtPDF ~ I(botDec$meanPDF - botDec$lagMean) + I(botDec$varPDF - botDec$lagVar) + 
                  I(botDec$skewPDF - botDec$lagSkew) + I(botDec$kurtPDF - botDec$lagKurt)))

summary(lm(topDec$leadMean - topDec$meanPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
             I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
residualPlot(lm(topDec$leadMean - topDec$meanPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
                  I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
summary(lm(topDec$leadVar - topDec$varPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
             I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
residualPlot(lm(topDec$leadMean - topDec$meanPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
                  I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
summary(lm(topDec$leadSkew - topDec$skewPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
             I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
residualPlot(lm(topDec$leadSkew - topDec$skewPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
                  I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
summary(lm(topDec$leadKurt - topDec$kurtPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
             I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))
residualPlot(lm(topDec$leadKurt - topDec$kurtPDF ~ I(topDec$meanPDF - topDec$lagMean) + I(topDec$varPDF - topDec$lagVar) + 
                  I(topDec$skewPDF - topDec$lagSkew) + I(topDec$kurtPDF - topDec$lagKurt)))


# Comparing with the realised physical distribution
pgData <- read.csv('pGroupData.csv')
pgData$Date <- as.Date(pgData$Date)
pgData$EXPIR_DATE <- as.Date(pgData$EXPIR_DATE)
summary(select(pgData, pMean, pVar, pSkew, pKurt))
intersect(colnames(pgData), colnames(gLagData))
dim(merge(pgData, gLagData, by=c('Ticker', 'Date', 'EXPIR_DATE', 'PUTCALLIND')))
pgData <- merge(pgData, gLagData, by=intersect(colnames(pgData), colnames(gLagData)))

plot(pgData$meanPDF, pgData$pMean, cex=0.2)
plot(pgData$varPDF, pgData$pVar, cex=0.2)
plot(pgData$skewPDF, pgData$pSkew, cex=0.2)
plot(pgData$kurtPDF, pgData$pKurt, cex=0.2)

plot(gLagData$skewPDF, gLagData$kurtPDF, cex=0.1)
plot(pgData$pSkew, pgData$pKurt, cex=0.2)
plot(gLagData$skewPDF ^ 2, gLagData$kurtPDF, cex=0.1)
plot(pgData$pSkew ^ 2, pgData$pKurt, cex=0.2)

summary(lm(pgData$pMean ~ pgData$pVar + pgData$pSkew + pgData$pKurt))
summary(lm(pgData$pVar ~ pgData$pMean + pgData$pSkew + pgData$pKurt))
summary(lm(pgData$pSkew ~ pgData$pMean + pgData$pVar + pgData$pKurt))
summary(lm(pgData$pKurt ~ pgData$pMean + pgData$pVar + pgData$pSkew))

summary(lm(pgData$pMean ~ ., data=select(pgData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                         -pMean, -pVar, -pSkew, -pKurt, -leadMean, -leadVar, -leadSkew, -leadKurt,
                                         -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(pgData$pVar ~ ., data=select(pgData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                         -pMean, -pVar, -pSkew, -pKurt, -leadMean, -leadVar, -leadSkew, -leadKurt,
                                         -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(pgData$pSkew ~ ., data=select(pgData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                         -pMean, -pVar, -pSkew, -pKurt, -leadMean, -leadVar, -leadSkew, -leadKurt,
                                         -l10Mean, -l10Var, -l10Skew, -l10Kurt)))
summary(lm(pgData$pKurt ~ ., data=select(pgData, -Ticker, -Date, -EXPIR_DATE, -PUTCALLIND,
                                         -pMean, -pVar, -pSkew, -pKurt, -leadMean, -leadVar, -leadSkew, -leadKurt,
                                         -l10Mean, -l10Var, -l10Skew, -l10Kurt)))

summary(lm(pgData$pMean ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF))
residualPlot(lm(pgData$pMean ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF), cex=0.2)
summary(lm(pgData$pVar ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF))
residualPlot(lm(pgData$pVar ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF), cex=0.2)
summary(lm(pgData$pSkew ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF))
residualPlot(lm(pgData$pSkew ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF), cex=0.2)
summary(lm(pgData$pKurt ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF))
residualPlot(lm(pgData$pKurt ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF), cex=0.2)

manyDays <- pgData[pgData$EXPIR_DATE - pgData$Date > 50,]
summary(lm(manyDays$pMean ~ manyDays$meanPDF + manyDays$varPDF + manyDays$skewPDF + manyDays$kurtPDF))
residualPlot(lm(manyDays$pMean ~ manyDays$meanPDF + manyDays$varPDF + manyDays$skewPDF + manyDays$kurtPDF), cex=0.2)
summary(lm(manyDays$pVar ~ manyDays$meanPDF + manyDays$varPDF + manyDays$skewPDF + manyDays$kurtPDF))
residualPlot(lm(manyDays$pVar ~ manyDays$meanPDF + manyDays$varPDF + manyDays$skewPDF + manyDays$kurtPDF), cex=0.2)
summary(lm(manyDays$pSkew ~ manyDays$meanPDF + manyDays$varPDF + manyDays$skewPDF + manyDays$kurtPDF))
residualPlot(lm(manyDays$pSkew ~ manyDays$meanPDF + manyDays$varPDF + manyDays$skewPDF + manyDays$kurtPDF), cex=0.2)
summary(lm(manyDays$pKurt ~ manyDays$meanPDF + manyDays$varPDF + manyDays$skewPDF + manyDays$kurtPDF))
residualPlot(lm(manyDays$pKurt ~ manyDays$meanPDF + manyDays$varPDF + manyDays$skewPDF + manyDays$kurtPDF), cex=0.2)

summary(lm(pgData$pSkew ~ pgData$skewPDF + pgData$kurtPDF))
residualPlot(lm(pgData$pSkew ~ pgData$skewPDF + pgData$kurtPDF), cex=0.2)
summary(lm(pgData$pKurt ~ pgData$skewPDF + pgData$kurtPDF))
residualPlot(lm(pgData$pKurt ~ pgData$skewPDF + pgData$kurtPDF), cex=0.2)
summary(lm(pgData$pSkew ~ pgData$skewPDF + pgData$kurtPDF + I(pgData$skewPDF ^2)))
residualPlot(lm(pgData$pSkew ~ pgData$skewPDF + pgData$kurtPDF + I(pgData$skewPDF ^2)), cex=0.2)
summary(lm(pgData$pKurt ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF + I(pgData$skewPDF ^2)))
residualPlot(lm(pgData$pKurt ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF + I(pgData$skewPDF ^2)), cex=0.2)

summary(lm(pgData$pMean ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF +
             I(pgData$skewPDF ^2) + I(pgData$meanPDF ^2)))
residualPlot(lm(pgData$pMean ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF +
                  I(pgData$skewPDF ^2) + I(pgData$meanPDF ^2)), cex=0.2)
summary(lm(pgData$pVar ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF +
             I(pgData$skewPDF ^2) + I(pgData$meanPDF ^2)))
residualPlot(lm(pgData$pVar ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF +
                  I(pgData$skewPDF ^2) + I(pgData$meanPDF ^2)), cex=0.2)
summary(lm(pgData$pSkew ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF +
             I(pgData$skewPDF ^2) + I(pgData$meanPDF ^2)))
residualPlot(lm(pgData$pSkew ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF +
                  I(pgData$skewPDF ^2) + I(pgData$meanPDF ^2)), cex=0.2)
summary(lm(pgData$pKurt ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF +
             I(pgData$skewPDF ^2) + I(pgData$meanPDF ^2)))
residualPlot(lm(pgData$pKurt ~ pgData$meanPDF + pgData$varPDF + pgData$skewPDF + pgData$kurtPDF +
                  I(pgData$skewPDF ^2) + I(pgData$meanPDF ^2)), cex=0.2)


