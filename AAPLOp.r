rm(list=ls())
setwd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
data <- read.csv("AAPLOptions.csv"); datacopy <- data
# install.packages(c("plyr", "tidyr", "dplyr", "lubridate", "xts", "PerformanceAnalytics",
#                   "lmtest", "olsrr", "robustbase", "car", "stargazer"))
library(plyr); library(tidyr); library(dplyr); library(lubridate); library(xts);
library(PerformanceAnalytics); library(lmtest); library(olsrr); library(robustbase);
library(car); library(stargazer)

data <- datacopy
data <- subset(data,!is.na(impl_volatility))
# All are American options 
data <- data %>% select(-ticker, -issuer, -index_flag, -exercise_style)
data$price <- (data$best_bid + data$best_offer) / 2
data$strike_price <- data$strike_price / 1000
data$date <- as.Date(data$date, format="%d/%m/%Y")
data$exdate <- as.Date(data$exdate, format="%d/%m/%Y")
data <- data %>% group_by(date)
data$dtm <- data$exdate - data$date
calls <- data %>% filter(cp_flag=="C") %>% select(-cp_flag) %>% group_by(date, exdate)
puts <- data %>% filter(cp_flag=="P") %>% select(-cp_flag) %>% group_by(date, exdate)

firstcalls <- calls[calls$date==calls$date[1] & calls$exdate==calls$exdate[1],]
plot(firstcalls$strike_price, firstcalls$price, main="Call Prices")
plot(firstcalls$strike_price, firstcalls$impl_volatility, main="Call Volatility Smile")
firstputs <- puts[puts$date==puts$date[1] & puts$exdate==puts$exdate[1],]
plot(firstputs$strike_price, firstputs$price, main="Put Prices")
plot(firstputs$strike_price, firstputs$impl_volatility, main="Put Volatility Smile")

lastcalls <- calls[calls$date==tail(calls$date,1) & calls$exdate==tail(calls$exdate,1),]
plot(lastcalls$strike_price, lastcalls$price, main="Call Prices")
plot(lastcalls$strike_price, lastcalls$impl_volatility, main="Call Volatility Smile")
lastputs <- puts[puts$date==tail(puts$date,1) & puts$exdate==tail(puts$exdate,1),]
plot(lastputs$strike_price, lastputs$price, main="Put Prices")
plot(lastputs$strike_price, lastputs$impl_volatility, main="Put Volatility Smile")

firstop <- data %>% filter(optionid == optionid[1])

# First pdf point had to be left out as it was massively larger than all others 
# Large jump between first two strikes, so huge pdf value 
but = data.frame(rep(0, length(firstcalls$strike_price)))
colnames(but) = "pdf"
but$pdf <- lag(firstcalls$price) + lead(firstcalls$price) - 2*firstcalls$price
but$pdf <- but$pdf %>% replace_na(0)
but$pdf[but$pdf > 1] = 0
# but$pdf <- but$pdf / sum(but$pdf)
plot(but$pdf, type="l", main="Call PDF")
mean(but$pdf)
sd(but$pdf)
skewness(but$pdf)
kurtosis(but$pdf, method = "moment")

but$cdf = rep(0, length(firstcalls$strike_price))
but$cdf <- firstcalls$price-lead(firstcalls$price)
but$cdf <- but$cdf %>% replace_na(0)
but$cdf <- but$cdf / max(but$cdf)
plot(but$cdf, type="l", main="Call CDF")

# Want to compare risk-neutral distribution and physical 
mostdtm <- calls[calls$dtm==max(calls$dtm),]
fullData <- calls %>% filter(strike_price %in% mostdtm$strike_price, exdate %in% mostdtm$exdate)
plot(fullData$dtm[fullData$strike_price==fullData$strike_price[1]], fullData$price[fullData$strike_price==fullData$strike_price[1]], type='l')
call1 <- fullData[fullData$strike_price==fullData$strike_price[1],]
call1$ret <- log(call1$price/lag(call1$price))
call1 <- drop_na(call1)
plot(density(call1$ret), type='l')
distcall1 <- call1 %>% select(strike_price)
distcall1$pdf <- distcall1$strike_price * 0
distcall1$pdf <- lag(call1$price) + lead(call1$price) - 2*call1$price
distcall1$pdf <- distcall1$pdf %>% replace_na(0)
# Only one strike as it is the first option 
# Need to use all the options with the same maturity to get PDF 








# S&P Constituent prices 
spcon <- read.csv("SPWRDS.csv")
write.csv(spcon$gvkey, "SPCon.txt", row.names = FALSE)
spp = read.csv("SPPrices.csv")
colnames(spp) = c("gvkey", "iid", "date", "tic", "price", "trfd")
spp$ret <- log(spp$price/lag(spp$price))
spp <- spp %>% group_by(gvkey) %>% slice(-1)
spdays <- merge(spp, spcon, by = "gvkey") %>% select(-gvkeyx)
spdays$date <- as.Date(spdays$date, format="%d/%m/%Y")
spdays$from <- as.Date(spdays$from, format="%d/%m/%Y")
spdays$thru <- as.Date(spdays$thru, format="%d/%m/%Y")
spdays$thru[is.na(spdays$thru)] = "2030-01-01"

spdays <- spdays %>% filter(date >= from & date <= thru) %>% select(-tic.y, -trfd)
spdays <- spdays %>% group_by(date, gvkey)
colnames(spdays)[4] = "tic"
gvkey1 <- spdays[spdays$gvkey==spdays$gvkey[1],]
gvkey1$sortret <- sort(gvkey1$ret)
x <- seq(-12, 5.5, length=nrow(gvkey1))*sd(gvkey1$sortret)+mean(gvkey1$sortret)
plot(dnorm(gvkey1$sortret, mean(gvkey1$sortret), sd(gvkey1$sortret)))
lines(dnorm(x, mean(gvkey1$sortret), sd(gvkey1$sortret)))
skewness(gvkey1$ret)
kurtosis(gvkey1$ret, method = "moment")
(min(gvkey1$ret)-mean(gvkey1$ret))/sd(gvkey1$ret)
(max(gvkey1$ret)-mean(gvkey1$ret))/sd(gvkey1$ret)



