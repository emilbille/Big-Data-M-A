library(tidyverse); library(AER); library(stargazer); library(dynlm);
library(quantmod); library(forecast); library(strucchange); library(readr);
library(vars); library(xts); library(mfx)


### Upload Data
MA <- read_csv("ALL.csv")
MA$Time <- as.Date(MA$Time, "%m/%d/%Y")
#We're going to use xts which makes time series data a little
#easier to work with. (we used this above)
MA.xts <- xts(MA[, 4:6], order.by = MA$Time)


#ADF test
GDP.df.test <- ur.df(MA.xts$GDP, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)

Transactions.df.test <- ur.df(MA.xts$GDP, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)

Valuation.df.test <- ur.df(MA.xts$GDP, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)

### Choosing lags for Transactions and GDP

acf(MA.xts$Valuation)
pacf(MA.xts$Transactions)


auto.arima(MA.xts$Transactions, max.p = 6, max.q = 0,
           stationary = TRUE, seasonal = FALSE,
           ic = "bic")
ar.1 <- arima(MA.xts$Transactions, order = c(14,0,0))
acf(ar.1$residuals)

#Create lags
MA.xts$Transactions.1 <- lag(MA.xts$Transactions) 
MA.xts$Transactions.2 <- lag(MA.xts$Transactions, 2) 
MA.xts$Transactions.3 <- lag(MA.xts$Transactions, 3) 
MA.xts$Transactions.4 <- lag(MA.xts$Transactions, 4)


MA.xts$GDP.1 <- lag(MA.xts$GDP)
MA.xts$GDP.2 <- lag(MA.xts$GDP, 2)
MA.xts$GDP.3 <- lag(MA.xts$GDP, 3)
MA.xts$GDP.4 <- lag(MA.xts$GDP, 4)
MA.xts$GDP.5 <- lag(MA.xts$GDP, 5)


ic.mat <- matrix(NA, nrow = 20, ncol = 2)

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:20) {
  mod.temp <- dynlm(Transactions ~ L(Transactions, 1:4) + L(GDP, 1:i), data = as.zoo(MA.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
}
print(ic.mat)

adl.4.3 <- dynlm(Transactions ~ L(Transactions, 1) + L(Transactions, 2) + L(Transactions, 3) 
                 + L(Transactions, 4) + L(GDP, 1) + L(GDP, 2) + L(GDP, 3), as.zoo(MA.xts))
stargazer( adl.4.3, type = "text",
          keep.stat = c("n", "rsq"))

### Granger causality test
linearHypothesis(adl.4.3, c("L(GDP, 1) = 0", "L(GDP, 2) = 0", "L(GDP, 3) = 0"),
                 vcov = sandwich)

##### Valuation


# Choosing lags for Valuation and GDP

acf(MA.xts$Valuation)
pacf(MA.xts$Valuation)


auto.arima(MA.xts$Valuation, max.p = 6, max.q = 0,
           stationary = TRUE, seasonal = FALSE,
           ic = "bic")

ic.mat <- matrix(NA, nrow = 20, ncol = 2)

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:20) {
  mod.temp <- dynlm(Valuation ~ L(GDP, 1:i), data = as.zoo(MA.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
}
print(ic.mat)


dl.2 <- dynlm(Valuation ~ L(GDP, 1) + L(GDP, 2), as.zoo(MA.xts))
stargazer(dl.2, type = "text",
           keep.stat = c("n", "rsq"))

linearHypothesis(dl.2, c("L(GDP, 1) = 0", "L(GDP, 2) = 0"),
                 vcov = sandwich)


