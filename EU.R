### Upload Data
MAEU <- read_csv("EU.csv")
MAEU$Time <- as.Date(MAEU$Time, "%m/%d/%Y")
#We're going to use xts which makes time series data a little
#easier to work with. (we used this above)
MAEU.xts <- xts(MAEU[, 4:6], order.by = MAEU$Time)


MAEU.xts <- na.omit(MAEU.xts)

Transactions.df.test <- ur.df(MAEU.xts$Transactions, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)
Valuation.df.test <- ur.df(MAEU.xts$Valuation, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)
GDP.df.test <- ur.df(MAEU.xts$GDP, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)


#### Choosing lags for Transactions and GDP

acf(MAEU.xts$Valuation)
pacf(MAEU.xts$Valuation)

auto.arima(MAEU.xts$Transactions, max.p = 6, max.q = 0,
           stationary = TRUE, seasonal = FALSE,
           ic = "aic")
#Create lags
MAEU.xts$Transactions.1 <- lag(MAEU.xts$Transactions) 
MAEU.xts$Transactions.2 <- lag(MAEU.xts$Transactions, 2) 
MAEU.xts$Transactions.3 <- lag(MAEU.xts$Transactions, 3)
MAEU.xts$Transactions.4 <- lag(MAEU.xts$Transactions, 4)

MAEU.xts$GDP.1 <- lag(MAEU.xts$GDP)
MAEU.xts$GDP.2 <- lag(MAEU.xts$GDP, 2)
MAEU.xts$GDP.3 <- lag(MAEU.xts$GDP, 3)
MAEU.xts$GDP.4 <- lag(MAEU.xts$GDP, 4)

ic.mat <- matrix(NA, nrow = 20, ncol = 2)

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:20) {
  mod.temp <- dynlm(Transactions ~ L(Transactions, 1:4) + L(GDP, 1:i), data = as.zoo(MAEU.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
}
print(ic.mat)

AR.4 <- dynlm(Transactions ~ L(Transactions, 1:4), as.zoo(MAEU.xts))
stargazer(AR.4, type = "text",
           keep.stat = c("n", "rsq"))

#### Choosing lags for Valuation and GDP

auto.arima(MAEU.xts$Valuation, max.p = 6, max.q = 0,
           stationary = TRUE, seasonal = FALSE,
           ic = "aic")
pacf(MAEU.xts$Valuation)

ic.mat <- matrix(NA, nrow = 20, ncol = 2)

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:20) {
  mod.temp <- dynlm(Valuation ~ L(GDP, 1:i), data = as.zoo(MAEU.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
}
print(ic.mat)

DL.2 <- dynlm(Valuation ~ L(GDP, 1) + L(GDP, 2), as.zoo(MAEU.xts))
stargazer(DL.2, type = "text",
          keep.stat = c("n", "rsq"))

linearHypothesis(DL.2, c("L(GDP, 1) = 0", "L(GDP, 2) = 0"),
                 vcov = sandwich)


