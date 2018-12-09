### Upload Data
MAUS <- read_csv("US.csv")
MAUS$Time <- as.Date(MAUS$Time, "%m/%d/%Y")
#We're going to use xts which makes time series data a little
#easier to work with. (we used this above)
MAUS.xts <- xts(MAUS[, 4:6], order.by = MAUS$Time)


MAUS.xts <- na.omit(MAUS.xts)


Valuation.df.test <- ur.df(MAUS.xts$Valuation, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)
GDP.df.test <- ur.df(MAUS.xts$GDP, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)
Transactions.df.test <- ur.df(MAUS.xts$Transactions, type = "trend", lags = 6, selectlags = "AIC")
summary(GDP.df.test)



### Choose lags for 
auto.arima(MAUS.xts$Transactions, max.p = 6, max.q = 0,
           stationary = TRUE, seasonal = FALSE, ic = "bic")

acf(MAUS.xts$Transactions)
pacf(MAUS.xts$Transactions)

MAUS.xts$Transactions.1 <- lag(MAUS.xts$Transactions) 
MAUS.xts$Transactions.2 <- lag(MAUS.xts$Transactions, 2) 
MAUS.xts$Transactions.3 <- lag(MAUS.xts$Transactions, 3)
MAUS.xts$Transactions.4 <- lag(MAUS.xts$Transactions, 4)


MAUS.xts$GDP.1 <- lag(MAUS.xts$GDP)
MAUS.xts$GDP.2 <- lag(MAUS.xts$GDP, 2)
MAUS.xts$GDP.3 <- lag(MAUS.xts$GDP, 3)
MAUS.xts$GDP.4 <- lag(MAUS.xts$GDP, 4)


ic.mat <- matrix(NA, nrow = 20, ncol = 2)

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:20) {
  mod.temp <- dynlm(Transactions ~ L(GDP, 1:i), data = as.zoo(MAUS.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
}
print(ic.mat)

dl.2 <- dynlm(Transactions ~ L(GDP, 1:2), as.zoo(MAUS.xts))
stargazer(dl.2, type = "text",
          keep.stat = c("n", "rsq"))

linearHypothesis(dl.2, c("L(GDP, 1) = 0", "L(GDP, 2) = 0"),
                 vcov = sandwich)

#### Valuation and GDP

auto.arima(MAUS.xts$Valuation, max.p = 6, max.q = 0,
           stationary = TRUE, seasonal = FALSE, ic = "aic")

ar.2 <- arima(MAUS.xts$Valuation, order = c(2,0,0))
acf(ar.2$residuals)

MAUS.xts$Valuation.1 <- lag(MAUS.xts$Valuation) 
MAUS.xts$Valuation.2 <- lag(MAUS.xts$Valuation, 2) 

acf(MAUS.xts$Valuation)
pacf(MAUS.xts$Valuation)

ic.mat <- matrix(NA, nrow = 20, ncol = 2)

colnames(ic.mat) <- c("AIC", "BIC")
for (i in 1:20) {
  mod.temp <- dynlm(Valuation ~ L(Valuation, 1:2) + L(GDP, 1:i), data = as.zoo(MAUS.xts))
  ic.mat[i, 1] <- AIC(mod.temp)
  ic.mat[i, 2] <- BIC(mod.temp)
}
print(ic.mat)

Adl.2.3 <- dynlm(Valuation ~ L(Valuation, 1) + L(Valuation, 2) + L(GDP, 1) + L(GDP, 2) + L(GDP,3), as.zoo(MAUS.xts))
stargazer(dl.2, type = "text",
          keep.stat = c("n", "rsq"))

stargazer( mod.ValEU.2.3, type = "text",
           keep.stat = c("n", "rsq"))

linearHypothesis(Adl.2.3,
                 c("L(GDP, 1) = 0", "L(GDP, 2) = 0", "L(GDP, 3) = 0"),
                 vcov = sandwich)



