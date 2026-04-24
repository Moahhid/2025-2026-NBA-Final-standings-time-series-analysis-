
# ---- Packages ----
library(forecast) #for forecasting 
library(tseries) #for adf test to check for stationarity of our time series
library(tsibble) #This package makes our cross validation process easier and efficient
library(fable) #allows us to perform time series forecasting with tidy and temporary tsibble format.
library(readxl) #Allows us to read excel files with many sheets
library(ggplot2) #to generate nice plots
library(zoo) #to compute rolling sample statistics


# ---- Path ----
file <- "C://Users//Moahhid Amir//OneDrive - McMaster University//Documents//courses//STATS 4A03//Project//My project//Season end standings//NBA_game_log_2025-2026.xlsx"


# ---- Load data ----
sheets <- excel_sheets(file)
logs <- lapply(sheets, function(s) read_excel(file, sheet = s))
names(logs) <- sheets

#preview the data.
attach(logs) #Allows us to access columns/array without having to call logs each time. 
head(LAL)
str(LAL)


# ---- Rolling net rating ----
LAL_rolling_NRtg <- rollmean(LAL$NRtg, k = 3) #k = 3 for 3 games or (~1 week rolling average)

LAL_rolling_NRtg <- ts(LAL_rolling_NRtg[1:length(LAL_rolling_NRtg)-1]) #we take the first n-1 values since the last value is NA due to the rolling mean computation.


# ---- Plots: actual vs rolling ----
par(mfrow = c(1, 2))

plot(LAL$NRtg, main = "Actual Lakers Net ratings per game",type = "l",
     ylab = "Net rating", xlab = "Time (games)")
plot(LAL_rolling_NRtg, main = "Lakers 3 game rolling average net ratings",
     ylab = "Net rating", xlab = "Time (games)")


# --- Stationarity checks ----
adf.test(LAL_rolling_NRtg)

diff_LAL_NRtg <- diff(LAL_rolling_NRtg) #taking the first difference

par(mfrow = c(1, 2)) 
acf(diff_LAL_NRtg, lag.max = 50) #ACF and PACF plots to determine model orders (AR and MA terms)
pacf(diff_LAL_NRtg, lag.max = 50)

adf.test(diff_LAL_NRtg) #adf test for stationarity of the differenced series

par(mfrow = c(1,1))
plot(diff_LAL_NRtg)


# ---- ARIMA modeling ----
fit_mod1 <- Arima(LAL_rolling_NRtg, order = c(1,1,0))
summary(fit_mod1)

fit_mod2 <- Arima(LAL_rolling_NRtg, order = c(2,1,0))
summary(fit_mod2)


# ---- Residual diagnostics ----

#plot residuals to check for any patterns which would indicate dependency of residuals on time or our fitted values. We want to see random scatter.
res1 <- residuals(fit_mod1)
plot(res1)

#QQ plot to check normality of residuals
qqnorm(res1)
qqline(res1, col = "red")

#Ljung test to check for any autocorrelation behavior in residuals.

Box.test(res1, lag = 52, type = "Ljung-Box")


# ---- Tsibble + rolling CV ----
NRtg_tsb <- as_tsibble(LAL_rolling_NRtg, index = week)
NRtg_tr <- NRtg_tsb |>
  stretch_tsibble(40, .step = 1) #take training sets of 20 and take steps of size 1.

fit_cv <- NRtg_tr |> model(arima = ARIMA(value ~ pdq(1,1,0))) |>
  forecast(h = 1) |>
  accuracy(NRtg_tsb)

print(fit_cv)


# ---- Final model & forecast ----
fit_mod1 <- Arima(LAL_rolling_NRtg, order = c(1, 1, 0))
NRtg_forecast <- forecast(fit_mod1, h = 7, level = 95)

# Plot
plot(NRtg_forecast, 
     main = "Net rating forecast for Los Angeles Lakers for remainder of season(7 games)",
     xlab = "Games", 
     ylab = "Rolling average net rating")