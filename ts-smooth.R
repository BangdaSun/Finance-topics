### Smoothing for time series data
#
# References:
#
#   https://cran.r-project.org/web/packages/smooth/vignettes/smooth.html
#   https://github.com/config-i1/smooth
#
# install.packages("zoo")
# install.packages("forecast")
# install.packages("nloptr")
# install.packages("Mcomp")

library(zoo)
library(forecast)
library(nloptr)
library(smooth)
library(Mcomp)

### Related functions

# es() - Exponential Smoothing;
# ssarima() - State-Space ARIMA, also known as Several Seasonalities ARIMA;
# ces() - Complex Exponential Smoothing;
# ges() - Generalised Exponential Smoothing;
# sma() - Simple Moving Average in state-space form;
# sim.es() and simulate() - simulation functions for Exponential Smoothing.

### Related methods

# summary(ourModel) – function prints brief output with explanation of what was fitted, with what parameters and errors;
# fitted(ourModel) – fitted values;
# forecast(ourModel) – point and interval forecasts. This is needed for compatibility with Rob Hyndman’s “forecast” package. forecast(ourModel) returns object of class forecastSmooth;
# residuals(ourModel) – residuals of constructed model;
# AIC(ourModel), BIC(ourModel) and AICc(ourModel) – information criteria of the constructed model. AICc() function is not a standard stats'' function and is introduced bysmooth’’;
# plot(ourModel) – plots states of constructed model. If number of states is higher than 10, then several graphs are produced;
# simulate(ourModel) – produces data simulated from provided model. Currently only available for ETS via simulate(es(...));
# summary(forecast(ourModel)) – prints point and interval forecasts;
# plot(forecast(ourModel)) – produces graph with actuals, forecast, fitted and intervals using graphmaker() function.


# build a simple moving average model
sma_model1 = sma(M3$N2457$x, h = 1, intervals = TRUE)
sma_model2 = sma(M3$N2457$x, h = 5, intervals = TRUE)


# the result has class 'smooth'
class(sma_model1)
class(M3$N2457$x)

# model outputs
sma_model1
sma_model1$forecast
sma_model1$residuals
sma_model1$fitted
sma_model1$logLik
