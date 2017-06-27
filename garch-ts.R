# GARCH model
#
# reference 
# 
#   http://www.stat.nthu.edu.tw/~njhsu/Nonlinear%20Time%20Series/R%20functions%20for%20GARCH%20modeling.pdf
#

# install.packages(c("FinTS", "tseries", "fGarch"))
library(fGarch)
library(tseries)
library(FinTS)

### simulation

#   garch(1, 1) - func from fGarch
garch_model1 = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
class(garch_model1)
garch_model1_value = garchSim(garch_model1, n = 360)
class(garch_model1_value)
plot.ts(garch_model1_value, xlab = 'Date', ylab = 'value')

#   garch(1, 2)
garch_model2 = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = c(0.4, 0.4)))
garch_model2_value = garchSim(garch_model2, n = 500)
plot.ts(garch_model2_value, xlab = 'Date', ylab = 'value')

#   test arch effects
ArchTest(garch_model1_value)
par(mfrow = c(2, 2), mai = c(.8, .7, .5, .4))
acf(garch_model1_value)
acf(garch_model1_value ^ 2)
pacf(garch_model1_value)
pacf(garch_model1_value ^ 2)

#   build garch model - func from tseries
garch_model = garch(garch_model1_value, order = c(1, 1))
summary(garch_model)

#                     - func from fGarch
garch_model = garchFit(garch_model1_value ~ garch(1, 1))

#   diagonise
plot(garch_model)
