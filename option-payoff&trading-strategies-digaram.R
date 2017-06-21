
# Profit & loss function of call and put options

callOptPnL = function(stock, strike, cost) {
  # @param stock, stock price
  # @param strike, strike price
  # @param cost, constant cost
  return(max(0, stock - strike) - cost)
}

putOptPnL = function(stock, strike, cost) {
  # @param stock, stock price
  # @param strike, strike price
  # @param cost, constant cost
  return(max(0, strike - stock) - cost)
}

# Profit & loss plot for 4 different type of options
stockPrice = seq(from = 0, to = 80)

par(mfrow = c(2, 2))

pnlOflongCall = sapply(stockPrice, callOptPnL, strike = 20, cost = 5)
plot(stockPrice, pnlOflongCall, type = "l", main = "long call"); abline(h = 0, lty = 2)

pnlOflongPut = sapply(stockPrice, putOptPnL, strike = 20, cost = 5)
plot(stockPrice, pnlOflongPut, type = "l", main = "long put"); abline(h = 0, lty = 2)

pnlOfshortCall = -sapply(stockPrice, callOptPnL, strike = 20, cost = 5)
plot(stockPrice, pnlOfshortCall, type = "l", main = "short call"); abline(h = 0, lty = 2)

pnlOfshortPut = -sapply(stockPrice, putOptPnL, strike = 20, cost = 5)
plot(stockPrice, pnlOfshortPut, type = "l", main = "short put"); abline(h = 0, lty = 2)

### Vertical call
#   long call (low strike) + short call (higher)
pnlOflongcall_low   = sapply(stockPrice, callOptPnL, strike = 30, cost = 5)
pnlOfshortcall_high = -sapply(stockPrice, callOptPnL, strike = 50, cost = 5)
payoff              = pnlOflongcall_low + pnlOfshortcall_high
plot(stockPrice, payoff, type = "l", main = "vertical call"); abline(h = 0, lty = 2)

### Vertical put
#   long put (high strike) + short put (lower)
pnlOflong_put  = sapply(stockPrice, putOptPnL, strike = 50, cost = 5)
pnlOfshort_put = -sapply(stockPrice, putOptPnL, strike = 30, cost = 5)
payoff         = pnlOflong_put + pnlOfshort_put
plot(stockPrice, payoff, type = "l", main = "vertical short"); abline(h = 0, lty = 2)

### Long straddle
#   long put and call with same strike
pnlOflong_put  = sapply(stockPrice, putOptPnL, strike = 40, cost = 5)
pnlOflong_call = sapply(stockPrice, callOptPnL, strike = 40, cost = 5)
payoff         = pnlOflong_put + pnlOflong_call
plot(stockPrice, payoff, type = "l", main = "straddle"); abline(h = 0, lty = 2)

### Long strangle
#   long put (low strike) + long call (higher)
pnlOflong_put  = sapply(stockPrice, putOptPnL, strike = 40, cost = 5)
pnlOflong_call = sapply(stockPrice, callOptPnL, strike = 60, cost = 5)
payoff         = pnlOflong_put + pnlOflong_call
plot(stockPrice, payoff, type = "l", main = "strangle"); abline(h = 0, lty = 2)

### Butterfly
#   long call (low strike) + two short call (higher) + long call (highest)
pnlOflongcall_low  = sapply(stockPrice, callOptPnL, strike = 20, cost = 5)
pnlOfshortcall_mid = -sapply(stockPrice, callOptPnL, strike = 30, cost = 5)
pnlOflongcall_high = sapply(stockPrice, callOptPnL, strike = 40, cost = 5)
totalPayoff        = pnlOflongcall_low + 2 * pnlOfshortcall_mid + pnlOflongcall_high
plot(stockPrice, totalPayoff, type = "l", main = "butterfly"); abline(h = 0, lty = 2)

### Condor
#  two different strangles
#   strangle 1
pnlOflong_put        = sapply(stockPrice, putOptPnL, strike = 30, cost = 2)
pnlOflong_call       = sapply(stockPrice, callOptPnL, strike = 70, cost = 8)
payoff_longstrangle  = pnlOflong_put + pnlOflong_call
#   strangle 2
pnlOfshort_put       = -sapply(stockPrice, putOptPnL, strike = 40, cost = 6)
pnlOfshort_call      = -sapply(stockPrice, callOptPnL, strike = 60, cost = 7)
payoff_shortstrangle = pnlOfshort_put + pnlOfshort_call
payoff               = payoff_longstrangle + payoff_shortstrangle
plot(stockPrice, payoff, type = "l", main = "condor"); abline(h = 0, lty = 2)
