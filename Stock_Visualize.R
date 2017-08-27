library(quantmod)

getSymbols(c("AMZN"))

chartSeries(AMZN, subset='last 3 months')

addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)
