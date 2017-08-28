#Load the financial modelling package
library(quantmod)

#Load data from the stock of Apple
getSymbols(c("AAPL"))

#Technical Chart of the stock in the last 3 months
chartSeries(AAPL, subset = "last 3 months")

#Add a simple moving average to our technical chart
addSMA()

#Add a volatility indicator(Bollinger bands) to our Chart
addBBands()

#Add the RSI(relative Strength Indicator) to our chart

#Visualize the momentum of the stock
addMomentum()