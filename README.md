
# Algorithmic Finance personal Projects (Coded in R)
## This repository contains  my progress in Algorithmic Finance. 
## The following programs can be used by individual investors and analysts to easily visualize and analize a variety of stocks using the powerful programming language R
### I included my personal financial indicator programmed in R with a sample strategy including a portfolio allocation at the end

#### 2- *The program "Blackscholes" is a simple code that computes the fair price of an option using the well-known Black-Scholes-Merton Formula*
```
# Black-Scholes Option Formula
# the constants c and p from black scholes are programmed in the vector V
blackscholes <- function(S, X, rf, T, sigma) {
  v <- c(2)
  
  d1 <- (log(S/X)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  v[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
  v[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)
  
  v
}

```
#### 3- *The  program "Stock_Visualize" simply plots a technical chart of any stock in within a specific timeframe. In our code we use the stock of AMAZON(AMZN) we can easily modify the time frame depending on one's strategy*
```
#Initialize Financial Modelling Package
library(quantmod)

#Get Amazon stock data 
getSymbols(c("AMZN"))

#Visualize the stock over the past 3 months
chartSeries(AMZN, subset='last 3 months')

#Add a Volatility indicator, we chose Bollinger bands in this case
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)
```
*the technical chart can be found at [AMZN Chart.pdf](https://github.com/JKEVIN2010/JKEVIN2010.github.io/files/1254693/AMZN.Chart.pdf)*

#### 4- *The program "Stock_Chart_Manipulation" provides an advanced chart of the stock of the company Apple(AAPL) combined with multiple financial indicators including volatility indicators(Bollinger bands), simple moving average(SMA) as well as the overall momentum of the stock over the past three months*

```
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
addRSI()

#Visualize the momentum of the stock
addMomentum()

#More indicators could be added depending on the trader's strategy
```

*The chart can be found here [APPL Technical Chart.pdf](https://github.com/JKEVIN2010/JKEVIN2010.github.io/files/1254982/APPL.Technical.Chart.pdf)*

#### 5- *This is code contains my personal Financial Indicator as well as a sample trading strategy with portfolio allocation programmmed in R*
```
#This program is a personal financial Indicator
#Load the financial modelling packages

library(quantmod)
library(quantstrat)
library(TTR)

#Create Dates
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- "2015-12-31"

# Set the timezone
Sys.setenv(TZ = "UTC")

#Set the currency

currency("USD")

#Define a strategy
#Define trade size and initial equity

tradesize <- 1000000
initeq    <- 1000000

#Define name of strategy, portfolio and account

strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"

#Remove any existing strategy
rm.strat(strategy.st)

# Initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
strategy(strategy.st, store = TRUE)

# Create customised indicatior using the David Varadi Oscillator(DVO)
# The DVO attempts to find opportunities to buy a temporary dip and sell in a temporary uptrend. 
#In addition to obligatory market data, an oscillator function takes in two lookback periods.

# Delare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Add the DVO indicator to the strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out the indicator
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]

```




