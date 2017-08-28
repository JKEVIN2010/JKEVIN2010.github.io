
# Algorithmic Finance personal Projects (Coded in R)
## This repository contains  my progress in Algorithmic Finance. 
## The following programs can be used by individual investors and analysts to easily visualize and analyze a variety of stocks using the powerful programming language R

### TABLE OF CONTENTS:

### 1- Options Pricing model using Blackscholes
### 2- Technical Stock visualization 
### 3- Advanced Stock Visualization
### 4- Customised Financial Indicator
### 5- Prediction of a stock price using Times Series Models

#### 1- *The program "Blackscholes" is a simple code that computes the fair price of an option using the well-known Black-Scholes-Merton Formula*
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
#### 2- *The  program "Stock_Visualize" simply plots a technical chart of any stock in within a specific timeframe. In our code we use the stock of AMAZON(AMZN) we can easily modify the time frame depending on one's strategy*
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

#### 3- *The program "Stock_Chart_Manipulation" provides an advanced chart of the stock of the company Apple(AAPL) combined with multiple financial indicators including volatility indicators(Bollinger bands), simple moving average(SMA) as well as the overall momentum of the stock over the past three months*

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

#### 4- *This is code contains my personal Financial Indicator as well as a sample trading strategy with portfolio allocation programmmed in R*
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

#### 5- *This program predicts the price of any given stock using the ARIMA model(Time Series Analysis) we used the stock of google in this example and dowloaded the prices data from https://finance.yahoo.com/quote/GOOG/history?period1=1376884800&period2=1461556800&interval=1mo&filter=history&frequency=1mo*

```
library(forecast)

#Stock Price prediction
#We are using Google for this example
#Import the csv file from Yahoo Finance
goog <- read.csv("GOOG.csv", header = TRUE)

#Visualize the data
head(goog)

#Convert the closing column(5th column) into a time series and use freq = 12 months
goog.ts <- ts(rev(goog[,5]), start = c(2013, 8), freq = 12)

#Take the log of the closing prices and store in a dataframe
google <- data.frame(closing = goog.ts, lclosing = log(goog.ts))

#Plot the data frame 
plot(google$closing, main="Google stock prices(GOOLE)",lwd = 2,sub = "August 2013 to
April 2016", ylab = "Closing Prices")

#Decompose Google time series into a seasonal, trend and residual components usingl st()

google.stl <- stl(google$closing, s.window = "periodic")


#Arima model with h = 24 for a 2 year prediction with 95% confidence level
google.f <- forecast(google.stl, method = "arima", h = 24, level = 95)

#Plot the forecast

plot(google.f, ylab = "stock price", xlab = "year", sub = "Forecast from april 2016 to april 2018" )


```
*The plot of the forecast of the stock price of google in the next 2 years can be found here [Stock Price Prediction.pdf](https://github.com/JKEVIN2010/JKEVIN2010.github.io/files/1255167/Stock.Price.Prediction.pdf)*





