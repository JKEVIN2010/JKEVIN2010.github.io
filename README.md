
# Algorithmic Finance personal Projects (Coded in R)
## This repository contains  my progress in Algorithmic Finance. 
## The following programs can be used by individual investors and analysts to easily visualize and analize a variety of stocks using the powerful programming language R


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




