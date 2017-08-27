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


