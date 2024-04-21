library(pracma)
library(reshape)
library(ggplot2)
library(matrixStats)

sim <- function(asset_class, N, M, sigma) {
  rates <- c(3.86, 4.18, 4.38, 4.60, 4.78, 4.75, 4.39, 4.10, 3.80, 3.73, 3.61, 3.84, 3.57) / 100
  times <- c(0, 1/12, 1/6, 0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30)
  t.plot <- 1
  t <- seq(0, t.plot, length = N + 1)
  d <- N/t.plot
  y <- interp1(x = times, y = rates, xi = t, method = "spline")
  r.day <- (1 + y)^(1/d) - 1
  f0.day <- rep(NA, length(y))
  f0.day[1] <- r.day[1]
  
  for (i in 2:length(y)) {
    f0.day[i] <- (1 + r.day[i])^(i)/(1 + r.day[i-1])^(i-1) - 1
  }
  f0 <- (1 + f0.day)^d - 1
  r <- matrix(NA, ncol = N + 1, nrow = M)
  r[,1] <- f0[1]
  
  for (i in 1:N) {
    Z <- rnorm(M)
    r[,i+1] <- r[,i] + 
      (f0[i+1] - f0[i]) +
      sigma*sigma/2*(t[i+1]^2 - t[i]^2) + sigma*sqrt(t[i+1] - t[i])*Z
  }
  return(r)
}

bond_pricing <- function(asset_class, face_value, coupon_rate, ytm, maturity) {
  freq <- 2
  periods <- maturity * freq
  coupon <- face_value * coupon_rate / freq
  pv_coupons <- sum(coupon / (1 + ytm/freq)^(1:periods))
  pv_face <- face_value / (1 + ytm/freq)^periods
  price <- pv_coupons + pv_face
  return(price)
}
portfolio <- data.frame(
  asset_class = c("UST", "Agency", "MBS", "CMO", "CMBS"),
  weight = c(0.6, 0.05, 0.2, 0.1, 0.05)
)

N <- 12
M <- 1000
sigma <- 0.01
scenarios <- lapply(portfolio$asset_class, sim, N = N, M = M, sigma = sigma)
names(scenarios) <- portfolio$asset_class
portfolio_values <- matrix(0, ncol = N + 1, nrow = M)

for (m in 1:M) {
  for (n in 1:(N + 1)) {
    prices <- sapply(1:nrow(portfolio), function(i) {
      bond_pricing(
        portfolio$asset_class[i],
        face_value = 100,
        coupon_rate = 0.05,
        ytm = scenarios[[portfolio$asset_class[i]]][m, n],
        maturity = 10
      )
    })
    portfolio_values[m, n] <- sum(prices * portfolio$weight)
  }
}

var_95 <- rowQuantiles(portfolio_values, probs = 0.05)
es_95 <- rowMeans(portfolio_values * (portfolio_values < var_95))

plot(colMeans(portfolio_values), type = 'l', xlab = "Month", ylab = "Mean Portfolio Value", 
     main = "Expected Portfolio Value", col = "blue")
lines(var_95, col = "red", lty = 2)
lines(es_95, col = "red", lty = 3)

legend("topright", 
       c("Mean", "95% VaR", "95% ES"),
       col = c("blue", "red", "red"), 
       lty = c(1, 2, 3))
