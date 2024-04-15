rm(list=ls())

library("pracma")
library("reshape")
library("ggplot2")

t.plot <- 1
nStep <- 252
t <- seq(0,t.plot,length=nStep+1)
d <- nStep/t.plot

rates <- c(5.48,5.50,5.45,5.42,5.36,5.13,4.88,4.70,4.54,4.53,4.50,4.73,4.61)/100
times <- c(0,1/12,1/6,0.25,0.5,1,2,3,5,7,10,20,30)

#interpolate
y <- interp1(x=times, y=rates, xi = t, method = "spline")
plot(y=y,x=t,type="l",main="Yield Curve")

r.day <- (1 + y)^(1/d) - 1

f0.day <- rep(NA,length(y))
f0.day[1] <- r.day[1]

for (i in 2:length(y)){
    f0.day[i] <- (1 + r.day[i])^(i)/(1 + r.day[i-1])^(i-1) - 1
}

#convert forward rates to annual rates
f0 <- (1 + f0.day)^d - 1
plot(y=f0,x=t,type="l",main="Forward Rates")

#simulation
M <- 100
N <- nStep
r <- matrix(NA,ncol=N+1,nrow=M)
r[,1] <- f0[1]
sigma <- 0.001 # volatility of interest rates

for (i in 1:N){
    Z <- rnorm(M)
    r[,i+1] <- r[,i] +
        (f0[i+1] - f0[i]) +
        sigma*sigma/2*(t[i+1]^2 - t[i]^2) +
        sigma*sqrt(t[i+1] - t[i])*Z
}

rates <- as.data.frame(t(r))
rates$t <- t
names(rates)

df.plot <- melt(rates,id.vars = "t")

p <- ggplot(data = df.plot, aes(x = t, y = value, color = variable)) +
    geom_line() + guides(fill=FALSE, color=FALSE)

p <- p + labs(x = "Time", y ="r(t)",title="Simulated Ho-Lee Model")
print(p)







