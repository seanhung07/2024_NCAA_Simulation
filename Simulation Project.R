rm(list=ls())

library("pracma")
library("reshape")
library("ggplot2")

t.plot <- 1
nStep <- 252
t <- seq(0,t.plot,length=nStep+1)
d <- nStep/t.plot

rates <- c(3.86,4.18,4.38,4.60,4.78,4.75,4.39,4.10,3.80,3.73,3.61,3.84,3.57)/100
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

my.duration <- function(t,c,f,ytm=NA,P=NA){

    pv <- ((c*f)/ytm)*(1-(1/(1+ytm)^t))+(f/(1+ytm)^t) #calc present value of each bond
    
    df <- as.data.frame(matrix(NA,ncol=4,nrow=t)) #create empty df
    df[,1] <- rep(c*f) #coupon pmts in col-1
    df[t,1] <- f+(c*f) #principal repmt at final t
    
    #discount each coupon + principal repmt col-2
    for (i in 1:t){
        df[i,2] <- df[i,1]/(1+ytm)^(i)
    }
    
    #weight each pmt in pv terms col-3
    for (i in 1:t){
        df[i,3] <- df[i,2]/pv
    }
    
    #weight*period col-4
    for (i in 1:t){
        df[i,4] <- df[i,3]*i
    }
    
    #macaulay duration = sum of 4th column 
    mac.duration <- sum(df[,4])
    
    #modified duration
    duration <- mac.duration/(1+ytm) 
    return(duration)
}





