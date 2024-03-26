rm(list=ls())

library(ggplot2)
library(reshape2)
tournament_sim <- function(cond_win){
  regions <- c("East","South","MidWest","West")
  full_bracket <- array(NA,dim=c(16,6,4))
  region_winners <- matrix(NA,nrow=4,ncol=7)
  for (i in 1:4){
    ret <- region_sim(cond_win)
    full_bracket[,,i] <- ret$qual
    region_winners[i,] <- ret$winner
  }
  tmp <- cbind(matrix(seq(1:4),ncol=1),region_winners)
  for (i in 1:2){
    tmp1 <- matrix(NA,nrow=nrow(tmp)/2,ncol=ncol(tmp))
    for(j in 1:(nrow(tmp)/2)){
      seed1 <- tmp[j,i+6]
      seed2 <- tmp[nrow(tmp)+1-j,i+6]
      total <- seed1 + seed2
      val <- runif(1,0,1)
      if (((seed1/total) >= val) | (total == seed1)){
        tmp1[j,] <- tmp[j,]
        full_bracket[tmp[j,2],i+4,tmp1[j,1]] <- 1
      }
      else{
        tmp1[j,] <- tmp[nrow(tmp)+1-j,]
        full_bracket[tmp[nrow(tmp)+1-j,2],i+4,tmp1[j,1]] <- 1
      }
    }
    tmp <- tmp1
  }
  sprintf("Winner is seed %d from %s region",tmp[1,2],regions[tmp[1,1]])
  out <- list("brackets" = full_bracket, "winner" = tmp)
  return(out)
}

region_sim <- function(cond_win){
  qual <- matrix(0,nrow=nrow(cond_win),ncol=6)
  tmp <- cbind(matrix(seq(1,nrow(cond_win)),ncol=1),cond_win)
  #simulate each round
  for (i in 1:4){
    tmp1 <- matrix(NA,nrow=nrow(tmp)/2,ncol=ncol(tmp))
    for (j in 1:(nrow(tmp)/2)){
      seed1 <- tmp[j,i+1]
      seed2 <- tmp[nrow(tmp)+1-j,i+1]
      total <- seed1 + seed2
      val <- runif(1,0,1)
      if (((seed1/total) >= val) | (total == seed1)){
        tmp1[j,] <- tmp[j,]
        qual[tmp[j,1],i] <- 1
      }
      else{
        tmp1[j,] <- tmp[nrow(tmp)+1-j,]
        qual[tmp[nrow(tmp)+1-j,1],i] <- 1
      }
    }
    tmp <- tmp1
  }
  out <- list("qual" = qual, "winner" = tmp)
  return(out)
}

get_brackets <- function(brackets){
  regions <- c("East","South","MidWest","West")
  for (i in 1:4){
    sprintf("%s Bracket",regions[i])
    print(brackets[,,i])
  }
}

win_rate <- matrix(c(.987,.842,.664,.395,.243,.158,
                     .928,.625,.447,.211,.086,.033,
                     .855,.526,.257,.112,.072,.026,
                     .789,.474,.151,.092,.026,.013,
                     .651,.342,.079,.059,.026,.000,
                     .618,.289,.105,.020,.013,.007,
                     .612,.191,.066,.020,.007,.007,
                     .487,.105,.059,.039,.026,.007,
                     .513,.053,.033,.013,.000,.000,
                     .388,.158,.059,.007,.000,.000,
                     .382,.171,.059,.033,.000,.000,
                     .349,.145,.013,.000,.000,.000,
                     .211,.039,.000,.000,.000,.000,
                     .145,.013,.000,.000,.000,.000,
                     .072,.026,.007,.000,.000,.000,
                     .013,.000,.000,.000,.000,.000),
                   byrow=T,ncol=6)

condition_win <- matrix(c(.8531,.7886,.5949,.6152,.6502,
                          .6735,.7152,.4720,.4076,.3837,
                          .6152,.4886,.4358,.6429,.3611,
                          .6008,.3186,.6093,.2826,.5000,
                          .5253,.2310,.7468,.4407,.0000,
                          .4676,.3633,.1905,.6500,.5385,
                          .3121,.3455,.3030,.3500,1.000,
                          .2156,.5619,.6610,.6667,.2692,
                          .1033,.6226,.3939,.0000,.0000,
                          .4072,.3734,.1186,.0000,.0000,
                          .4476,.3450,.5593,.0000,.0000,
                          .4155,.0897,.0000,.0000,.0000,
                          .1848,.0000,.0000,.0000,.0000,
                          .0897,.0000,.0000,.0000,.0000,
                          .3611,.2692,.0000,.0000,.0000,
                          .0000,.0000,.0000,.0000,.0000),
                        byrow=T,ncol=5)

cond_win <- cbind(win_rate[,1],condition_win)

regions <- c("East","South","MidWest","West")

N <- 10000
topseed_second <- 0
topseed_third <- 0
for (i in 1:N){
  outcome <- tournament_sim(cond_win)
  brackets <- outcome$brackets
  winner <- outcome$winner
  #sprintf("Winner is seed %d from %s region",winner[1,2],regions[winner[1,1]])
  if (sum(brackets[1:8,1,]) == 32){
    topseed_second <- topseed_second + 1
  }
  if (sum(brackets[1:4,2,]) == 16){
    topseed_third <- topseed_third + 1
  }
}

#Question #2
finalfour <- function(a,b,c,d){
  prob <- win_rate[a,4]*win_rate[b,4]*win_rate[c, 4]*win_rate[d,4]
  return(prob)
}

tmp1 <- finalfour(1,1,1,1)
sprintf("Likelihood of five #1 seeds reaching final four %f",tmp1)

tmp2 <- finalfour(3,3,2,1)
sprintf("Likelihood of #1, #2, #3, #3 seeds reaching final four %f",tmp2)

#Question 3
prob2 <- topseed_second/N
prob3 <- topseed_third/N
sprintf("Likelihood of top seeds advancing second round %f",prob2)

sprintf("Likelihood of top seeds advancing third round %f",prob3)



#Question 1
get_detailed_round_data <- function(brackets) {
  round_advanced_data <- array(0, dim = c(16, 6, 4))
  for (r in 1:4) {
    for (s in 1:16) {
      for (round in 1:6) {
        if (brackets[s, round, r] == 1) {
          round_advanced_data[s, round, r] <- 1
        }
      }
    }
  }
  return(round_advanced_data)
}

round_data_collection <- array(0, dim = c(16, 6, 4))
for (i in 1:N) {
  outcome <- tournament_sim(cond_win)
  brackets <- outcome$brackets
  detailed_round_data <- get_detailed_round_data(brackets)
  round_data_collection <- round_data_collection + detailed_round_data
}

round_data <- melt(round_data_collection, varnames = c("Seed", "Round", "Region"))
round_data$Seed <- as.integer(round_data$Seed)

round_data$Seed <- as.factor(round_data$Seed)
round_data$Round <- as.factor(round_data$Round)

g <- ggplot(round_data, aes(x = Seed, y = value, fill = Seed)) +
  geom_bar(stat = "identity", fill= "gray") +
  facet_wrap(~ Round, scales = "free_y") +
  labs(title = "Distribution of Seeds by Round",
       x = "Seed",
       y = "Frequency") +
  theme_minimal()

print(g)
