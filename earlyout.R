# Example 
library(dplyr)
library(gridExtra)
library(reshape2)
library(ggplot2)

source('TaxFunctions.R')

file <- "lifetable.txt" # File to use
dat <- tbl_df(read.csv(file, sep=" ", stringsAsFactors=FALSE))

high3 <- 119776
yos <- 25
age_leave <- 48
pens_age <- 48
eo <- TRUE
inflation <- 0.02

be_val_m <- function(high3,yos,pens_age,eo,inflation){
    pens <- pension(high3,high3,high3,pens_age,yos,pens_age,eo,0)
    le <- dat %>% filter(age==pens_age)
    return(pens*le$life_expectancy_m)
}
be_val_f <- function(high3,yos,pens_age,eo,inflation){
    pens <- pension(high3,high3,high3,pens_age,yos,pens_age,eo,0)
    le <- dat %>% filter(age==pens_age)
    return(pens*le$life_expectancy_f)
}

age <- numeric(119-pens_age)
be_m <- numeric(119-pens_age)
be_f <- numeric(119-pens_age)

for(i in 0:(119-pens_age)){
    age[i+1] <- pens_age + i
    be_m[i+1] <- be_val_m(high3,yos+i,pens_age+i,eo,0)
    be_f[i+1] <- be_val_f(high3,yos+i,pens_age+i,eo,0)    
}

surv_m <- 1 - dat$death_prob_m
surv_f <- 1 - dat$death_prob_f
survive_m <- numeric(119-48)
survive_f <- numeric(119-48)
age2 <- numeric(119-48)
age2[1] <- 48
survive_m[1] <- 1
survive_f[1] <- 1

pens_now <- pension(high3,high3,high3,pens_age,yos,pens_age,eo,0)
delta <- numeric(119-57)
for(i in 49:120){
    age2[i-47] <- i
    if(i>57){
        delta[i-48] <- pens_now/(1+inflation)^(9)-
            pension(high3,high3,high3,pens_age+9,yos+9,pens_age+9,
                    eo,0)
    }else{
        delta[i-48] <- pens_now/((1+inflation)^(i-49))+9000
    }
    survive_m[i-47] <- survive_m[i-48]*surv_m[i]
    survive_f[i-47] <- survive_f[i-48]*surv_f[i]
}
sum(delta*survive_m[2:73])
sum(delta*survive_f[2:73])

for(i in 49:120){
    age2[i-47] <- i
    if(i>62){
        delta[i-48] <- pens_now/(1+inflation)^(9)-
            pension(high3,high3,high3,pens_age+14,yos+14,pens_age+14,
                    eo,0)
    }else if(i>57){
        delta[i-48] <- pens_now/(1+inflation)^(9) + 9000
    }else{        
        delta[i-48] <- pens_now/((1+inflation)^(i-49))+9000
    }
    survive_m[i-47] <- survive_m[i-48]*surv_m[i]
    survive_f[i-47] <- survive_f[i-48]*surv_f[i]
}
sum(delta*survive_m[2:73])
sum(delta*survive_f[2:73])