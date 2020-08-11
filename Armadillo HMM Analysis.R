### Estimate armadillo behavior

set.seed(1)

library(tidyverse)
library(momentuHMM)
library(lubridate)
library(tictoc)


setwd("~/Documents/Snail Kite Project/Data/armadillos")

dat<- read.csv("Modified Armadillo Data.csv", header = T, sep = ",")
dat$id<- as.character(dat$id)
dat$date<- as_datetime(dat$date)


#add noise to burrow locs
tmp<- which(dat$InBurrow == 1)
dat$x[tmp]<- dat$x[tmp] + runif(length(tmp), -0.5,0.5)
dat$y[tmp]<- dat$y[tmp] + runif(length(tmp), -0.5,0.5)


#prep data for use by `momentuHMM`
dat<- dat %>% rename(ID = id)
dat.prep<- prepData(dat, type = "UTM", coordNames = c("x", "y"))
dat.prep<-  dat.prep[dat.prep$dt == 300,]


#run HMM

## K = 2

stateNames <- c("Burrow","Active")
whichzero <- which(dat.prep$step == 0)
propzero <- length(whichzero)/nrow(dat.prep)
zeromass0 <- c(propzero, 0)        #for zero distances by state

# Step length mean
stepMean0 <- c(0.25, 25)
# Step length standard deviation
stepSD0 <- c(0.1, 25)
# # Step length shape
# stepShape0<- c(1, 1)
# # Step length scale
# stepScale0<- c(0.5, 10)
# # Turning angle mean
angleMean0 <- c(pi, 0)
# Turning angle concentration
angleCon0 <- c(0.8, 0.8)
# Fit model
if(propzero > 0) {  #don't include zero mass if no 0s present
  stepPar0 <- c(stepMean0, stepSD0, zeromass0)
} else {
  stepPar0 <- c(stepMean0, stepSD0)
}
anglePar0 <- c(angleMean0, angleCon0)

tic()
mod.res2<- fitHMM(data = dat.prep, nbStates = 2, 
                 Par0 = list(step = stepPar0, angle = anglePar0),
                 dist = list(step = "gamma", angle = "wrpcauchy"),
                 formula = ~ 1, stationary=TRUE, #stationary for a slightly better fit
                 estAngleMean = list(angle=TRUE),
                 stateNames = stateNames,
                 optMethod = "Nelder-Mead",
                 retryFits = 5)
toc()
#2-state model takes 22 min to run using 30 different starting values

mod.res2
plot(mod.res2)
#Receive "Error in aInd[zoo]:(aInd[zoo + 1] - 1) : NA/NaN argument" error when using
#plot(mod.res2)



## K = 3

stateNames <- c("Burrow","Foraging", "Transit")
whichzero <- which(dat.prep$step == 0)
propzero <- length(whichzero)/nrow(dat.prep)
zeromass0 <- c(propzero, 0, 0)        #for zero distances by state

# Step length mean
stepMean0 <- c(0.25, 5, 25)
# Step length standard deviation
stepSD0 <- c(0.1, 5, 25)
# Turning angle mean
angleMean0 <- c(pi, pi, 0)
# Turning angle concentration
angleCon0 <- c(0.8, 0.2, 0.8)
# Fit model
if(propzero > 0) {  #don't include zero mass if no 0s present
  stepPar0 <- c(stepMean0, stepSD0, zeromass0)
} else {
  stepPar0 <- c(stepMean0, stepSD0)
}
anglePar0 <- c(angleMean0, angleCon0)

tic()
mod.res3<- fitHMM(data = dat.prep, nbStates = 3, 
                  Par0 = list(step = stepPar0, angle = anglePar0),
                  dist = list(step = "gamma", angle = "wrpcauchy"),
                  formula = ~ 1, stationary=TRUE, #stationary for a slightly better fit
                  estAngleMean = list(angle=TRUE),
                  stateNames = stateNames,
                  optMethod = "Nelder-Mead",
                  retryFits = 30)
toc()
#3-state model takes 74 min to run using 30 different starting values

mod.res3
plot(mod.res3)
