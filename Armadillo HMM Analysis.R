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


# #filter_out burrow locs
# dat2<- filter(dat, dist != 0)

#add random noise to burrow locs
tmp<- which(dat$InBurrow == 1)
dat$y[tmp]<- dat$y[tmp] + runif(length(tmp), -0.5,0.5)
dat$x[tmp]<- dat$x[tmp] + runif(length(tmp), -0.5,0.5)


#prep data for use by `momentuHMM`
dat2<- dat %>% rename(ID = id)
dat2<- bayesmove::round_track_time(dat2, "ID", 300, 60)
dat2<- filter(dat2, dt == 300)
dat.prep<- prepData(dat2, type = "UTM", coordNames = c("x", "y"))


#run HMM

# Empty list for order selection
k.models<- list()

## K = 2
allm<- list()
niter<- 30
stateNames <- c("Burrow","Active")
whichzero <- which(dat.prep$step == 0)
propzero <- length(whichzero)/nrow(dat.prep)
zeromass0 <- c(propzero, 0)        #for zero distances by state

tic()
for (i in 1:niter) {
  print(paste("Iteration", i))
  
  # Step length mean
  stepMean0 <- runif(2,
                     min = c(0.01,10),
                     max = c(2,30))
  # Step length standard deviation
  stepSD0 <- runif(2,
                   min = c(0.01,10),
                   max = c(2,30))
  # Turning angle mean
  angleMean0 <- runif(2,
                      min = c(pi/2,0),
                      max = c(pi,pi/2))
  # Turning angle concentration
  angleCon0 <- runif(2,
                     min = c(0.6,0.1),
                     max = c(0.9,0.7))
  # Fit model
  if(propzero > 0) {  #don't include zero mass if no 0s present
    stepPar0 <- c(stepMean0, stepSD0, zeromass0)
  } else {
    stepPar0 <- c(stepMean0, stepSD0)
  }
  anglePar0 <- c(angleMean0, angleCon0)
  
  allm[[i]]<- fitHMM(data = dat.prep, nbStates = 2, 
                    Par0 = list(step = stepPar0, angle = anglePar0),
                    dist = list(step = "gamma", angle = "wrpcauchy"),
                    formula = ~ 1, stationary=TRUE, #stationary for a slightly better fit
                    estAngleMean = list(angle=TRUE),
                    stateNames = stateNames,
                    optMethod = "Nelder-Mead")
}
toc()

# Extract likelihoods of fitted models
allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))

# Index of best fitting model (smallest negative log-likelihood)
whichbest <- which.min(allnllk)

# Best fitting model
k.models[[1]] <- allm[[whichbest]]
k.models[[1]]
plot(k.models[[1]])






## K = 3
allm<- list()
niter<- 30
stateNames <- c("Burrow","Foraging","Transit")
whichzero <- which(dat.prep$step == 0)
propzero <- length(whichzero)/nrow(dat.prep)
zeromass0 <- c(propzero, 0, 0)        #for zero distances by state

tic()
for (i in 1:niter) {
  print(paste("Iteration", i))
  
  # Step length mean
  stepMean0 <- runif(3,
                     min = c(0.01, 3, 20),
                     max = c(2, 15, 40))
  # Step length standard deviation
  stepSD0 <- runif(3,
                   min = c(0.01, 2, 20),
                   max = c(2, 10, 40))
  # Turning angle mean
  angleMean0 <- runif(3,
                      min = c(3*pi/4, pi/2, 0),
                      max = c(pi, pi, pi/4))
  # Turning angle concentration
  angleCon0 <- runif(3,
                     min = c(0.5, 0.3, 0.6),
                     max = c(0.9, 0.7, 0.9))
  # Fit model
  if(propzero > 0) {  #don't include zero mass if no 0s present
    stepPar0 <- c(stepMean0, stepSD0, zeromass0)
  } else {
    stepPar0 <- c(stepMean0, stepSD0)
  }
  anglePar0 <- c(angleMean0, angleCon0)
  
  allm[[i]]<- fitHMM(data = dat.prep, nbStates = 3, 
                     Par0 = list(step = stepPar0, angle = anglePar0),
                     dist = list(step = "gamma", angle = "wrpcauchy"),
                     formula = ~ 1, stationary=TRUE, #stationary for a slightly better fit
                     estAngleMean = list(angle=TRUE),
                     stateNames = stateNames,
                     optMethod = "Nelder-Mead")
}
toc()

# Extract likelihoods of fitted models
allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))

# Index of best fitting model (smallest negative log-likelihood)
whichbest <- which.min(allnllk)

# Best fitting model
k.models[[2]] <- allm[[whichbest]]
k.models[[2]]
plot(k.models[[2]])




## K = 4

allm<- list()
niter<- 30
stateNames <- c("Burrow","Foraging","Transit","Dispersal")
whichzero <- which(dat.prep$step == 0)
propzero <- length(whichzero)/nrow(dat.prep)
zeromass0 <- c(propzero, 0, 0, 0)        #for zero distances by state

tic()
for (i in 1:niter) {
  print(paste("Iteration", i))
  
  # Step length mean
  stepMean0 <- runif(4,
                     min = c(0.01, 3, 20, 40),
                     max = c(2, 15, 40, 60))
  # Step length standard deviation
  stepSD0 <- runif(4,
                   min = c(0.01, 2, 20, 40),
                   max = c(2, 10, 40, 60))
  # Turning angle mean
  angleMean0 <- runif(4,
                      min = c(3*pi/4, pi/2, 0, 0),
                      max = c(pi, pi, pi/4, pi/8))
  # Turning angle concentration
  angleCon0 <- runif(4,
                     min = c(0.5, 0.3, 0.6, 0.6),
                     max = c(0.9, 0.7, 0.9, 0.9))
  # Fit model
  if(propzero > 0) {  #don't include zero mass if no 0s present
    stepPar0 <- c(stepMean0, stepSD0, zeromass0)
  } else {
    stepPar0 <- c(stepMean0, stepSD0)
  }
  anglePar0 <- c(angleMean0, angleCon0)
  
  allm[[i]]<- fitHMM(data = dat.prep, nbStates = 4, 
                     Par0 = list(step = stepPar0, angle = anglePar0),
                     dist = list(step = "gamma", angle = "wrpcauchy"),
                     formula = ~ 1, stationary=TRUE, #stationary for a slightly better fit
                     estAngleMean = list(angle=TRUE),
                     stateNames = stateNames,
                     optMethod = "Nelder-Mead")
}
toc()

# Extract likelihoods of fitted models
allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))

# Index of best fitting model (smallest negative log-likelihood)
whichbest <- which.min(allnllk)

# Best fitting model
k.models[[3]] <- allm[[whichbest]]
k.models[[3]]
plot(k.models[[3]])



### Compare models by AIC ###
k.2<- k.models[[1]]  # 2 states
k.3<- k.models[[2]]  # 3 states  
k.4<- k.models[[3]]  # 4 states

AIC(k.2, k.3, k.4)
AICweights(k.2, k.3, k.4)  #3 states is far and away the best model

## Look at pseudo-resids
plotPR(k.models[[1]])  #q-q plot way off on upper half
plotPR(k.models[[2]])  #q-q plot off on the upper tail
plotPR(k.models[[3]])  #q-q plot best of 3 models; close to line




### Extract PDF params and behavior estimates for further use

## get states
hmm.states<- viterbi(k.models[[2]])
dat.prep$state<- hmm.states


## get distribution params
hmm.params<- getPar0(k.models[[2]])$Par

#step length params
hmm.step.params<- hmm.params$step[1:6] %>% 
  t() %>% 
  data.frame()

#turning angle params
hmm.angle.params<- hmm.params$angle[1:6] %>% 
  t() %>% 
  data.frame()



## Save results
# write.csv(dat.prep, "Armadillo HMM Results.csv", row.names = F)
# write.csv(hmm.step.params, "HMM result step params_weird.csv", row.names = F)
# write.csv(hmm.angle.params, "HMM result angle params_weird.csv", row.names = F)
