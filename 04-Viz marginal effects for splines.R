library(tidyverse)
library(lubridate)
library(splines)


###########################################
### Viz partial responses to covs by ID ###
###########################################

path<- read.csv("Emanuel Resistance Data.csv", as.is = T)
path$dt<- path$dt/60  #convert to min from sec


# Filter data for only steps with 6 >= dt >= 8 min
cond<- path[path$dt >= 6 & path$dt <= 8 & !is.na(path$dt), "seg.id"]
path<- path[path$seg.id %in% cond,]



## EVI
#Generate sequence along green
rango1<- range(path$evi)
seq.evi<- seq(rango1[1], rango1[2], length.out = 100)

#Run splines on standardized sequence
knot.locs<- seq(rango1[1], rango1[2], length.out = 4)[2:3]
spline.evi<- bs(seq.evi, degree=2, intercept = FALSE, knots = knot.locs)

#Create design matrix where 0s added for all other non-spline vars
design.mat<- cbind(matrix(0, nrow = 100, ncol = 4), spline.evi)

store.betas.mcmc<- coda::as.mcmc(store.betas)
betas<- as.data.frame(coda::HPDinterval(store.betas.mcmc))
betas$mean<- colMeans(store.betas.mcmc)
  
y.mu<- exp(design.mat %*% betas$mean)
y.low<- exp(design.mat %*% betas$lower)
y.up<- exp(design.mat %*% betas$upper)
  
  
# Add results to data frame
y.mu.df<- data.frame(x = seq.evi,
                       y = y.mu,
                       ymin = y.low,
                       ymax = y.up
                       )


# Plot relationship
ggplot(data = y.mu.df) +
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), alpha =  0.3) +
  geom_line(aes(x, y), size = 1) +
  labs(x = "\nStandardized EVI", y = "Time Spent per Cell (min)\n") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))
