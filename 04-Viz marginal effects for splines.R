
library(tidyverse)
library(lubridate)
library(splines)
library(ggExtra)


###########################################
### Viz partial responses to covs by ID ###
###########################################

path<- read.csv("Emanuel Resistance Data.csv", as.is = T)
path$dt<- path$dt/60  #convert to min from sec
path$month<- month.abb[month(path$date)]
path$month<- factor(path$month, levels = month.abb[c(5:12,1)])

# Filter data for only steps with 6 >= dt >= 8 min
cond<- path[path$dt >= 6 & path$dt <= 8 & !is.na(path$dt), "seg.id"]
path<- path[path$seg.id %in% cond,]


## Load results from resistance model
store.betas<- read.csv("Giant Armadillo Resistance Results.csv", as.is = T)
store.betas<- store.betas[,-1]


## EVI
#Generate sequence along EVI
rango1<- range(path$evi)
seq.evi<- seq(rango1[1], rango1[2], length.out = 100)

#Run splines on standardized sequence
knot.locs<- seq(rango1[1], rango1[2], length.out = 4)[2:3]
spline.evi<- bs(seq.evi, degree=2, intercept = TRUE, knots = knot.locs)
spline.evi<- do.call(rbind, replicate(5, spline.evi, simplify=FALSE))

#Create design matrix
month1<- rep(unique(month(path$date)), each = 100)
month1<- factor(month1, levels = unique(month1))
month.dumm<- model.matrix(~month1 + 0)
design.mat<- cbind(month.dumm[,-1], spline.evi)


y.post<- matrix(NA, nrow = nrow(store.betas), ncol = nrow(design.mat))
for (i in 1:nrow(store.betas)) {
  y.post[i,]<- exp(design.mat %*% t(store.betas[i,]))
}

y.post.mcmc<- coda::as.mcmc(y.post)
betas<- as.data.frame(coda::HPDinterval(y.post.mcmc))
betas$mean<- colMeans(y.post.mcmc)
betas<- betas %>% 
  mutate(month = rep(unique(path$month), each = 100),
         evi = rep(seq.evi, 5),
         .before =  everything())

# identify and clip off top and bottom 1% of EVI sequence (x-axis)
clip.vals<- quantile(path$evi, c(0.01,0.99))
betas2<- betas %>% 
  filter(evi >= clip.vals[1] & evi <= clip.vals[2])


# Plot relationship
ggplot(data = betas2) +
  geom_ribbon(aes(x=evi, ymin=lower, ymax=upper, fill = month), alpha =  0.3) +
  scale_fill_viridis_d(guide = F) +
  geom_line(aes(evi, mean), size = 1) +
  geom_point(data = path, aes(x = evi, y = 1), alpha = 0) +
  geom_rug(data = path, aes(x = evi)) +
  xlim(clip.vals) +
  labs(x = "\nStandardized EVI", y = "Time Spent per Cell (min)\n") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold")) +
  facet_wrap(~month)
