library(Rcpp)
library(mvtnorm)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(tictoc)
library(splines)

source('gibbs_resist.R')
source('gibbs_resist_func.R')
source('slice_b_gamma.R')
source('slice_betas.R')
sourceCpp('resist_aux.cpp')


#############################
### Import armadillo data ###
#############################

path<- read.csv("Emanuel Resistance Data.csv", as.is = T)
path$dt<- path$dt/60  #convert to min from sec


# Filter data for only steps with 6 >= dt >= 8 min
cond<- path[path$dt >= 6 & path$dt <= 8 & !is.na(path$dt), "seg.id"]
path<- path[path$seg.id %in% cond,]



### Reformat Data ###

# Center and Scale covariate
# path.s<- path %>% 
#   mutate_at(c("ndvi","awei"),
#             ~scale(., center = TRUE, scale = TRUE)) %>%
#   drop_na(ndvi)
path.s<- path
path.s$month<- month.abb[month(path.s$date)]
path.s$month<- factor(path.s$month, levels = month.abb[c(5:12,1)])


# Add B-spline (w/ 2 internal knots) for 'EVI'
rango<- range(path.s$evi)
knot.locs<- seq(rango[1], rango[2], length.out = 4)[2:3]
spline.evi<- as.data.frame(bs(path.s$evi, degree=2, intercept = TRUE,
                                   knots = knot.locs))
names(spline.evi)<- paste("spline", 1:ncol(spline.evi), sep = ".")
path.s<- cbind(path.s, spline.evi)


ind<- c(paste("spline", 1:ncol(spline.evi), sep = "."))


month.dumm<- model.matrix(~path.s$month + 0)
month.dumm<- month.dumm[,which(colSums(month.dumm) > 0)]  #remove months w/o observations

xmat<- data.matrix(cbind(month.dumm[,-1], path.s[,ind]))  #treat May as ref
colnames(xmat)[1:4]<- c("Jun","Sep","Oct","Nov")

#reformat seg.id so it is consecutive and numeric
path.s$seg.id<- factor(path.s$seg.id) 
levels(path.s$seg.id)<- 1:length(unique(path.s$seg.id))
path.s$seg.id<- as.numeric(path.s$seg.id)

#check seg.id
seg.id<- path.s$seg.id
k<- unique(seg.id)
unique(k-c(1:max(k))) #should be 0 

#get y soma
cond=!is.na(path.s$dt)
ysoma=path.s[cond,'dt']



#################
### Run Model ###
#################

#model args
ngibbs=10000
nburn=ngibbs/2
w=0.1
MaxIter=10000

#priors
var.betas=rep(10,ncol(xmat)) 


#Run model
set.seed(123)
mod<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                            ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                            w = w, MaxIter = MaxIter)
# takes 6 min to run (for 10000 iter)




###################################################
### Check Posterior and Perform Model Selection ###
###################################################

#store results
store.llk<- mod$llk
store.b<- mod$b.gamma
store.betas<- mod$betas

#look at overall convergence
plot(store.llk, type='l')
plot(store.llk[(nburn + 1):ngibbs], type='l')
acf(store.llk[(nburn + 1):ngibbs])

plot(store.b, type='l')
plot(store.b[(nburn + 1):ngibbs], type='l')
acf(store.b[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas<- ncol(mod$betas)
for (i in 1:nbetas){
  plot(mod$betas[,i], type='l')  
}

for (i in 1:nbetas){
  plot(mod$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached



############################
### Export model results ###
############################
res<- cbind(store.b, store.betas)
res<- data.frame(res) %>% 
  slice((nburn + 1):ngibbs)
names(res)<- c("gamma.b", colnames(xmat))

# write.csv(res, "Giant Armadillo Resistance Results.csv", row.names = F)
