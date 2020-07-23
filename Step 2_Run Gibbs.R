library(Rcpp)
library(mvtnorm)
library(dplyr)
library(ggplot2)
library(tictoc)
set.seed(1)

source('gibbs_resist.R')
source('gibbs_resist_func.R')
source('slice_b_gamma.R')
sourceCpp('resist_aux.cpp')

# dat<- read.csv('Armadillo Resistance Data.csv', as.is=T)
path.N<- read.csv('N Armadillo Resistance Data.csv', as.is=T)
path.S<- read.csv('S Armadillo Resistance Data.csv', as.is=T)

# #set to mean = 0 and sd = 1
# dat<- dat %>% 
#   mutate_at(c("dist2rd","elev","ndvi","lunar"), ~scale(., center = TRUE, scale = TRUE))

#analyze sites separately
path.N.scaled<- path.N %>% 
  mutate_at(c("dist2rd","ndvi","lunar"), ~scale(., center = TRUE, scale = TRUE))

path.S.scaled<- path.S %>% 
  mutate_at(c("dist2rd","ndvi","lunar"), ~scale(., center = TRUE, scale = TRUE))




#North Pantanal

ind=grep(paste(c("dist2rd","ndvi","lunar"), collapse="|"), names(path.N.scaled))
xmat=data.matrix(cbind(1, path.N.scaled[,ind]))

#check seg.id
seg.id=path.N.scaled$seg.id
k=unique(seg.id)
unique(k-c(1:max(k)))

#get y soma
tmp=unique(path.N.scaled[,c('seg.id','dt')])
ysoma<- tmp %>% 
  tidyr::drop_na() %>% 
  dplyr::pull(dt)
  
#model args
ngibbs=5000
nburn=ngibbs/2
w=0.1
MaxIter=100

#priors
gamma1=0.1
var.betas=c(100,rep(1,ncol(xmat)-1))

#2-state model
ngroup=2

tic()
res.N_2states<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id, ngroup = ngroup,
                     ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                     gamma1 = gamma1, w = w, MaxIter = MaxtIter)
toc()
# takes 19 min to run (for 2 behaviors, 5000 iter)


#3-state model
ngroup=3

tic()
res.N_3states<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id, ngroup = ngroup,
                             ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                             gamma1 = gamma1, w = w, MaxIter = MaxtIter)
toc()
# takes 25 min to run (for 3 behaviors, 5000 iter)




#South Pantanal

ind=grep(paste(c("dist2rd","ndvi","lunar"), collapse="|"), names(path.S.scaled))
xmat=data.matrix(cbind(1, path.S.scaled[,ind]))

#check seg.id
seg.id=path.S.scaled$seg.id
k=unique(seg.id)
unique(k-c(1:max(k)))

#get y soma
tmp=unique(path.S.scaled[,c('seg.id','dt')])
ysoma<- tmp %>% 
  tidyr::drop_na() %>% 
  dplyr::pull(dt)

#model args
ngibbs=5000
nburn=ngibbs/2
w=0.1
MaxIter=100

#priors
gamma1=0.1
var.betas=c(100,rep(1,ncol(xmat)-1))

#2-state model
ngroup=2

tic()
res.S_2states<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id, ngroup = ngroup,
                             ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                             gamma1 = gamma1, w = w, MaxIter = MaxtIter)
toc()
# takes ~7 min to run (for 2 behaviors, 5000 iter)


#3-state model
ngroup=3

tic()
res.S_3states<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id, ngroup = ngroup,
                             ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                             gamma1 = gamma1, w = w, MaxIter = MaxtIter)
toc()
# takes ~8.5 min to run (for 3 behaviors, 5000 iter)