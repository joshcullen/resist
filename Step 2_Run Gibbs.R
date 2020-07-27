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

ind<- grep(paste(c("dist2rd","ndvi","lunar"), collapse="|"), names(path.N.scaled))
xmat<- data.matrix(cbind(1, path.N.scaled[,ind]))

#check seg.id
seg.id<- path.N.scaled$seg.id
k<- unique(seg.id)
unique(k-c(1:max(k)))

#get y soma
tmp<- unique(path.N.scaled[,c('seg.id','dt')])
ysoma<- tmp %>% 
  tidyr::drop_na() %>% 
  dplyr::pull(dt)
  
#model args
ngibbs<- 1000
nburn<- ngibbs/2
w<- 0.1
MaxIter<- 100

#priors
var.betas<- c(100,rep(10,ncol(xmat)-1))


mod.res_N<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                     ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                     w = w, MaxIter = MaxtIter)
# takes 2 min to run (for 1000 iter)






#South Pantanal

ind<- grep(paste(c("dist2rd","ndvi","lunar"), collapse="|"), names(path.S.scaled))
xmat<- data.matrix(cbind(1, path.S.scaled[,ind]))

#check seg.id
seg.id<- path.S.scaled$seg.id
k<- unique(seg.id)
unique(k-c(1:max(k)))

#get y soma
tmp<- unique(path.S.scaled[,c('seg.id','dt')])
ysoma<- tmp %>% 
  tidyr::drop_na() %>% 
  dplyr::pull(dt)

#model args
ngibbs<- 1000
nburn<- ngibbs/2
w<- 0.1
MaxIter<- 100

#priors
var.betas<- c(100,rep(10,ncol(xmat)-1))


mod.res_S<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                         ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                         w = w, MaxIter = MaxtIter)
# takes 1 min to run (for 1000 iter)


