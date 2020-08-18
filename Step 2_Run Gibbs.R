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

# All armadillos
# dat<- read.csv('Armadillo Resistance Data.csv', as.is=T)

# N and S IDs separated
# path.N<- read.csv('N Armadillo Resistance Data.csv', as.is=T)
# path.S<- read.csv('S Armadillo Resistance Data.csv', as.is=T)

# N and S IDs separated and burrow locs removed
# path.N<- read.csv('N Armadillo Resistance Data_NoBurrow.csv', as.is=T)
# path.S<- read.csv('S Armadillo Resistance Data_NoBurrow.csv', as.is=T)

# N and S IDs separated for dispersal locs
path.N<- read.csv('N Armadillo Resistance Data_dispersal.csv', as.is=T)
path.S<- read.csv('S Armadillo Resistance Data_dispersal.csv', as.is=T)

# #set to mean = 0 and sd = 1
# dat<- dat %>% 
#   mutate_at(c("dist2rd","elev","ndvi","lunar"), ~scale(., center = TRUE, scale = TRUE))

#analyze sites separately
path.N.scaled<- path.N %>% 
  mutate_at(c("dist2rd","ndvi","lunar"), ~scale(., center = TRUE, scale = TRUE)) %>%
  mutate(dist_ndvi = dist2rd*ndvi)

path.S.scaled<- path.S %>% 
  mutate_at(c("dist2rd","ndvi","lunar"), ~scale(., center = TRUE, scale = TRUE)) %>%
  mutate(dist_ndvi = dist2rd*ndvi)
  



#North Pantanal

ind<- grep(paste(c("dist2rd","ndvi","lunar","dist_ndvi"), collapse="|"), names(path.N.scaled))
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


#W/o interaction term
mod.res_N1<- gibbs_resist(ysoma = ysoma, xmat = xmat[,1:4], seg.id = seg.id,
                     ngibbs = ngibbs, nburn = nburn, var.betas = var.betas[1:4],
                     w = w, MaxIter = MaxtIter)
# takes 10 s to run (for 1000 iter)


#W/ interaction term
mod.res_N2<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                          ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                          w = w, MaxIter = MaxtIter)
# takes 11 s to run (for 1000 iter)






#South Pantanal

ind<- grep(paste(c("dist2rd","ndvi","lunar","dist_ndvi"), collapse="|"), names(path.S.scaled))
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


#W/o interaction term
mod.res_S1<- gibbs_resist(ysoma = ysoma, xmat = xmat[,1:4], seg.id = seg.id,
                         ngibbs = ngibbs, nburn = nburn, var.betas = var.betas[1:4],
                         w = w, MaxIter = MaxtIter)
# takes 8 s to run (for 1000 iter)

#W/ interaction term
mod.res_S2<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                         ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                         w = w, MaxIter = MaxtIter)
# takes 9 s to run (for 1000 iter)




###################################################
### Check Posterior and Perform Model Selection ###
###################################################


### North Pantanal ###

#W/o interaction term
store.llk_N1<- mod.res_N1$llk
store.b_N1<- mod.res_N1$b.gamma
store.betas_N1<- mod.res_N1$betas

#look at overall convergence
plot(store.llk_N1, type='l')
nburn=500
abline(v=nburn, col='red')
plot(store.llk_N1[(nburn + 1):ngibbs], type='l')
acf(store.llk_N1[(nburn + 1):ngibbs])

plot(store.b_N1, type='l')
plot(store.b_N1[(nburn + 1):ngibbs], type='l')
acf(store.b_N1[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_N1<- ncol(mod.res_N1$betas)
for (i in 1:nbetas_N1){
  plot(mod.res_N1$betas[,i], type='l')  
}

for (i in 1:nbetas_N1){
  plot(mod.res_N1$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached




#W/ interaction term
store.llk_N2<- mod.res_N2$llk
store.b_N2<- mod.res_N2$b.gamma
store.betas_N2<- mod.res_N2$betas

#look at overall convergence
plot(store.llk_N2, type='l')
nburn=500
abline(v=nburn, col='red')
plot(store.llk_N2[(nburn + 1):ngibbs], type='l')
acf(store.llk_N2[(nburn + 1):ngibbs])

plot(store.b_N2, type='l')
plot(store.b_N2[(nburn + 1):ngibbs], type='l')
acf(store.b_N2[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_N2<- ncol(mod.res_N2$betas)
for (i in 1:nbetas_N2){
  plot(mod.res_N2$betas[,i], type='l')  
}

for (i in 1:nbetas_N2){
  plot(mod.res_N2$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached


AIC_mcmc = function(llk, npar) {
  (-2 * llk) + (2*npar) 
}

AIC_N_NoInt<- AIC_mcmc(llk = mean(store.llk_N1), npar = 5)
AIC_N_Int<- AIC_mcmc(llk = mean(store.llk_N2), npar = 6)

AIC_N_NoInt - AIC_N_Int  #model 1 (w/o interaction) is much better





### South Pantanal ###

#W/o interaction term
store.llk_S1<- mod.res_S1$llk
store.b_S1<- mod.res_S1$b.gamma
store.betas_S1<- mod.res_S1$betas

#look at overall convergence
plot(store.llk_S1, type='l')
nburn=500
abline(v=nburn, col='red')
plot(store.llk_S1[(nburn + 1):ngibbs], type='l')
acf(store.llk_S1[(nburn + 1):ngibbs])

plot(store.b_S1, type='l')
plot(store.b_S1[(nburn + 1):ngibbs], type='l')
acf(store.b_S1[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_S1<- ncol(mod.res_S1$betas)
for (i in 1:nbetas_S1){
  plot(mod.res_S1$betas[,i], type='l')  
}

for (i in 1:nbetas_S1){
  plot(mod.res_S1$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached




#W/ interaction term
store.llk_S2<- mod.res_S2$llk
store.b_S2<- mod.res_S2$b.gamma
store.betas_S2<- mod.res_S2$betas

#look at overall convergence
plot(store.llk_S2, type='l')
nburn=500
abline(v=nburn, col='red')
plot(store.llk_S2[(nburn + 1):ngibbs], type='l')
acf(store.llk_S2[(nburn + 1):ngibbs])

plot(store.b_S2, type='l')
plot(store.b_S2[(nburn + 1):ngibbs], type='l')
acf(store.b_S2[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_S2<- ncol(mod.res_S2$betas)
for (i in 1:nbetas_S2){
  plot(mod.res_S2$betas[,i], type='l')  
}

for (i in 1:nbetas_S2){
  plot(mod.res_S2$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached



AIC_S_NoInt<- AIC_mcmc(llk = mean(store.llk_S1), npar = 5)
AIC_S_Int<- AIC_mcmc(llk = mean(store.llk_S2), npar = 6)

AIC_S_NoInt - AIC_S_Int  #model 2 (w/ interaction) is much better






### FIND WAY TO EXPORT AND SAVE RESULTS TO USE IN ANALYSES AND DATA VIZ

# Export results

write.csv(, "N Armadillo Resistance Results_dispersal.csv", row.names = F)