library(Rcpp)
library(mvtnorm)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tictoc)

source('gibbs_resist.R')
source('gibbs_resist_func.R')
source('slice_b_gamma.R')
source('slice_betas.R')
sourceCpp('resist_aux.cpp')


# N and S IDs separated
path.N<- read.csv('N Armadillo Resistance Data.csv', as.is=T)
path.S<- read.csv('S Armadillo Resistance Data.csv', as.is=T)

path.N$dt<- path.N$dt/60  #convert to min from sec
path.S$dt<- path.S$dt/60


# Filter data for only steps with 3 >= dt >= 7 min
cond.N<- path.N[path.N$dt >= 3 & path.N$dt <= 7 & !is.na(path.N$dt), "seg.id"]
path.N<- path.N[path.N$seg.id %in% cond.N,]

cond.S<- path.S[path.S$dt >= 3 & path.S$dt <= 7 & !is.na(path.S$dt), "seg.id"]
path.S<- path.S[path.S$seg.id %in% cond.S,]


# Filter data by behavior (foraging or transit)
path.N.forage<- path.N %>% 
  filter(state == "Foraging")
path.N.transit<- path.N %>% 
  filter(state == "Transit")

path.S.forage<- path.S %>% 
  filter(state == "Foraging")
path.S.transit<- path.S %>% 
  filter(state == "Transit")


#Center and Scale covariates 
path.N.forage<- path.N.forage %>% 
  mutate_at(c("dist2rd","slope","ndvi","lunar","t.ar","rain"),
            ~scale(., center = TRUE, scale = TRUE)) %>% 
  drop_na(t.ar)
path.N.transit<- path.N.transit %>% 
  mutate_at(c("dist2rd","slope","ndvi","lunar","t.ar","rain"),
            ~scale(., center = TRUE, scale = TRUE)) %>% 
  drop_na(t.ar)

path.S.forage<- path.S.forage %>% 
  mutate_at(c("dist2rd","slope","ndvi","lunar","t.ar","rain"),
            ~scale(., center = TRUE, scale = TRUE)) %>% 
  drop_na(t.ar)
path.S.transit<- path.S.transit %>% 
  mutate_at(c("dist2rd","slope","ndvi","lunar","t.ar","rain"),
            ~scale(., center = TRUE, scale = TRUE)) %>% 
  drop_na(t.ar)





######################
### North Pantanal ###
######################

## Foraging

ind<- c("dist2rd","slope","ndvi","t.ar","rain")
xmat<- data.matrix(cbind(1, path.N.forage[,ind]))

#reformat seg.id so it is consecutive and numeric
path.N.forage$seg.id<- factor(path.N.forage$seg.id) 
levels(path.N.forage$seg.id)<- 1:length(unique(path.N.forage$seg.id))
path.N.forage$seg.id<- as.numeric(path.N.forage$seg.id)

#check seg.id
seg.id<- path.N.forage$seg.id
k<- unique(seg.id)
unique(k-c(1:max(k))) #should be 0 

#get y soma
cond=!is.na(path.N.forage$dt)
ysoma=path.N.forage[cond,'dt']
  
#model args
ngibbs=1000
nburn=ngibbs/2
w=0.1
MaxIter=10000

#priors
var.betas=rep(10,ncol(xmat)) #changed


#Run model
set.seed(2)
mod.forage_N<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                     ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                     w = w, MaxIter = MaxIter)
# takes 2.5 min to run (for 1000 iter)





## Transit

ind<- c("dist2rd","slope","ndvi","t.ar","rain")
xmat<- data.matrix(cbind(1, path.N.transit[,ind]))

#reformat seg.id so it is consecutive and numeric
path.N.transit$seg.id<- factor(path.N.transit$seg.id) 
levels(path.N.transit$seg.id)<- 1:length(unique(path.N.transit$seg.id))
path.N.transit$seg.id<- as.numeric(path.N.transit$seg.id)

#check seg.id
seg.id<- path.N.transit$seg.id
k<- unique(seg.id)
unique(k-c(1:max(k))) #should be 0 

#get y soma
cond=!is.na(path.N.transit$dt)
ysoma=path.N.transit[cond,'dt']

#model args
ngibbs=1000
nburn=ngibbs/2
w=0.1
MaxIter=10000

#priors
var.betas=rep(10,ncol(xmat)) #changed


#Run model
set.seed(2)
mod.transit_N<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                             ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                             w = w, MaxIter = MaxIter)
# takes 48 s to run (for 1000 iter)









######################
### South Pantanal ###
######################

ind<- grep(paste(c("dist2rd","slope","ndvi","t.ar","rain"), collapse="|"),
           names(path.S.forage))
xmat<- data.matrix(cbind(1, path.S.forage[,ind]))

#reformat seg.id so it is consecutive and numeric
path.S.forage$seg.id<- factor(path.S.forage$seg.id) 
levels(path.S.forage$seg.id)<- 1:length(unique(path.S.forage$seg.id))
path.S.forage$seg.id<- as.numeric(path.S.forage$seg.id)

#check seg.id
seg.id<- path.S.forage$seg.id
k<- unique(seg.id)
unique(k-c(1:max(k))) #should be 0 

#get y soma
cond=!is.na(path.S.forage$dt)
ysoma=path.S.forage[cond,'dt']

#model args
ngibbs=1000
nburn=ngibbs/2
w=0.1
MaxIter=10000

#priors
var.betas=rep(10,ncol(xmat)) #changed


#Run model
set.seed(2)
mod.forage_S<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                             ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                             w = w, MaxIter = MaxIter)
# takes 1 min to run (for 1000 iter)






## Transit

ind<- grep(paste(c("dist2rd","slope","ndvi","t.ar","rain"), collapse="|"),
           names(path.S.transit))
xmat<- data.matrix(cbind(1, path.S.transit[,ind]))

#reformat seg.id so it is consecutive and numeric
path.S.transit$seg.id<- factor(path.S.transit$seg.id) 
levels(path.S.transit$seg.id)<- 1:length(unique(path.S.transit$seg.id))
path.S.transit$seg.id<- as.numeric(path.S.transit$seg.id)

#check seg.id
seg.id<- path.S.transit$seg.id
k<- unique(seg.id)
unique(k-c(1:max(k))) #should be 0 

#get y soma
cond=!is.na(path.S.transit$dt)
ysoma=path.S.transit[cond,'dt']

#model args
ngibbs=1000
nburn=ngibbs/2
w=0.1
MaxIter=10000

#priors
var.betas=rep(10,ncol(xmat)) #changed


#Run model
set.seed(2)
mod.transit_S<- gibbs_resist(ysoma = ysoma, xmat = xmat, seg.id = seg.id,
                              ngibbs = ngibbs, nburn = nburn, var.betas = var.betas,
                              w = w, MaxIter = MaxIter)
# takes 25 s to run (for 1000 iter)




###################################################
### Check Posterior and Perform Model Selection ###
###################################################

######################
### North Pantanal ###
######################

## Foraging

#W/o interaction term
store.llk.forage_N<- mod.forage_N$llk
store.b.forage_N<- mod.forage_N$b.gamma
store.betas.forage_N<- mod.forage_N$betas

#look at overall convergence
plot(store.llk.forage_N, type='l')
abline(v=nburn, col='red')
plot(store.llk.forage_N[(nburn + 1):ngibbs], type='l')
acf(store.llk.forage_N[(nburn + 1):ngibbs])

plot(store.b.forage_N, type='l')
plot(store.b.forage_N[(nburn + 1):ngibbs], type='l')
acf(store.b.forage_N[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas.forage_N<- ncol(mod.forage_N$betas)
for (i in 1:nbetas.forage_N){
  plot(mod.forage_N$betas[,i], type='l')  
}

for (i in 1:nbetas_N){
  plot(mod.forage_N$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached






# AIC_mcmc = function(llk, npar) {
#   (-2 * llk) + (2*npar) 
# }
# 
# AIC_N_NoInt<- AIC_mcmc(llk = mean(store.llk_N[(nburn+1):ngibbs,]), npar = 6)
# AIC_N_Int<- AIC_mcmc(llk = mean(store.llk_N2[(nburn+1):ngibbs,]), npar = 7)
# 
# AIC_N_NoInt - AIC_N_Int  #model 2 (w/ interaction) is much better






## Transit

#W/o interaction term
store.llk.transit_N<- mod.transit_N$llk
store.b.transit_N<- mod.transit_N$b.gamma
store.betas.transit_N<- mod.transit_N$betas

#look at overall convergence
plot(store.llk.transit_N, type='l')
abline(v=nburn, col='red')
plot(store.llk.transit_N[(nburn + 1):ngibbs], type='l')
acf(store.llk.transit_N[(nburn + 1):ngibbs])

plot(store.b.transit_N, type='l')
plot(store.b.transit_N[(nburn + 1):ngibbs], type='l')
acf(store.b.transit_N[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas.transit_N<- ncol(mod.transit_N$betas)
for (i in 1:nbetas.transit_N){
  plot(mod.transit_N$betas[,i], type='l')  
}

for (i in 1:nbetas_N){
  plot(mod.transit_N$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached










######################
### South Pantanal ###
######################

## Foraging

#W/o interaction term
store.llk.forage_S<- mod.forage_S$llk
store.b.forage_S<- mod.forage_S$b.gamma
store.betas.forage_S<- mod.forage_S$betas

#look at overall convergence
plot(store.llk.forage_S, type='l')
abline(v=nburn, col='red')
plot(store.llk.forage_S[(nburn + 1):ngibbs], type='l')
acf(store.llk.forage_S[(nburn + 1):ngibbs])

plot(store.b.forage_S, type='l')
plot(store.b.forage_S[(nburn + 1):ngibbs], type='l')
acf(store.b.forage_S[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas.forage_S<- ncol(mod.forage_S$betas)
for (i in 1:nbetas.forage_S){
  plot(mod.forage_S$betas[,i], type='l')  
}

for (i in 1:nbetas.forage_S){
  plot(mod.forage_S$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached







## Transit

#W/o interaction term
store.llk.transit_S<- mod.transit_S$llk
store.b.transit_S<- mod.transit_S$b.gamma
store.betas.transit_S<- mod.transit_S$betas

#look at overall convergence
plot(store.llk.transit_S, type='l')
abline(v=nburn, col='red')
plot(store.llk.transit_S[(nburn + 1):ngibbs], type='l')
acf(store.llk.transit_S[(nburn + 1):ngibbs])

plot(store.b.transit_S, type='l')
plot(store.b.transit_S[(nburn + 1):ngibbs], type='l')
acf(store.b.transit_S[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas.transit_S<- ncol(mod.transit_S$betas)
for (i in 1:nbetas.transit_S){
  plot(mod.transit_S$betas[,i], type='l')  
}

for (i in 1:nbetas.transit_S){
  plot(mod.transit_S$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))

## Traceplots all indicate that convergence has been reached








### FIND WAY TO EXPORT AND SAVE RESULTS TO USE IN ANALYSES AND DATA VIZ

# Export results

# write.csv(, "N Armadillo Resistance Results_dispersal.csv", row.names = F)