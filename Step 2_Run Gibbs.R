library('Rcpp')
library(tictoc)
set.seed(1)

source('gibbs_resist.R')
source('gibbs_resist_func.R')
sourceCpp('resist_aux.cpp')

dat<- read.csv('Armadillo Resistance Data.csv',as.is=T)
dat$dist2rd<- scale(dat$dist2rd, center = TRUE, scale = TRUE)  #set to mean = 0 and sd = 1

ind=grep('dist',colnames(dat))
xmat=data.matrix(cbind(1,dat[,ind]))
seg.id=dat$seg.id
ngroup=10

#get y soma
tmp=unique(dat[,c('seg.id','dt')])
cond=!is.na(tmp$dt)
ysoma=tmp[cond,'dt']
ngibbs=10000
nburn=ngibbs/2

#priors
gamma1=0.1
var.betas=c(100,rep(1,ncol(xmat)-1))

tic()
mod.res=gibbs_resist(ysoma=ysoma,xmat=xmat,seg.id=seg.id,ngroup=ngroup,
                     ngibbs=ngibbs,nburn=nburn,var.betas=var.betas,
                     gamma1=gamma1)
toc()

# takes 2 hrs to run