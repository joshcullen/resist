rm(list=ls())
library('Rcpp')
library('mvtnorm')
set.seed(92)

setwd('U:\\GIT_models\\resist')
source('gibbs_resist.R')
source('gibbs_resist_func.R')
source('slice_b_gamma.R')
source('slice_betas.R')
sourceCpp('resist_aux.cpp')
dat=read.csv('fake data resistance model.csv',as.is=T)
ind=grep('cov',colnames(dat))
xmat=matrix(dat[,ind],nrow(dat),1) #no intercept in design matrix
seg.id=dat$seg.id

#get y soma
tmp=unique(dat[,c('seg.id','dt')])
cond=!is.na(tmp$dt)
ysoma=tmp[cond,'dt']
ngibbs=1000
nburn=ngibbs/2
w=0.1
MaxIter=100

#priors
var.betas=c(100,rep(10,ncol(xmat)-1))

#get b0.estim
tmp=table(seg.id)
b0.estim=log(mean(ysoma/tmp))

mod.res=gibbs_resist(ysoma=ysoma,xmat=xmat,seg.id=seg.id,
                     ngibbs=ngibbs,nburn=nburn,var.betas=var.betas,
                     w=w,MaxIter=MaxIter,b0.estim=b0.estim)
