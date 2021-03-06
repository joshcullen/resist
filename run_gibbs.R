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
xmat=data.matrix(cbind(1,dat[,ind])) #notice inclusion of intercept
seg.id=dat$seg.id

#get y soma
tmp=unique(dat[,c('seg.id','dt')])
cond=!is.na(tmp$dt)
ysoma=tmp[cond,'dt']
ngibbs=1000
nburn=ngibbs/2
w=0.01
MaxIter=1000

#priors
var.betas=c(100,rep(10,ncol(xmat)-1))

mod.res=gibbs_resist(ysoma=ysoma,xmat=xmat,seg.id=seg.id,
                     ngibbs=ngibbs,nburn=nburn,var.betas=var.betas,
                     w=w,MaxIter=MaxIter)