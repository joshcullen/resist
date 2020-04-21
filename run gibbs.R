rm(list=ls())
library('Rcpp')
set.seed(1)

setwd('U:\\GIT_models\\resist')
sourceCpp('resist_aux.cpp')
source('gibbs_resist_func.R')
source('slice_betas.R')
source('slice_bgamma.R')
source('gibbs_resist.R')
dat=read.csv('fake data.csv',as.is=T)
ind=grep('cov',colnames(dat))
nparam=length(ind)+1

#priors
var.betas=c(10,rep(10,nparam-1))
gamma1=0.1
ngroups=4

#stuff for gibbs sampler
w=0.1
MaxIter=100
ngibbs=1000
nburn=ngibbs/2

mod=gibbs_resist(dat=dat,var.betas=var.betas,gamma1=gamma1,ngroups=ngroups,ngibbs=ngibbs,nburn=nburn)