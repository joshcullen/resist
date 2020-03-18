rm(list=ls())
library('Rcpp')
set.seed(1)

setwd('U:\\GIT_models\\resist')
sourceCpp('resist_aux.cpp')
source('gibbs_resist_func.R')
source('slice_betas.R')
dat=read.csv('fake data.csv',as.is=T)
n=nrow(dat)
ind=grep('cov',colnames(dat))
xmat=data.matrix(cbind(1,dat[,ind]))
nparam=ncol(xmat)
seg.id=dat$seg.id

#get y soma
tmp=unique(dat[,c('seg.id','ysoma')])
cond=!is.na(tmp$ysoma)
ysoma=tmp[cond,'ysoma']
n.ysoma=length(ysoma)

#priors
var.betas=c(10,rep(1,nparam-1))
gamma1=0.1

#initial parameters
ngroups=4
betas=matrix(0,nparam,ngroups)
b.gamma=2
z=sample(1:ngroups,size=n.ysoma,replace=T)
theta=rep(1/ngroups,ngroups)

#stuff for gibbs sampler
w=0.1
MaxIter=100
ngibbs=1000
nburn=ngibbs/2
jump1=list(b.gamma=1)
accept1=list(b.gamma=0)
store.betas=matrix(NA,ngibbs,nparam*ngroups)
store.z=matrix(NA,ngibbs,n.ysoma)
store.b=matrix(NA,ngibbs,1)
store.llk=matrix(NA,ngibbs,1)
nadapt=50

for (i in 1:ngibbs){
  print(i)
  
  #sample betas 
  betas=Sample_betas(ngroups=ngroups,nparam=nparam,xmat=xmat,z=z,
                     ysoma=ysoma,betas=betas,b.gamma=b.gamma,var.betas=var.betas,
                     w=w,MaxIter=MaxIter,seg.id=seg.id)
  # tmp=sample.betas(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$betas,
  #                  b.gamma=b.gamma,nparam=nparam,var.betas=var.betas,
  #                  seg.id=seg.id,z=z,ngroups=ngroups,n=n)
  # betas=tmp$betas
  # accept1$betas=accept1$betas+tmp$accept
  # betas=betas.true
  
  #sample b.gamma
  # tmp=sample.b.gamma(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$b.gamma,
  #                    b.gamma=b.gamma,seg.id=seg.id,z=z,n=n)
  # b.gamma=tmp$b.gamma
  # accept1$b.gamma=accept1$b.gamma+tmp$accept
  b.gamma=b.true
  
  #sample z 
  # z=sample.z(xmat=xmat,betas=betas,n.ysoma=n.ysoma,ngroups=ngroups,seg.id=seg.id,
  #            b.gamma=b.gamma,ysoma=ysoma,ltheta=log(theta))
  z=aux.true$z
  
  #estimate theta
  # theta=sample.theta(z=z,gamma1=gamma1,ngroups=ngroups)
  theta=rep(1/ngroups,ngroups)
  
  #get llk
  llk=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,seg.id=seg.id,z=z,n=n)
  
  #store results
  store.betas[i,]=betas
  store.b[i]=b.gamma
  store.llk[i]=llk
  store.z[i,]=z
  
  #adapt MH
  if (i%%nadapt==0 & i<nburn){
    for (j in 1:length(jump1)){
      accept.rate=accept1[[j]]/nadapt
      jump.tmp=jump1[[j]]
      print(accept.rate)
      cond=accept.rate<0.1; jump.tmp[cond]=jump.tmp[cond]/2
      cond=accept.rate>0.5; jump.tmp[cond]=jump.tmp[cond]*2
      jump1[[j]]=jump.tmp
      accept1[[j]]=rep(0,length(jump.tmp))
    }
  } 
}
