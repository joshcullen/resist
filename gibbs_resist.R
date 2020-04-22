rm(list=ls())
library('Rcpp')
set.seed(1)

setwd('U:\\GIT_models\\resist')
source('gibbs_resist_func.R')
sourceCpp('resist_aux.cpp')
dat=read.csv('fake data.csv',as.is=T)
n=nrow(dat)
ind=grep('cov',colnames(dat))
xmat=data.matrix(cbind(1,dat[,ind]))
nparam=ncol(xmat)
seg.id=dat$seg.id
ngroup=3

#get y soma
tmp=unique(dat[,c('seg.id','ysoma')])
cond=!is.na(tmp$ysoma)
ysoma=tmp[cond,'ysoma']
nagg=length(ysoma)

#priors
var.betas=c(100,rep(1,nparam-1))

#initial parameters
betas=matrix(0,nparam,ngroup)
b.gamma=2
z=sample(1:ngroup,size=nagg,replace=T)

#stuff for gibbs sampler
ngibbs=1000
nburn=ngibbs/2
jump1=list(betas=matrix(1,nparam,ngroup),b.gamma=1)
accept1=list(betas=matrix(0,nparam,ngroup),b.gamma=0)
store.betas=matrix(NA,ngibbs,nparam*ngroup)
store.b=matrix(NA,ngibbs,1)
store.llk=matrix(NA,ngibbs,1)
accept.output=100

for (i in 1:ngibbs){
  print(i)
  
  #sample betas
  tmp=sample.betas(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$betas,
                   b.gamma=b.gamma,nparam=nparam,var.betas=var.betas,
                   seg.id=seg.id,ngroup=ngroup,nagg=nagg,z=z)
  betas=tmp$betas
  accept1$betas=accept1$betas+tmp$accept
  # betas[1]=-1
  
  #sample b.gamma
  tmp=sample.b.gamma(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$b.gamma,
                     b.gamma=b.gamma,seg.id=seg.id,ngroup=ngroup,nagg=nagg,z=z)
  b.gamma=tmp$b.gamma
  accept1$b.gamma=accept1$b.gamma+tmp$accept
  # b.gamma=b.true
  
  #sample z
  z=sample.z(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
             seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  
  #get llk
  p=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
            seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  #sum the loglikel for the correct group
  llk=0
  for (j in 1:ngroup){
    cond=z==j
    llk=llk+sum(p[cond,j])
  }    

  #adaptation MH algorithm
  if (i<nburn & i%%accept.output==0){
    k=print.adapt(accept1z=accept1,jump1z=jump1,accept.output=accept.output)
    accept1=k$accept1
    jump1=k$jump1
  }
  
  #store results
  store.betas[i,]=betas
  store.b[i]=b.gamma
  store.llk[i]=llk
  z.estim=z
}
