rm(list=ls())
set.seed(1)

setwd('U:\\GIT_models\\resist')
source('gibbs_resist_func.R')
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

#priors
var.betas=c(100,rep(1,nparam-1))

#initial parameters
betas=rep(0,nparam)
b.gamma=2

#stuff for gibbs sampler
ngibbs=1000
nburn=ngibbs/2
jump1=list(betas=rep(1,nparam),b.gamma=1)
accept1=list(betas=rep(0,nparam),b.gamma=0)
store.betas=matrix(NA,ngibbs,nparam)
store.b=matrix(NA,ngibbs,1)
store.llk=matrix(NA,ngibbs,1)
nadapt=50

for (i in 1:ngibbs){
  print(i)
  
  #sample betas
  tmp=sample.betas(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$betas,
                   b.gamma=b.gamma,nparam=nparam,var.betas=var.betas,
                   seg.id=seg.id)
  betas=tmp$betas
  accept1$betas=accept1$betas+tmp$accept
  # betas[1]=-1
  
  #sample b.gamma
  tmp=sample.b.gamma(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$b.gamma,
                     b.gamma=b.gamma,seg.id=seg.id)
  b.gamma=tmp$b.gamma
  accept1$b.gamma=accept1$b.gamma+tmp$accept
  # b.gamma=b.true
  
  #get llk
  llk=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,seg.id=seg.id)
  
  #store results
  store.betas[i,]=betas
  store.b[i]=b.gamma
  store.llk[i]=llk
  
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
