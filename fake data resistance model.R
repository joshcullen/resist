rm(list=ls())
set.seed(1)

xmax=10000
xstep=0.1
nsteps=xmax/xstep
tmp=runif(100,min=-1,max=1)
cov1=rep(tmp,each=xmax/100)

#parameters 
b0.true=b0=1
b1.true=b1=0.1

#simulate data
media=exp(b0+b1*cov1)
b.gamma.true=b.gamma=1
a1=media*b.gamma
delta.t=rgamma(nsteps,a1,b.gamma)
hist(delta.t)

dist=cumsum(rep(xstep,nsteps))
time.tot=cumsum(delta.t)
fim=data.frame(dist=dist,time.tot=time.tot)
fim$select=0
nobs=nrow(fim)

#select data every 30 seconds
target.time=30
tmp=abs(fim$time.tot-target.time)
ind1=0
fim$seg.id=NA
for (i in 1:100000){
  print(i)

  #get new ind0 and ind1
  ind0=ind1+1
  max1=min(c(ind0+1000,nobs))
  tmp=abs(fim$time.tot[ind0:max1]-target.time*i)
  ind1=which(tmp==min(tmp))+(ind0-1)
  
  #use this information to set seg.id and select
  seq1=ind0:ind1
  fim$seg.id[seq1]=i
  fim$select[ind1]=1

  #stop if finished
  if (ind1==nobs) break
}
sum(fim$select)

fim$cov1=cov1
fim$dt=fim$select*target.time
cond=fim$select==0; fim$dt[cond]=NA

setwd('U:\\GIT_models\\resist')
fim1=fim[,c('seg.id','cov1','dt')]
write.csv(fim1,'fake data resistance model.csv',row.names=F)