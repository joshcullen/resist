rm(list=ls())
set.seed(1)

n=10000
nparam=5
xmat=matrix(runif(nparam*n,min=-1,max=1),n,nparam)
colnames(xmat)=paste0('covs',1:nparam)
betas=c(-1,0,1,0,-1,0)
media=exp(cbind(1,xmat)%*%betas)

b=0.5
a=b*media
y=rgamma(n,a,b)

fim=cbind(y,xmat)
setwd('U:\\GIT_models\\resist')
write.csv(fim,'fake data.csv',row.names=F)