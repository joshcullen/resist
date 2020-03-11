rm(list=ls())
set.seed(2)

n=1000
nparam=5

#generate spatially correlated covariates 
rho=0.9
aux=outer(X=1:n,Y=1:n,"-")
cor.mat=rho^abs(aux)
sig2=1
sigma.mat=sig2*cor.mat
aux.mat=chol(sigma.mat)
z=rnorm(n*nparam,mean=0,sd=1)
xmat=aux.mat%*%matrix(z,n,nparam); 
colnames(xmat)=paste0('covs',1:nparam)

#check results
par(mfrow=c(3,2))
for (i in 1:nparam){
  res=acf(xmat[,i],plot=F)
  true1=rho^(0:(length(res$acf)-1))
  rango=range(c(res$acf,true1))
  plot(res$acf,true1,xlim=rango,ylim=rango)
  lines(rango,rango,col='red')
}

setwd('U:\\GIT_models\\resist')
write.csv(xmat,'spatially correlated xmat.csv',row.names=F)