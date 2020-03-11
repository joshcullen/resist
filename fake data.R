rm(list=ls())
set.seed(1)

setwd('U:\\GIT_models\\resist')
xmat=read.csv('spatially correlated xmat.csv',as.is=T)
xmat=data.matrix(xmat)
nomes.cov=colnames(xmat)
n=nrow(xmat)

betas.true=betas=c(-1,0,1,0,-1,0)
media=exp(cbind(1,xmat)%*%betas)

b.true=b=0.5
a=b*media
y=rgamma(n,a,b)
fim=as.data.frame(cbind(y,xmat))
fim$ysoma=NA
fim$seg.id=NA
#aggregate these data
ind=sort(c(sample(1:n,size=n/10),1,n)) #has to include 1 and n to use all observations
for (i in 2:length(ind)){
  seq1=ind[i-1]:(ind[i]-1)
  fim$seg.id[seq1]=i-1
  if (n==1) ysoma=fim[seq1,'y']
  if (n> 1) ysoma=sum(fim[seq1,'y'])
  fim$ysoma[ind[i]-1]=ysoma
}

ind=which(colnames(fim)=='y')
setwd('U:\\GIT_models\\resist')
write.csv(fim[,-ind],'fake data.csv',row.names=F)