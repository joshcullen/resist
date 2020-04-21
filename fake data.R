rm(list=ls())
set.seed(4)

setwd('U:\\GIT_models\\resist')
n=10000
nparam=5
xmat=matrix(runif(n*nparam,min=-1,max=1),n,nparam)
nomes.cov=paste0('covs',1:nparam)
colnames(xmat)=nomes.cov
n=nrow(xmat)

betas.true=betas=c(-1,0,1,0,-1,0)
media=exp(cbind(1,xmat)%*%betas); range(round(media,3))

b.true=b=2
a=b*media
y=rgamma(n,a,b); range(y)

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