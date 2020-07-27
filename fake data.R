rm(list=ls())
set.seed(18)

setwd('U:\\GIT_models\\resist')
n=50000
nparam=3
xmat=matrix(runif(n*nparam,min=-2,max=2),n,nparam)
nomes.cov=paste0('covs',1:nparam)
colnames(xmat)=nomes.cov
n=nrow(xmat)

betas.true=betas=runif(nparam+1)
media=exp(cbind(1,xmat)%*%betas); range(round(media,3))

b.true=b=4
a=b*media
ymat=rgamma(n,a,b) 

fim=as.data.frame(xmat)
fim$ysoma=NA
fim$seg.id=NA

#aggregate these data
ind=floor(c(seq(from=1,to=n,by=n/5000),n+1)) #has to include 1 and n to use all observations
for (i in 2:length(ind)){
  seq1=ind[i-1]:(ind[i]-1)
  n=length(seq1)
  fim$seg.id[seq1]=i-1
  ysoma=ymat[seq1]
  fim$ysoma[ind[i]-1]=sum(ysoma)
}
max(fim$seg.id)
length(unique(fim$seg.id))
range(fim$ysoma,na.rm=T)

#export results
setwd('U:\\GIT_models\\resist')
write.csv(fim,'fake data.csv',row.names=F)