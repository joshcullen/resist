rm(list=ls())
set.seed(3)

setwd('U:\\GIT_models\\resist')
n=50000
nparam=3
xmat=matrix(runif(n*nparam,min=-2,max=2),n,nparam)
nomes.cov=paste0('covs',1:nparam)
colnames(xmat)=nomes.cov
n=nrow(xmat)

ngroup=4
betas.true=betas=matrix(c( 2, 0 ,1,-2,
                           0, -2, 0,-1,
                          -1, 0, 0, 0,
                           0, 0, -2, 0),nparam+1,ngroup,byrow=T)
media=exp(cbind(1,xmat)%*%betas); range(round(media,3))

b.true=b=2
a=b*media
ymat=matrix(rgamma(n*ngroup,a,b),n,ngroup); 

fim=as.data.frame(xmat)
fim$z=NA
fim$ysoma=NA
fim$seg.id=NA

#aggregate these data
ind=floor(c(seq(from=1,to=n,by=n/5000),n+1)) #has to include 1 and n to use all observations
for (i in 2:length(ind)){
  seq1=ind[i-1]:(ind[i]-1)
  n=length(seq1)
  fim$seg.id[seq1]=i-1
  z=sample(1:ngroup,size=1)
  fim$z[ind[i]-1]=z
  ysoma=ymat[seq1,z]
  fim$ysoma[ind[i]-1]=sum(ysoma)
}
max(fim$seg.id)
length(unique(fim$seg.id))
range(fim$ysoma,na.rm=T)

#get z.true
tmp=unique(fim[,c('z','seg.id')])
z.true=tmp[!is.na(tmp$z),'z']
table(z.true)

#export results
ind=which(colnames(fim)=='z')
setwd('U:\\GIT_models\\resist')
write.csv(fim[,-ind],'fake data.csv',row.names=F)