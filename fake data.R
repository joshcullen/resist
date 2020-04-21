rm(list=ls())
set.seed(4)

#calculate covariates
setwd('U:\\GIT_models\\resist')
n=10000
nparam=5
xmat=matrix(runif(n*nparam,min=-3,max=3),n,nparam)
nomes.cov=paste0('covs',1:nparam)
colnames(xmat)=nomes.cov
n=nrow(xmat)

#get groups
ngroup=4
init1=sort(c(1,sample(1:n,size=n/10)))
end1=c(init1[-1]-1,n)
aux.true=aux=data.frame(init1=init1,end1=end1,z=sample(1:ngroup,size=length(init1),replace=T))

#get media
nparam=6
betas.true=betas=matrix(c(-1,0 ,1,0,-1,0,
                           1,0 ,1,0, 1,0,
                          -1,-1,0,0, 0,0,
                           0, 0,1,1, 0,0),nparam,ngroup)
media=exp(cbind(1,xmat)%*%betas); range(media)

b.true=b=200
atmp=b*media

#generate data
a.gamma=rep(NA,n)
for (i in 1:n){
  cond=i >= aux.true$init1 & i <= aux.true$end1
  z=aux.true$z[cond]
  a.gamma[i]=atmp[i,z]
}
y=rgamma(n,a.gamma,b)
plot(a.gamma/b,y)
fim=as.data.frame(cbind(y,xmat))

#aggregate these data
fim$ysoma=NA
fim$seg.id=NA

for (i in 1:nrow(aux)){
  seq1=aux$init1[i]:aux$end1[i]
  fim$seg.id[seq1]=i
  if (n==1) ysoma=fim[seq1,'y']
  if (n> 1) ysoma=sum(fim[seq1,'y'])
  fim$ysoma[aux$end1[i]]=ysoma
}

ind=which(colnames(fim)=='y')
setwd('U:\\GIT_models\\resist')
write.csv(fim[,-ind],'fake data.csv',row.names=F)