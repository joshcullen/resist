plot(store.llk,type='l')
nburn=8000
abline(v=nburn,col='red')
plot(store.llk[nburn:ngibbs],type='l')

round(apply(store.betas[nburn:ngibbs,],2,mean),2)

plot(store.b,type='l')
plot(store.b[nburn:ngibbs],type='l')

tmp=data.frame(zestim=store.z[ngibbs,],ztrue=aux.true$z)
tmp1=table(tmp); tmp1

ordem=numeric()
for (i in 1:ncol(tmp1)){
  aux=which(tmp1[,i]==max(tmp1[,i]))  
  n=length(aux)
  if (n==1) ordem=c(ordem,aux)
  if (n>1) ordem=c(ordem,sample(aux,size=1))
}
tmp1[ordem,]

betas.estim=matrix(store.betas[ngibbs,],nparam,ngroups)
rango=range(c(betas.estim,betas.true))
plot(betas.estim[,ordem],betas.true,xlim=rango,ylim=rango)
lines(rango,rango,col='red')