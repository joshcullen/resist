#look at overall convergence
plot(store.llk,type='l')
nburn=400
abline(v=nburn,col='red')
plot(store.llk[nburn:ngibbs],type='l')

plot(store.b,type='l')
plot(store.b[nburn:ngibbs],type='l')

#look at convergence betas
par(mfrow=c(3,3),mar=rep(1,4))
ind=sample(1:ncol(store.betas),size=9)
for (i in 1:9) plot(store.betas[1:ngibbs,ind[i]],type='l')
for (i in 1:9) plot(store.betas[nburn:ngibbs,ind[i]],type='l')

#look at z's
fim=data.frame(z.estim=z.estim,z.true=z.true)
tab1=table(fim)
ordem=numeric()
for (i in 1:ncol(tab1)){
  ind=which(tab1[,i]==max(tab1[,i]))
  ordem=c(ordem,ind)
}
tab1[ordem,]

#look at betas
par(mfrow=c(1,1))
betas.estim=matrix(store.betas[ngibbs,],ncol(store.betas)/ngroup,ngroup)
rango=range(c(betas.estim,store.betas))
plot(betas.true,betas.estim[,ordem],xlim=rango,ylim=rango)
lines(rango,rango,col='red')