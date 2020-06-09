store.llk=mod.res$llk
store.b=mod.res$b.gamma
store.betas=mod.res$betas
z.estim=mod.res$z.estim
store.theta=mod.res$theta

#look at correlation
k=cor(cbind(store.b,store.betas))
k[k < 0.5 | k > -0.5]=NA
k

#look at overall convergence
par(mfrow=c(1,1))
plot(store.llk,type='l')
nburn=900
abline(v=nburn,col='red')
plot(store.llk[nburn:ngibbs],type='l')

#look at convergence betas
par(mfrow=c(3,3),mar=rep(1,4))
ind=sample(1:ncol(store.betas),size=9)
for (i in 1:9) plot(store.betas[1:ngibbs,ind[i]],type='l')
for (i in 1:9) plot(store.betas[nburn:ngibbs,ind[i]],type='l')

#look at convergence of b.gammas
par(mfrow=c(1,1))
plot(store.b,type='l')
abline(h=b.true,col='red')

#look at z's
fim=data.frame(z.estim=z.estim,z.true=z.true)
tab1=table(fim); tab1
ordem=numeric()
for (i in 1:ncol(tab1)){
  ind=which(tab1[,i]==max(tab1[,i]))
  ordem=c(ordem,ind)
}
tab1[ordem,]

#look at betas
ngroup=4
par(mfrow=c(1,1),mar=rep(3,4))
betas.estim=matrix(store.betas[ngibbs,],ncol(store.betas)/ngroup,ngroup)
rango=range(c(betas.estim[,ordem],betas.true))
plot(betas.true,betas.estim[,ordem],xlim=rango,ylim=rango)
lines(rango,rango,col='red')
