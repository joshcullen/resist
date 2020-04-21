plot(mod$llk,type='l')
nburn=9500
abline(v=nburn,col='red')
plot(mod$llk[nburn:ngibbs],type='l')

plot(mod$b.gamma,type='l')
plot(mod$b.gamma[nburn:ngibbs],type='l')
quantile(mod$b.gamma[nburn:ngibbs],c(0.025,0.5,0.975))

mod$theta[ngibbs,]
tmp=data.frame(zestim=mod$z[ngibbs,],ztrue=aux.true$z)
tmp1=table(tmp); tmp1

ordem=numeric()
for (i in 1:ncol(tmp1)){
  aux=which(tmp1[,i]==max(tmp1[,i]))  
  n=length(aux)
  if (n==1) ordem=c(ordem,aux)
  if (n>1) ordem=c(ordem,sample(aux,size=1))
}
tmp1[ordem,]

betas.estim=matrix(mod$betas[ngibbs,],nparam,ngroups)
rango=range(c(betas.estim,betas.true))
plot(betas.estim[,ordem],betas.true,xlim=rango,ylim=rango)
lines(rango,rango,col='red')