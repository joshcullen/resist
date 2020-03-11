plot(store.llk,type='l')
abline(v=nburn,col='red')
plot(store.llk[nburn:ngibbs],type='l')

round(apply(store.betas[nburn:ngibbs,],2,mean),2)

plot(store.betas[nburn:ngibbs,1],type='l')

plot(store.b,type='l')
plot(store.b[nburn:ngibbs],type='l')

plot(store.b[nburn:ngibbs],store.betas[nburn:ngibbs,1])