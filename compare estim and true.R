plot(store.llk,type='l')
nburn=250
abline(v=nburn,col='red')
plot(store.llk[nburn:ngibbs],type='l')

plot(store.b,type='l')
plot(store.b[nburn:ngibbs],type='l')

plot(store.betas[1:ngibbs,1],type='l')