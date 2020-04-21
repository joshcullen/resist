plot(store.llk,type='l')
nburn=200
abline(v=nburn,col='red')
plot(store.llk[nburn:ngibbs],type='l')

plot(store.b,type='l')
plot(store.b[nburn:ngibbs],type='l')

plot(store.b[nburn:ngibbs],store.betas[nburn:ngibbs,1])

plot(store.betas[1:ngibbs,1],type='l')