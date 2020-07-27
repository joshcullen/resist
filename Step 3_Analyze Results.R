######################
### North Pantanal ###
######################

store.llk_N<- mod.res_N$llk
store.b_N<- mod.res_N$b.gamma
store.betas_N<- mod.res_N$betas

#look at overall convergence
plot(store.llk_N, type='l')
nburn=500
abline(v=nburn, col='red')
plot(store.llk_N[(nburn + 1):ngibbs], type='l')
acf(store.llk_N[(nburn + 1):ngibbs])

plot(store.b_N, type='l')
plot(store.b_N[(nburn + 1):ngibbs], type='l')
acf(store.b_N[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_N<- ncol(mod.res_N$betas)
for (i in 1:nbetas_N){
  plot(mod.res_N$betas[,i], type='l')  
}

for (i in 1:nbetas_N){
  plot(mod.res_N$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))


#look at betas (convert to data frame)
store.betas_N.df<- data.frame(store.betas_N[(nburn+1):ngibbs, ])
names(store.betas_N.df)<- c("int","dist2rd","ndvi","lunar")
store.betas.long_N<- tidyr::pivot_longer(store.betas_N.df, cols = names(store.betas_N.df),
                                         names_to = "betas")
store.betas.long_N$betas<- factor(store.betas.long_N$betas,
                                   levels = names(store.betas_N.df)[1:4])

ggplot(store.betas.long_N, aes(x=betas, y=value)) +
  geom_boxplot(color="firebrick") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "North Pantanal") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))










######################
### South Pantanal ###
######################

store.llk_S<- mod.res_S$llk
store.b_S<- mod.res_S$b.gamma
store.betas_S<- mod.res_S$betas

#look at overall convergence
plot(store.llk_S, type='l')
nburn=500
abline(v=nburn, col='red')
plot(store.llk_S[(nburn + 1):ngibbs], type='l')
acf(store.llk_S[(nburn + 1):ngibbs])

plot(store.b_S, type='l')
plot(store.b_S[(nburn + 1):ngibbs], type='l')
acf(store.b_S[(nburn + 1):ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_S<- ncol(mod.res_S$betas)
for (i in 1:nbetas_S){
  plot(mod.res_S$betas[,i], type='l')  
}

for (i in 1:nbetas_S){
  plot(mod.res_S$betas[(nburn + 1):ngibbs, i], type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))


#look at betas (convert to data frame)
store.betas_S.df<- data.frame(store.betas_S[(nburn+1):ngibbs, ])
names(store.betas_S.df)<- c("int","dist2rd","ndvi","lunar")
store.betas.long_S<- tidyr::pivot_longer(store.betas_S.df, cols = names(store.betas_S.df),
                                         names_to = "betas")
store.betas.long_S$betas<- factor(store.betas.long_S$betas,
                                  levels = names(store.betas_S.df)[1:4])

ggplot(store.betas.long_S, aes(x=betas, y=value)) +
  geom_boxplot(color="darkturquoise") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "South Pantanal") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))
