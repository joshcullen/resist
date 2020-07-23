######################
### North Pantanal ###
######################

## 2 states

store.llk_N2=res.N_2states$llk
store.b_N2=res.N_2states$b.gamma
store.betas_N2=res.N_2states$betas
z.estim_N2=res.N_2states$z.estim

#look at overall convergence
plot(store.llk_N2,type='l')
nburn=2500
abline(v=nburn,col='red')
plot(store.llk_N2[nburn:ngibbs],type='l')
acf(store.llk_N2[nburn:ngibbs])

plot(store.b_N2,type='l')
plot(store.b_N2[nburn:ngibbs],type='l')
acf(store.b_N2[nburn:ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_N2=ncol(res.N_2states$betas)
for (i in 1:nbetas_N2){
  plot(res.N_2states$betas[,i],type='l')  
}

for (i in 1:nbetas_N2){
  plot(res.N_2states$betas[nburn:ngibbs,i],type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))


#look at betas (convert to data frame)
store.betas_N2.df<- data.frame(rbind(store.betas_N2[(nburn+1):ngibbs, 1:4],
                                  store.betas_N2[(nburn+1):ngibbs, 5:8]),
                            group = factor(rep(1:2, each = 2500)))
names(store.betas_N2.df)[2:4]<- c("dist2rd","ndvi","lunar")
names(store.betas_N2.df)[1]<- "int"
store.betas.long_N2<- tidyr::pivot_longer(store.betas_N2.df, -group, names_to = "betas")
store.betas.long_N2$betas<- factor(store.betas.long_N2$betas,
                                   levels = names(store.betas_N2.df)[1:4])

ggplot(store.betas.long_N2 %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(aes(color=group)) +
  scale_color_brewer("Behavior", palette = "Dark2") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "North Pantanal (2 states)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))





## 3 states

store.llk_N3=res.N_3states$llk
store.b_N3=res.N_3states$b.gamma
store.betas_N3=res.N_3states$betas
z.estim_N3=res.N_3states$z.estim

#look at overall convergence
plot(store.llk_N3,type='l')
nburn=2500
abline(v=nburn,col='red')
plot(store.llk_N3[nburn:ngibbs],type='l')
acf(store.llk_N3[nburn:ngibbs])

plot(store.b_N3,type='l')
plot(store.b_N3[nburn:ngibbs],type='l')
acf(store.b_N3[nburn:ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_N3=ncol(res.N_3states$betas)
for (i in 1:nbetas_N3){
  plot(res.N_3states$betas[,i],type='l')  
}

for (i in 1:nbetas_N3){
  plot(res.N_3states$betas[nburn:ngibbs,i],type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))


#look at betas (convert to data frame)
store.betas_N3.df<- data.frame(rbind(store.betas_N3[(nburn+1):ngibbs, 1:4],
                                     store.betas_N3[(nburn+1):ngibbs, 5:8],
                                     store.betas_N3[(nburn+1):ngibbs, 9:12]),
                               group = factor(rep(1:3, each = 2500)))
names(store.betas_N3.df)[2:4]<- c("dist2rd","ndvi","lunar")
names(store.betas_N3.df)[1]<- "int"
store.betas.long_N3<- tidyr::pivot_longer(store.betas_N3.df, -group, names_to = "betas")
store.betas.long_N3$betas<- factor(store.betas.long_N3$betas,
                                   levels = names(store.betas_N3.df)[1:4])

ggplot(store.betas.long_N3 %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(aes(color=group)) +
  scale_color_brewer("Behavior", palette = "Dark2") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "North Pantanal (3 states)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))




######################
### South Pantanal ###
######################

## 2 states

store.llk_S2=res.S_2states$llk
store.b_S2=res.S_2states$b.gamma
store.betas_S2=res.S_2states$betas
z.estim_S2=res.S_2states$z.estim

#look at overall convergence
plot(store.llk_S2,type='l')
nburn=2500
abline(v=nburn,col='red')
plot(store.llk_S2[nburn:ngibbs],type='l')
acf(store.llk_S2[nburn:ngibbs])

plot(store.b_S2,type='l')
plot(store.b_S2[nburn:ngibbs],type='l')
acf(store.b_S2[nburn:ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_S2=ncol(res.S_2states$betas)
for (i in 1:nbetas_S2){
  plot(res.S_2states$betas[,i],type='l')  
}

for (i in 1:nbetas_S2){
  plot(res.S_2states$betas[nburn:ngibbs,i],type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))


#look at betas (convert to data frame)
store.betas_S2.df<- data.frame(rbind(store.betas_S2[(nburn+1):ngibbs, 1:4],
                                     store.betas_S2[(nburn+1):ngibbs, 5:8]),
                               group = factor(rep(1:2, each = 2500)))
names(store.betas_S2.df)[2:4]<- c("dist2rd","ndvi","lunar")
names(store.betas_S2.df)[1]<- "int"
store.betas.long_S2<- tidyr::pivot_longer(store.betas_S2.df, -group, names_to = "betas")
store.betas.long_S2$betas<- factor(store.betas.long_S2$betas,
                                   levels = names(store.betas_S2.df)[1:4])

ggplot(store.betas.long_S2 %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(aes(color=group)) +
  scale_color_brewer("Behavior", palette = "Dark2") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "South Pantanal (2 states)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))





## 3 states

store.llk_S3=res.S_3states$llk
store.b_S3=res.S_3states$b.gamma
store.betas_S3=res.S_3states$betas
z.estim_S3=res.S_3states$z.estim

#look at overall convergence
plot(store.llk_S3,type='l')
nburn=2500
abline(v=nburn,col='red')
plot(store.llk_S3[nburn:ngibbs],type='l')
acf(store.llk_S3[nburn:ngibbs])

plot(store.b_S3,type='l')
plot(store.b_S3[nburn:ngibbs],type='l')
acf(store.b_S3[nburn:ngibbs])

#look at convergence betas
par(mfrow=c(2,2))
nbetas_S3=ncol(res.S_3states$betas)
for (i in 1:nbetas_S3){
  plot(res.S_3states$betas[,i],type='l')  
}

for (i in 1:nbetas_S3){
  plot(res.S_3states$betas[nburn:ngibbs,i],type='l')  
}
par(mfrow=c(1,1),mar=rep(3,4))


#look at betas (convert to data frame)
store.betas_S3.df<- data.frame(rbind(store.betas_S3[(nburn+1):ngibbs, 1:4],
                                     store.betas_S3[(nburn+1):ngibbs, 5:8],
                                     store.betas_S3[(nburn+1):ngibbs, 9:12]),
                               group = factor(rep(1:3, each = 2500)))
names(store.betas_S3.df)[2:4]<- c("dist2rd","ndvi","lunar")
names(store.betas_S3.df)[1]<- "int"
store.betas.long_S3<- tidyr::pivot_longer(store.betas_S3.df, -group, names_to = "betas")
store.betas.long_S3$betas<- factor(store.betas.long_S3$betas,
                                   levels = names(store.betas_S3.df)[1:4])

ggplot(store.betas.long_S3 %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(aes(color=group)) +
  scale_color_brewer("Behavior", palette = "Dark2") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "South Pantanal (3 states)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))
