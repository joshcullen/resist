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

# w/o intercept
ggplot(store.betas.long_N %>% filter(betas != "int"), aes(x=betas, y=value)) +
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

# w/o intercept
ggplot(store.betas.long_S %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(color="darkturquoise") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "South Pantanal") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))





#### Create Predictive Surfaces (dist2rd, ndvi) ####

## North Pantanal

#extract beta coeffs (mean)
betas_N<- colMeans(store.betas_N.df)

#Need to center and scale raster values so comparable to beta coeffs
covars.N2<- covars.N[[c("dist2rd","ndvi")]]
covars.N2$dist2rd<- scale(covars.N2$dist2rd, center = T, scale = T)
covars.N2$ndvi<- scale(covars.N2$ndvi, center = T, scale = T)


#Perform raster math using beta coeffs (include intercept and beta coeff for lunar as constants)
resist_surf_N<- exp(
  betas_N["int"] + 
  betas_N["dist2rd"]*covars.N2$dist2rd + 
  betas_N["ndvi"]*covars.N2$ndvi + 
  betas_N["lunar"]
  )
resist_surf_N_df<- as.data.frame(resist_surf_N, xy=T)
plot(resist_surf_N)

ggplot() +
  geom_tile(data = resist_surf_N_df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nMoving (s)", option = "inferno", na.value = "n") +
  geom_point(data = dat.N, aes(x, y, color = id)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing", title = "North Pantanal Resistance Surface") +
  theme_bw() +
  coord_equal()









## South Pantanal

#extract beta coeffs (mean)
betas_S<- colMeans(store.betas_S.df)

#Need to center and scale raster values so comparable to beta coeffs
covars.S2<- covars.S[[c("dist2rd","ndvi")]]
covars.S2$dist2rd<- scale(covars.S2$dist2rd, center = T, scale = T)
covars.S2$ndvi<- scale(covars.S2$ndvi, center = T, scale = T)


#Perform raster math using beta coeffs (include intercept and beta coeff for lunar as constants)
resist_surf_S<- exp(
  betas_S["int"] + 
    betas_S["dist2rd"]*covars.S2$dist2rd + 
    betas_S["ndvi"]*covars.S2$ndvi + 
    betas_S["lunar"]
)
resist_surf_S_df<- as.data.frame(resist_surf_S, xy=T)
plot(resist_surf_S)

ggplot() +
  geom_tile(data = resist_surf_S_df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nMoving (s)", option = "inferno", na.value = "n") +
  geom_point(data = dat.S, aes(x, y, color = id)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing", title = "South Pantanal Resistance Surface") +
  theme_bw() +
  coord_equal()
