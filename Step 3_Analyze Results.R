### Visualize Results from Best Models ###


### North Pantanal

#look at betas (convert to data frame)
store.betas_N.df<- data.frame(store.betas_N1[(nburn+1):ngibbs, ])
names(store.betas_N.df)<- c("int","dist2rd","ndvi","lunar")
store.betas.long_N<- tidyr::pivot_longer(store.betas_N.df, cols = names(store.betas_N.df),
                                         names_to = "betas")
store.betas.long_N$betas<- factor(store.betas.long_N$betas,
                                   levels = names(store.betas_N.df))

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






### South Pantanal

#look at betas (convert to data frame)
store.betas_S.df<- data.frame(store.betas_S2[(nburn+1):ngibbs, ])
names(store.betas_S.df)<- c("int","dist2rd","ndvi","lunar","dist_ndvi")
store.betas.long_S<- tidyr::pivot_longer(store.betas_S.df, cols = names(store.betas_S.df),
                                         names_to = "betas")
store.betas.long_S$betas<- factor(store.betas.long_S$betas,
                                  levels = names(store.betas_S.df))

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





#### Create Predictive Surfaces (dist2rd, ndvi, lunar) ####

## North Pantanal

#extract beta coeffs (mean)
betas_N<- colMeans(store.betas_N.df)

#Need to center and scale raster values so comparable to beta coeffs
### IMPROVE CODE SINCE COVARS.N IS GENERATED WITHIN ANOTHER SCRIPT
covars.N2<- covars.N[[c("dist2rd","ndvi")]]
covars.N2$dist2rd<- scale(covars.N2$dist2rd, center = T, scale = T)
covars.N2$ndvi<- scale(covars.N2$ndvi, center = T, scale = T)

scaled_lunar_N<- path.N$lunar %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame() %>% 
  summarise(min=min(V1), mean=mean(V1), max=max(V1))



##Perform raster math using beta coeffs (include intercept and beta coeff for lunar as constants)

#New moon
resistSurfN_new<- exp(
  betas_N["int"] + 
  betas_N["dist2rd"]*covars.N2$dist2rd + 
  betas_N["ndvi"]*covars.N2$ndvi + 
  betas_N["lunar"]*scaled_lunar_N$min  #for new moon
  )
resistSurfN_new.df<- as.data.frame(resistSurfN_new, xy=T) %>% 
  mutate(phase = "New")

#Quarter moon
resistSurfN_quarter<- exp(
  betas_N["int"] + 
    betas_N["dist2rd"]*covars.N2$dist2rd + 
    betas_N["ndvi"]*covars.N2$ndvi + 
    betas_N["lunar"]*scaled_lunar_N$mean  #for quarter moon
)
resistSurfN_quarter.df<- as.data.frame(resistSurfN_quarter, xy=T) %>% 
  mutate(phase = "Quarter")

#Full moon
resistSurfN_full<- exp(
  betas_N["int"] + 
    betas_N["dist2rd"]*covars.N2$dist2rd + 
    betas_N["ndvi"]*covars.N2$ndvi + 
    betas_N["lunar"]*scaled_lunar_N$max  #for full moon
)
resistSurfN_full.df<- as.data.frame(resistSurfN_full, xy=T) %>% 
  mutate(phase = "Full")


#Combine all results together for each level of lunar illumination
resistSurfN.df<- rbind(resistSurfN_new.df, resistSurfN_quarter.df, resistSurfN_full.df)
resistSurfN.df$phase<- factor(resistSurfN.df$phase, levels = c("New", "Quarter", "Full"))


ggplot() +
  geom_tile(data = resistSurfN.df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nper Cell (s)", option = "inferno", na.value = "n") +
  geom_point(data = dat.N, aes(x, y, color = id), size = 0.5, alpha = 0.2, show.legend = F) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing", title = "North Pantanal Resistance Surface") +
  theme_bw() +
  coord_equal() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  guides(fill = guide_colourbar(barwidth = 30, barheight = 1)) +
  facet_wrap(~ phase)









## South Pantanal

#extract beta coeffs (mean)
betas_S<- colMeans(store.betas_S.df)

#Need to center and scale raster values so comparable to beta coeffs
covars.S2<- covars.S[[c("dist2rd","ndvi")]]
values(covars.S2$ndvi)[values(covars.S2$ndvi) < 0.2]<- NA  #Mask water on landscape
covars.S2$dist2rd<- scale(covars.S2$dist2rd, center = T, scale = T)
covars.S2$ndvi<- scale(covars.S2$ndvi, center = T, scale = T)
dist_ndvi.rast<- covars.S2$dist2rd * covars.S2$ndvi

covars.S2<- stack(covars.S2, dist_ndvi.rast) %>% brick()

scaled_lunar_S<- path.S$lunar %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame() %>% 
  summarise(min=min(V1), mean=mean(V1), max=max(V1))


#Perform raster math using beta coeffs (include intercept and beta coeff for lunar as constants)

#New moon
resistSurfS_new<- exp(
  betas_S["int"] + 
    betas_S["dist2rd"]*covars.S2$dist2rd + 
    betas_S["ndvi"]*covars.S2$ndvi + 
    betas_S["lunar"]*scaled_lunar_S$min +  #for new moon
    betas_S["dist_ndvi"]*covars.S2$layer
)
resistSurfS_new.df<- as.data.frame(resistSurfS_new, xy=T) %>% 
  mutate(phase = "New")

#Quarter moon
resistSurfS_quarter<- exp(
  betas_S["int"] + 
    betas_S["dist2rd"]*covars.S2$dist2rd + 
    betas_S["ndvi"]*covars.S2$ndvi + 
    betas_S["lunar"]*scaled_lunar_S$mean +  #for quarter moon
    betas_S["dist_ndvi"]*covars.S2$layer
)
resistSurfS_quarter.df<- as.data.frame(resistSurfS_quarter, xy=T) %>% 
  mutate(phase = "Quarter")

#Full moon
resistSurfS_full<- exp(
  betas_S["int"] + 
    betas_S["dist2rd"]*covars.S2$dist2rd + 
    betas_S["ndvi"]*covars.S2$ndvi + 
    betas_S["lunar"]*scaled_lunar_S$max +  #for full moon
    betas_S["dist_ndvi"]*covars.S2$layer
)
resistSurfS_full.df<- as.data.frame(resistSurfS_full, xy=T) %>% 
  mutate(phase = "Full")


#Combine all results together for each level of lunar illumination
resistSurfS.df<- rbind(resistSurfS_new.df, resistSurfS_quarter.df, resistSurfS_full.df)
resistSurfS.df$phase<- factor(resistSurfS.df$phase, levels = c("New", "Quarter", "Full"))


ggplot() +
  geom_tile(data = resistSurfS.df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nper Cell (s)", option = "inferno", na.value = "n") +
  geom_point(data = dat.S, aes(x, y, color = id), size = 0.5, alpha = 0.2, show.legend = F) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing", title = "South Pantanal Resistance Surface") +
  theme_bw() +
  coord_equal() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  guides(fill = guide_colourbar(barwidth = 30, barheight = 1)) +
  facet_wrap(~ phase)
