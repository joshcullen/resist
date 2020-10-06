### Visualize Results from Best Models ###

library(ggridges)
library(raster)

### North Pantanal

## Foraging

#look at betas (convert to data frame)
store.betas_Nforage<- data.frame(mod.forage_N$betas[(nburn+1):ngibbs, ])
names(store.betas_Nforage)<- c("int","slope", "ndvi","t.ar","rain",
                               paste("spline", 1:ncol(spline.N.forage), sep = "."))
store.betas.long_Nforage<- tidyr::pivot_longer(store.betas_Nforage,
                                               cols = names(store.betas_Nforage),
                                               names_to = "betas")
store.betas.long_Nforage$betas<- factor(store.betas.long_Nforage$betas,
                                   levels = names(store.betas_Nforage))

ggplot(store.betas.long_Nforage, aes(x=betas, y=value)) +
  geom_boxplot(color="firebrick") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "North Pantanal (Foraging)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# w/o intercept
ggplot(store.betas.long_Nforage %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(color="firebrick") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "North Pantanal (Foraging)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggplot(store.betas.long_Nforage %>% filter(betas != "int"),aes(y=betas, x=value, fill = betas)) +
  geom_density_ridges() +
  scale_fill_viridis_d("Coeffs", guide = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0, size = 0.5) +
  labs(y="Beta Coefficients", x="Value", title = "North Pantanal (Foraging)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))




## Transit

#look at betas (convert to data frame)
store.betas_Ntransit<- data.frame(mod.transit_N$betas[(nburn+1):ngibbs, ])
names(store.betas_Ntransit)<- c("int","slope", "ndvi","t.ar","rain",
                                paste("spline", 1:ncol(spline.N.transit), sep = "."))
store.betas.long_Ntransit<- tidyr::pivot_longer(store.betas_Ntransit,
                                               cols = names(store.betas_Ntransit),
                                               names_to = "betas")
store.betas.long_Ntransit$betas<- factor(store.betas.long_Ntransit$betas,
                                        levels = names(store.betas_Ntransit))

ggplot(store.betas.long_Ntransit, aes(x=betas, y=value)) +
  geom_boxplot(color="firebrick") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "North Pantanal (Transit)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# w/o intercept
ggplot(store.betas.long_Ntransit %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(color="firebrick") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "North Pantanal (Transit)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggplot(store.betas.long_Ntransit %>% filter(betas != "int"), aes(y=betas, x=value,
                                                                 fill = betas)) +
  geom_density_ridges() +
  scale_fill_viridis_d("Coeffs", guide = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0, size = 0.5) +
  labs(y="Beta Coefficients", x="Value", title = "North Pantanal (Transit)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))






### South Pantanal

## Foraging

#look at betas (convert to data frame)
store.betas_Sforage<- data.frame(mod.forage_S$betas[(nburn+1):ngibbs, ])
names(store.betas_Sforage)<- c("int","slope", "ndvi","t.ar","rain",
                               paste("spline", 1:ncol(spline.S.forage), sep = "."))
store.betas.long_Sforage<- tidyr::pivot_longer(store.betas_Sforage,
                                               cols = names(store.betas_Sforage),
                                               names_to = "betas")
store.betas.long_Sforage$betas<- factor(store.betas.long_Sforage$betas,
                                        levels = names(store.betas_Sforage))

ggplot(store.betas.long_Sforage, aes(x=betas, y=value)) +
  geom_boxplot(color="darkturquoise") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "South Pantanal (Foraging)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# w/o intercept
ggplot(store.betas.long_Sforage %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(color="darkturquoise") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "South Pantanal (Foraging)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggplot(store.betas.long_Sforage %>% filter(betas != "int"),aes(y=betas, x=value, fill = betas)) +
  geom_density_ridges() +
  scale_fill_viridis_d("Coeffs", guide = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0, size = 0.5) +
  labs(y="Beta Coefficients", x="Value", title = "South Pantanal (Foraging)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))




## Transit

#look at betas (convert to data frame)
store.betas_Stransit<- data.frame(mod.transit_S$betas[(nburn+1):ngibbs, ])
names(store.betas_Stransit)<-  c("int","slope", "ndvi","t.ar","rain",
                                 paste("spline", 1:ncol(spline.N.transit), sep = "."))
store.betas.long_Stransit<- tidyr::pivot_longer(store.betas_Stransit,
                                                cols = names(store.betas_Stransit),
                                                names_to = "betas")
store.betas.long_Stransit$betas<- factor(store.betas.long_Stransit$betas,
                                         levels = names(store.betas_Stransit))

ggplot(store.betas.long_Stransit, aes(x=betas, y=value)) +
  geom_boxplot(color="darkturquoise") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "South Pantanal (Transit)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# w/o intercept
ggplot(store.betas.long_Stransit %>% filter(betas != "int"), aes(x=betas, y=value)) +
  geom_boxplot(color="darkturquoise") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Beta Coefficients", y="Value", title = "South Pantanal (Transit)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggplot(store.betas.long_Stransit %>% filter(betas != "int"), aes(y=betas, x=value,
                                                                 fill = betas)) +
  geom_density_ridges() +
  scale_fill_viridis_d("Coeffs", guide = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0, size = 0.5) +
  labs(y="Beta Coefficients", x="Value", title = "South Pantanal (Transit)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))









#### Create Predictive Surfaces (dist2rd, slope, ndvi, t.ar, rain) ####

#Load data to plot tracks
dat<- read.csv("Armadillo HMM Results.csv", header = T, sep = ",")

dat<- dat %>% 
  rename(id = ID) %>% 
  mutate_at("id", as.character) %>% 
  mutate_at("state", as.factor) %>% 
  mutate_at("state", ~recode(., '1' = "Burrow",
                             '2' = "Foraging", '3' = "Transit"))
dat$date<- lubridate::as_datetime(dat$date)

# Separate tracks by region (N or S)
dat.N<- dat %>% filter(region == "N")
dat.S<- dat %>% filter(region == "S")

## North Pantanal

#extract beta coeffs (mean)
betas.forage_N<- colMeans(store.betas_Nforage)

#Need to center and scale raster values so comparable to beta coeffs

#Load env raster data
dist2rdN<- raster('dist2rd_N.tif')
slopeN<- raster('slope_N.tif')
ndviN<- raster('ndvi_N.tif')
# lulcN<- raster('lulc_N.tif')

covars.N<- brick(dist2rdN, slopeN, ndviN)
covars.N$dist2rd_N<- scale(covars.N$dist2rd_N, center = T, scale = T)
covars.N$slope_N<- scale(covars.N$slope_N, center = T, scale = T)
covars.N$ndvi_N<- scale(covars.N$ndvi_N, center = T, scale = T)

# Mask all unused pixels
ind_N<- unique(cellFromXY(slopeN, dat.N[, c("x","y")]))
covars.N_masked<- covars.N
covars.N_masked[setdiff(1:ncell(covars.N_masked), ind_N)] <- NA


scaled_t.ar_N<- path.N$t.ar %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame() %>% 
  summarise(min=min(V1, na.rm = T), mean=mean(V1, na.rm = T), max=max(V1, na.rm = T))

scaled_rain_N<- path.N$rain %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame() %>% 
  summarise(min=min(V1, na.rm = T), mean=mean(V1, na.rm = T), max=max(V1, na.rm = T))


## Foraging ##

##Perform raster math using beta coeffs

#Min recorded temperature
resistSurfN.forage_minTemp<- exp(
  betas.forage_N["int"] + 
  betas.forage_N["slope"]*covars.N_masked$slope_N + 
  betas.forage_N["ndvi"]*covars.N_masked$ndvi_N + 
  betas.forage_N["t.ar"]*scaled_t.ar_N$min +  #for min temp
  betas.forage_N["rain"]*scaled_rain_N$mean +  #for avg rainfall
  betas.forage_N["spline.1"]*covars.N_masked$dist2rd_N +
  betas.forage_N["spline.2"]*covars.N_masked$dist2rd_N
  )
resistSurfN.forage_minTemp.df<- as.data.frame(resistSurfN.forage_minTemp, xy=T) %>% 
  mutate(temp.level = "Min")

#Avg recorded temperature
resistSurfN.forage_avgTemp<- exp(
  betas.forage_N["int"] + 
    betas.forage_N["slope"]*covars.N_masked$slope_N + 
    betas.forage_N["ndvi"]*covars.N_masked$ndvi_N + 
    betas.forage_N["t.ar"]*scaled_t.ar_N$mean +  #for mean temp
    betas.forage_N["rain"]*scaled_rain_N$mean +  #for avg rainfall
    betas.forage_N["spline.1"]*covars.N_masked$dist2rd_N +
    betas.forage_N["spline.2"]*covars.N_masked$dist2rd_N
)
resistSurfN.forage_avgTemp.df<- as.data.frame(resistSurfN.forage_avgTemp, xy=T) %>% 
  mutate(temp.level = "Avg")

#Max recorded temperature
resistSurfN.forage_maxTemp<- exp(
  betas.forage_N["int"] + 
    betas.forage_N["slope"]*covars.N_masked$slope_N + 
    betas.forage_N["ndvi"]*covars.N_masked$ndvi_N + 
    betas.forage_N["t.ar"]*scaled_t.ar_N$max +  #for max temp
    betas.forage_N["rain"]*scaled_rain_N$mean +  #for avg rainfall
    betas.forage_N["spline.1"]*covars.N_masked$dist2rd_N +
    betas.forage_N["spline.2"]*covars.N_masked$dist2rd_N
)
resistSurfN.forage_maxTemp.df<- as.data.frame(resistSurfN.forage_maxTemp, xy=T) %>% 
  mutate(temp.level = "Max")


#Combine all results together for each level of lunar illumination
resistSurfN.forage.df<- rbind(resistSurfN.forage_minTemp.df, resistSurfN.forage_avgTemp.df,
                              resistSurfN.forage_maxTemp.df)
resistSurfN.forage.df$temp.level<- factor(resistSurfN.forage.df$temp.level,
                                          levels = c("Min", "Avg", "Max"))



ggplot() +
  geom_tile(data = resistSurfN.forage.df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nper Cell (min)", option = "inferno", na.value = "n") +
  # geom_point(data = dat.N %>% filter(state == "Foraging"), aes(x, y, color = id),
  #            size = 0.5, alpha = 0.2, show.legend = F) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing", title = "North Pantanal Foraging Resistance Surface") +
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
  facet_wrap(~ temp.level)





## Transit ##

##Perform raster math using beta coeffs

#extract beta coeffs (mean)
betas.transit_N<- colMeans(store.betas_Ntransit)

#Min recorded temperature
resistSurfN.transit_minTemp<- exp(
  betas.transit_N["int"] + 
    betas.transit_N["slope"]*covars.N_masked$slope_N + 
    betas.transit_N["ndvi"]*covars.N_masked$ndvi_N + 
    betas.transit_N["t.ar"]*scaled_t.ar_N$min +  #for min temp
    betas.transit_N["rain"]*scaled_rain_N$mean +  #for avg rainfall
    betas.transit_N["spline.1"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.2"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.3"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.4"]*covars.N_masked$dist2rd_N
)
resistSurfN.transit_minTemp.df<- as.data.frame(resistSurfN.transit_minTemp, xy=T) %>% 
  mutate(temp.level = "Min")

#Avg recorded temperature
resistSurfN.transit_avgTemp<- exp(
  betas.transit_N["int"] + 
    betas.transit_N["slope"]*covars.N_masked$slope_N + 
    betas.transit_N["ndvi"]*covars.N_masked$ndvi_N + 
    betas.transit_N["t.ar"]*scaled_t.ar_N$mean +  #for avg temp
    betas.transit_N["rain"]*scaled_rain_N$mean +  #for avg rainfall
    betas.transit_N["spline.1"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.2"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.3"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.4"]*covars.N_masked$dist2rd_N
)
resistSurfN.transit_avgTemp.df<- as.data.frame(resistSurfN.transit_avgTemp, xy=T) %>% 
  mutate(temp.level = "Avg")

#Max recorded temperature
resistSurfN.transit_maxTemp<- exp(
  betas.transit_N["int"] + 
    betas.transit_N["slope"]*covars.N_masked$slope_N + 
    betas.transit_N["ndvi"]*covars.N_masked$ndvi_N + 
    betas.transit_N["t.ar"]*scaled_t.ar_N$max +  #for max temp
    betas.transit_N["rain"]*scaled_rain_N$mean +  #for avg rainfall
    betas.transit_N["spline.1"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.2"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.3"]*covars.N_masked$dist2rd_N +
    betas.transit_N["spline.4"]*covars.N_masked$dist2rd_N
)
resistSurfN.transit_maxTemp.df<- as.data.frame(resistSurfN.transit_maxTemp, xy=T) %>% 
  mutate(temp.level = "Max")


#Combine all results together for each level of lunar illumination
resistSurfN.transit.df<- rbind(resistSurfN.transit_minTemp.df, resistSurfN.transit_avgTemp.df,
                              resistSurfN.transit_maxTemp.df)
resistSurfN.transit.df$temp.level<- factor(resistSurfN.transit.df$temp.level,
                                          levels = c("Min", "Avg", "Max"))



ggplot() +
  geom_tile(data = resistSurfN.transit.df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nper Cell (min)", option = "inferno", na.value = "n") +
  # geom_point(data = dat.N %>% filter(state == "Transit"), aes(x, y, color = id),
  #            size = 0.5, alpha = 0.2, show.legend = F) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing", title = "North Pantanal Transit Resistance Surface") +
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
  facet_wrap(~ temp.level)




## South Pantanal

#extract beta coeffs (mean)
betas.forage_S<- colMeans(store.betas_Sforage)

#Seed to center and scale raster values so comparable to beta coeffs

#Load env raster data
dist2rdS<- raster('dist2rd_S.tif')
slopeS<- raster('slope_S.tif')
ndviS<- raster('ndvi_S.tif')

covars.S<- brick(dist2rdS, slopeS, ndviS)
covars.S$dist2rd_S<- scale(covars.S$dist2rd_S, center = T, scale = T)
covars.S$slope_S<- scale(covars.S$slope_S, center = T, scale = T)
# values(covars.S$ndvi_S)[values(covars.S$ndvi_S) < 0.2]<- NA  #Mask water on landscape
covars.S$ndvi_S<- scale(covars.S$ndvi_S, center = T, scale = T)

# Mask all unused pixels (for foraging)
ind_S<- unique(cellFromXY(slopeS, dat.S[, c("x","y")]))
covars.S_masked<- covars.S
covars.S_masked[setdiff(1:ncell(covars.S_masked), ind_S)] <- NA

scaled_t.ar_S<- path.S$t.ar %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame() %>% 
  summarise(min=min(V1, na.rm = T), mean=mean(V1, na.rm = T), max=max(V1, na.rm = T))

scaled_rain_S<- path.S$rain %>% 
  scale(center = T, scale = T) %>% 
  as.data.frame() %>% 
  summarise(min=min(V1, na.rm = T), mean=mean(V1, na.rm = T), max=max(V1, na.rm = T))


## Foraging ##

##Perform raster math using beta coeffs

#Min recorded temperature
resistSurfS.forage_minTemp<- exp(
  betas.forage_S["int"] + 
    betas.forage_S["slope"]*covars.S_masked$slope_S + 
    betas.forage_S["ndvi"]*covars.S_masked$ndvi_S + 
    betas.forage_S["t.ar"]*scaled_t.ar_S$min +  #for min temp
    betas.forage_S["rain"]*scaled_rain_S$mean +  #for avg rainfall
    betas.forage_S["spline.1"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.2"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.3"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.4"]*covars.S_masked$dist2rd_S
)
resistSurfS.forage_minTemp.df<- as.data.frame(resistSurfS.forage_minTemp, xy=T) %>% 
  mutate(temp.level = "Min")

#Avg recorded temperature
resistSurfS.forage_avgTemp<- exp(
  betas.forage_S["int"] + 
    betas.forage_S["slope"]*covars.S_masked$slope_S + 
    betas.forage_S["ndvi"]*covars.S_masked$ndvi_S + 
    betas.forage_S["t.ar"]*scaled_t.ar_S$mean + #for min temp
    betas.forage_S["rain"]*scaled_rain_S$mean +  #for avg rainfall
    betas.forage_S["spline.1"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.2"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.3"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.4"]*covars.S_masked$dist2rd_S
)
resistSurfS.forage_avgTemp.df<- as.data.frame(resistSurfS.forage_avgTemp, xy=T) %>% 
  mutate(temp.level = "Avg")

#Max recorded temperature
resistSurfS.forage_maxTemp<- exp(
  betas.forage_S["int"] + 
    betas.forage_S["slope"]*covars.S_masked$slope_S + 
    betas.forage_S["ndvi"]*covars.S_masked$ndvi_S + 
    betas.forage_S["t.ar"]*scaled_t.ar_S$max + #for max temp
    betas.forage_S["rain"]*scaled_rain_S$mean +  #for avg rainfall
    betas.forage_S["spline.1"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.2"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.3"]*covars.S_masked$dist2rd_S +
    betas.forage_S["spline.4"]*covars.S_masked$dist2rd_S
)
resistSurfS.forage_maxTemp.df<- as.data.frame(resistSurfS.forage_maxTemp, xy=T) %>% 
  mutate(temp.level = "Max")


#Combine all results together for each level of lunar illumination
resistSurfS.forage.df<- rbind(resistSurfS.forage_minTemp.df, resistSurfS.forage_avgTemp.df,
                              resistSurfS.forage_maxTemp.df)
resistSurfS.forage.df$temp.level<- factor(resistSurfS.forage.df$temp.level,
                                          levels = c("Min", "Avg", "Max"))



ggplot() +
  geom_tile(data = resistSurfS.forage.df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nper Cell (min)", option = "inferno", na.value = "n") +
  # geom_point(data = dat.S %>% filter(state == "Foraging"), aes(x, y, color = id),
  #            size = 0.5, alpha = 0.2, show.legend = F) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing", title = "South Pantanal Foraging Resistance Surface") +
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
  facet_wrap(~ temp.level)





## Transit ##

##Perform raster math using beta coeffs

#extract beta coeffs (mean)
betas.transit_S<- colMeans(store.betas_Stransit)

#Min recorded temperature
resistSurfS.transit_minTemp<- exp(
  betas.transit_S["int"] + 
    betas.transit_S["slope"]*covars.S_masked$slope_S + 
    betas.transit_S["ndvi"]*covars.S_masked$ndvi_S + 
    betas.transit_S["t.ar"]*scaled_t.ar_S$min +  #for min temp
    betas.transit_S["rain"]*scaled_rain_S$mean +  #for avg rainfall
    betas.transit_S["spline.1"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.2"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.3"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.4"]*covars.S_masked$dist2rd_S
)
resistSurfS.transit_minTemp.df<- as.data.frame(resistSurfS.transit_minTemp, xy=T) %>% 
  mutate(temp.level = "Min")

#Avg recorded temperature
resistSurfS.transit_avgTemp<- exp(
  betas.transit_S["int"] + 
    betas.transit_S["slope"]*covars.S_masked$slope_S + 
    betas.transit_S["ndvi"]*covars.S_masked$ndvi_S + 
    betas.transit_S["t.ar"]*scaled_t.ar_S$mean + #for avg temp
    betas.transit_S["rain"]*scaled_rain_S$mean +  #for avg rainfall
    betas.transit_S["spline.1"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.2"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.3"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.4"]*covars.S_masked$dist2rd_S
)
resistSurfS.transit_avgTemp.df<- as.data.frame(resistSurfS.transit_avgTemp, xy=T) %>% 
  mutate(temp.level = "Avg")

#Max recorded temperature
resistSurfS.transit_maxTemp<- exp(
  betas.transit_S["int"] + 
    betas.transit_S["slope"]*covars.S_masked$slope_S + 
    betas.transit_S["ndvi"]*covars.S_masked$ndvi_S + 
    betas.transit_S["t.ar"]*scaled_t.ar_S$max + #for max temp
    betas.transit_S["rain"]*scaled_rain_S$mean +  #for avg rainfall
    betas.transit_S["spline.1"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.2"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.3"]*covars.S_masked$dist2rd_S +
    betas.transit_S["spline.4"]*covars.S_masked$dist2rd_S
)
resistSurfS.transit_maxTemp.df<- as.data.frame(resistSurfS.transit_maxTemp, xy=T) %>% 
  mutate(temp.level = "Max")


#Combine all results together for each level of lunar illumination
resistSurfS.transit.df<- rbind(resistSurfS.transit_minTemp.df, resistSurfS.transit_avgTemp.df,
                               resistSurfS.transit_maxTemp.df)
resistSurfS.transit.df$temp.level<- factor(resistSurfS.transit.df$temp.level,
                                           levels = c("Min", "Avg", "Max"))



ggplot() +
  geom_tile(data = resistSurfS.transit.df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nper Cell (min)", option = "inferno", na.value = "n") +
  # geom_point(data = dat.S %>% filter(state == "Transit"), aes(x, y, color = id),
  #            size = 0.5, alpha = 0.2, show.legend = F) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing", title = "South Pantanal Transit Resistance Surface") +
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
  facet_wrap(~ temp.level)
