### Visualize Results from Best Models ###

library(ggridges)
library(raster)
library(lubridate)

#look at betas (convert to data frame)
store.betas<- data.frame(mod$betas[(nburn+1):ngibbs, ])
names(store.betas)<- c("int","ndvi","awei")
store.betas.long<- tidyr::pivot_longer(store.betas,
                                               cols = names(store.betas),
                                               names_to = "betas")
store.betas.long$betas<- factor(store.betas.long$betas,
                                        levels = names(store.betas))

ggplot(store.betas.long, aes(x=betas, y=value)) +
  geom_boxplot(color="firebrick") +
  geom_hline(yintercept = 0, size = 0.5) +
  labs(x="Effect Sizes", y="Value") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))

ggplot(store.betas.long, aes(y=betas, x=value, fill = betas)) +
  geom_density_ridges() +
  scale_fill_viridis_d("Coeffs", guide = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = 0, size = 0.5) +
  labs(y="Effect Sizes", x="Value") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12))




#### Create Predictive Surfaces (ndvi) ####

#Load data to plot tracks
setwd("~/Documents/Snail Kite Project/Data/R Scripts/acceleration")
dat<- read.csv('Giant Armadillo state estimates.csv', as.is = T)
dat$date<- as_datetime(dat$date, tz = "UTC")
dat<-  dat %>% 
  rename(x = easting, y = northing) %>% 
  mutate(across(c('z.map','z.post.thresh','z.post.max'), factor,
                levels = c("Slow-Turn","Slow-Unif","Exploratory","Transit","Unclassified"))
  )

dat$month<- month.abb[month(dat$date)]
dat$month<- factor(dat$month, levels = month.abb[c(5:12,1)])
dat$season<- ifelse(dat$month %in% month.abb[1:7], "Flood", "Dry")


#extract beta coeffs (mean)
betas<- colMeans(store.betas)

#Need to center and scale raster values so comparable to beta coeffs

#Load env raster data
## NDVI
setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist_avg")
ndvi<- brick('GiantArm_ndvi_season.grd')
ndvi<- crop(ndvi, extent(dat %>% 
                           summarize(xmin = min(x) - 3000,
                                     xmax = max(x) + 3000,
                                     ymin = min(y) - 3000,
                                     ymax = max(y) + 3000) %>% 
                           unlist()))

ndvi.s<- scale(ndvi, center = T, scale = T)


## AWEI

awei<- brick('GiantArm_awei_season.grd')
awei<- crop(awei, ndvi)
awei.s<- scale(awei, center = T, scale = T)


# Mask all unused pixels
# ind<- unique(cellFromXY(ndvi.s, dat[, c("x","y")]))
# ndvi.s_masked<- ndvi.s
# ndvi.s_masked[setdiff(1:ncell(ndvi.s_masked), ind_N)] <- NA


##Perform raster math using beta coeffs
resistSurf_flood<- exp(
    betas["int"] + 
    betas["ndvi"]*ndvi.s$Flood +  #for Flood
    betas["awei"]*awei.s$Flood  #for Flood
)
resistSurf_flood.df<- as.data.frame(resistSurf_flood, xy=T) %>% 
  mutate(season = "Flood")

resistSurf_dry<- exp(
  betas["int"] + 
    betas["ndvi"]*ndvi.s$Dry +  #for Dry
    betas["awei"]*awei.s$Dry  #for Dry
)
resistSurf_dry.df<- as.data.frame(resistSurf_dry, xy=T) %>% 
  mutate(season = "Dry")




#Combine all results together for each season
resistSurf.df<- rbind(resistSurf_flood.df, resistSurf_dry.df)


## Map predictive surfaces
ggplot() +
  geom_tile(data = resistSurf.df, aes(x, y, fill = layer)) +
  scale_fill_viridis_c("Time Spent\nper Cell (min)", option = "inferno",
                       na.value = "transparent") +
  # geom_point(data = dat.N %>% filter(state == "Foraging"), aes(x, y, color = id),
  #            size = 0.5, alpha = 0.2, show.legend = F) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing") +
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
  facet_wrap(~ season)
