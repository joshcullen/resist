### Visualize Results from Best Models ###

library(ggridges)
library(raster)
library(lubridate)

#look at betas (convert to data frame)
store.betas<- data.frame(mod$betas[(nburn+1):ngibbs, ])
names(store.betas)<- c("Jun","Sep","Oct","Nov",paste("spline", 1:ncol(spline.evi), sep = "."))
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

dat.em<-  dat %>% 
  filter(id == "emanuel")

#extract beta coeffs (mean)
betas<- colMeans(store.betas)

#Need to center and scale raster values so comparable to beta coeffs

#Load env raster data
## EVI
setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist_avg")
evi<- brick('GiantArm_evi_monthly.grd')
evi.s<- scale(evi, center = T, scale = T)

evi.s<- crop(evi.s, extent(dat.em %>% 
                           summarize(xmin = min(x) - 3000,
                                     xmax = max(x) + 3000,
                                     ymin = min(y) - 3000,
                                     ymax = max(y) + 3000) %>% 
                           unlist()))




##Perform raster math using beta coeffs
resistSurf<- list()
evi.s2<- evi.s[[which(names(evi.s) %in% unique(path.s$month))]]

#adjust month-based intercepts
# betas2<- betas
# betas2[2:5]<- betas2[1] + betas2[2:5]

for (i in 1:length(unique(path.s$month))) {
  resistSurf[[i]]<- exp(
    ifelse(i == 1, 0, betas[i-1]) + 
    betas["spline.1"]*evi.s2[[i]] +
    betas["spline.2"]*evi.s2[[i]] +
    betas["spline.3"]*evi.s2[[i]] +
    betas["spline.4"]*evi.s2[[i]]
    )
}
resistSurf<- stack(resistSurf)
names(resistSurf)<- names(evi.s2)

resistSurf.df<- as.data.frame(resistSurf, xy=T) %>% 
  pivot_longer(cols = -c(x,y), names_to = "month", values_to = "time")
  # mutate(month = rep(month.abb[c(5:12,1)], each = ncell(ndwi$May)))
resistSurf.df$month<- factor(resistSurf.df$month, levels = names(evi.s2))

# resistSurf_dry<- exp(
#   betas["int"] + 
#     betas["ndvi"]*ndvi.s$Dry +  #for Dry
#     betas["awei"]*awei.s$Dry  #for Dry
# )
# resistSurf_dry.df<- as.data.frame(resistSurf_dry, xy=T) %>% 
#   mutate(season = "Dry")




#Combine all results together for each season
# resistSurf.df<- rbind(resistSurf_flood.df, resistSurf_dry.df)


## Map predictive surfaces
ggplot() +
  geom_tile(data = resistSurf.df, aes(x, y, fill = time)) +
  scale_fill_viridis_c("Time Spent\nper Cell (min)", option = "inferno",
                       na.value = "transparent", limits = c(0,10)) +
  geom_point(data = dat.em, aes(x, y),
             size = 0.5, alpha = 0.5, show.legend = F, color = "chartreuse") +
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
  facet_wrap(~ month)
