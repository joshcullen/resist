### Visualize Results from Best Models ###

library(tidyverse)
library(ggridges)
library(raster)
library(lubridate)
library(splines)


### Load data ###

path<- read.csv("Emanuel Resistance Data.csv", as.is = T)
path$dt<- path$dt/60  #convert to min from sec
path$month<- month.abb[month(path$date)]
path$month<- factor(path$month, levels = month.abb[c(5:12,1)])

# Filter data for only steps with 6 >= dt >= 8 min
cond<- path[path$dt >= 6 & path$dt <= 8 & !is.na(path$dt), "seg.id"]
path<- path[path$seg.id %in% cond,]

store.betas<- read.csv("Giant Armadillo Resistance Results.csv", as.is = T)

#look only at betas
store.betas<- store.betas[,-1]
store.betas.long<- tidyr::pivot_longer(store.betas,
                                               cols = names(store.betas),
                                               names_to = "betas")
store.betas.long$betas<- factor(store.betas.long$betas, levels = names(store.betas))

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




#### Create Predictive Surfaces (EVI) ####

#Load data to plot tracks
setwd("~/Documents/Snail Kite Project/Data/R Scripts/acceleration")
dat<- read.csv('Giant Armadillo state estimates.csv', as.is = T)
dat$date<- as_datetime(dat$date, tz = "UTC")
dat<- dat %>% 
  rename(x = easting, y = northing) %>% 
  mutate(across(c('z.map','z.post.thresh','z.post.max'), factor,
                levels = c("Slow-Turn","Slow-Unif","Exploratory","Transit","Unclassified"))
  )

dat$month<- month.abb[month(dat$date)]
dat$month<- factor(dat$month, levels = month.abb[c(5:12,1)])
# dat$season<- ifelse(dat$month %in% month.abb[1:7], "Flood", "Dry")

dat.em<-  dat %>% 
  filter(id == "emanuel")

#extract beta coeffs (mean)
# betas<- colMeans(store.betas)

#Need to center and scale raster values so comparable to beta coeffs

#Load env raster data
## EVI
setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist_avg")
evi<- brick('GiantArm_evi_monthly.grd')
evi<- crop(evi, extent(dat %>% 
                         summarize(xmin = min(x) - 3000,
                                   xmax = max(x) + 3000,
                                   ymin = min(y) - 3000,
                                   ymax = max(y) + 3000) %>% 
                         unlist()))
evi[getValues(evi) > 1 | getValues(evi) < -1]<- NA  #mask pixels where values are outside of accepted range
evi.s<- scale(evi)
evi.s<- crop(evi.s, extent(dat.em %>% 
                           summarize(xmin = min(x) - 3000,
                                     xmax = max(x) + 3000,
                                     ymin = min(y) - 3000,
                                     ymax = max(y) + 3000) %>% 
                           unlist()))
# evi.s[getValues(evi.s) > max(path$evi) | getValues(evi.s) < min(path$evi)]<- NA  #mask pixels where values are outside of accepted range


##Perform raster math using beta coeffs
evi.s2<- evi.s[[which(names(evi.s) %in% unique(dat.em$month))]]

#Run splines on standardized sequence
rango1<- range(path$evi)
knot.locs<- seq(rango1[1], rango1[2], length.out = 4)[2:3]
spline.evi<- bs(values(evi.s2), degree=2, intercept = TRUE, knots = knot.locs)

month.dumm<- factor(rep(unique(dat.em$month), each = ncell(evi.s2)), levels = unique(dat.em$month))
month.dumm<- model.matrix(~month.dumm + 0)
design.mat<- cbind(month.dumm[,-1], spline.evi)

#create list where months are separated into different elements
design.mat.list<- list()
oo = 1
for (i in 1:nlayers(evi.s2)) {
  design.mat.list[[i]]<- design.mat[oo:(oo - 1 + ncell(evi.s2)),]
  oo = oo + ncell(evi.s2)
}




predict.speed<- function(dat.list, betas) {
  tictoc::tic()
  
  #progress bar
  pb<- progress::progress_bar$new(
    format = " iteration (:current/:total) [:bar] :percent [Elapsed: :elapsed, Remaining: :eta]",
    total = nrow(betas), clear = FALSE, width = 100)
  tmp<- matrix(NA, nrow = nrow(betas), ncol = nrow(dat.list))
  
  for (i in 1:nrow(betas))  {
    tmp[i,]<- exp(dat.list %*% t(betas[i,]))
    
    pb$tick()  #create progress bar
  }
  
  resistVals<- data.frame(mean = colMeans(tmp, na.rm = T),
                          sd = matrixStats::colSds(tmp)  #much faster than apply()
                          )
  tictoc::toc()
  
  resistVals
}



resistVals<- map(design.mat.list, ~predict.speed(., store.betas))

resistSurf.mean<- resistSurf.sd<- evi.s2
values(resistSurf.mean)<- map(resistVals, pluck, "mean") %>% unlist()
values(resistSurf.sd)<- map(resistVals, pluck, "sd") %>% unlist()


resistSurf.mean.df<- as.data.frame(resistSurf.mean, xy=T) %>% 
  pivot_longer(cols = -c(x,y), names_to = "month", values_to = "time") %>% 
  mutate(type = "Mean")
resistSurf.mean.df$month<- factor(resistSurf.mean.df$month, levels = names(evi.s2))

resistSurf.sd.df<- as.data.frame(resistSurf.sd, xy=T) %>% 
  pivot_longer(cols = -c(x,y), names_to = "month", values_to = "time") %>% 
  mutate(type = "SD")
resistSurf.sd.df$month<- factor(resistSurf.sd.df$month, levels = names(evi.s2))

resistSurf.df<- rbind(resistSurf.mean.df, resistSurf.sd.df)


## Map predictive surfaces

#mean
ggplot() +
  geom_raster(data = resistSurf.mean.df, aes(x, y, fill = time)) +
  scale_fill_viridis_c("Time Spent\nper Cell (min)", option = "inferno",
                       na.value = "transparent") +
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

#SD
ggplot() +
  geom_tile(data = resistSurf.sd.df, aes(x, y, fill = time)) +
  scale_fill_viridis_c("Time Spent\nper Cell (min)", option = "viridis",
                       na.value = "transparent") +
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




##################################
### Export prediction surfaces ###
##################################
setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist")

# write.csv(resistSurf.df, "Giant Armadillo Resistance Surfaces.csv", row.names = F)
