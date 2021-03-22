### Load armadillo telemetry data

library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(sp)
library(furrr)
library(future)
library(progressr)

source('helper functions.R')

#############################
### Import armadillo data ###
#############################

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
dat$season<- factor(dat$season, levels = c("Flood","Dry"))


# Filter for Emanuel
dat.em<- dat %>% 
  filter(id == "emanuel")



#######################################
### Import Environmental Covariates ###
#######################################

setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist_avg")

## EVI

evi<- brick('GiantArm_evi_monthly.grd')
evi<- crop(evi, extent(dat.em %>% 
                                 summarize(xmin = min(x) - 3000,
                                           xmax = max(x) + 3000,
                                           ymin = min(y) - 3000,
                                           ymax = max(y) + 3000) %>% 
                                 unlist()))
evi[getValues(evi) > 1 | getValues(evi) < -1]<- NA  #mask pixels where values are outside of accepted range
# names(evi)<- month.abb[c(5:12,1)]
evi.s<- scale(evi)


# View distribs of EVI by month
evi.df<- as.data.frame(evi, xy = T)
evi.df<- pivot_longer(evi.df, cols = -c(x,y), names_to = "month", values_to = "evi") %>% 
  mutate_at("month", factor, levels = names(evi))

#density plot
ggplot(evi.df, aes(evi, fill = month)) +
  geom_density(alpha = 0.5) +
  xlim(0,1) +
  labs(x = "EVI", y = "Density") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

#boxplot
ggplot(evi.df, aes(evi, month, color = month)) +
  geom_boxplot(alpha = 0.5) +
  # xlim(0,1) +
  labs(x = "EVI", y = "Month") +
  scale_color_viridis_d(guide = F) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

#map
ggplot(evi.df %>% filter(month %in% month.abb[c(5:6,9:11)]), aes(x, y, fill = evi)) +
  geom_tile() +
  labs(x = "Easting", y = "Northing") +
  scale_fill_gradientn("EVI",
                       colors = c('#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718',
                               '#74A901', '#66A000', '#529400', '#3E8601', '#207401', '#056201',
                               '#004C00', '#023B01', '#012E01', '#011D01', '#011301'),
                    na.value = "transparent", limits = c(0,1)) +
  coord_equal() +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12)) +
  facet_wrap(~ month)


## LULC
setwd("~/Documents/Snail Kite Project/Data/R Scripts/acceleration")


flood.lulc<- raster('cheiann_UTM1.tif')
names(flood.lulc)<- 'lulc'

#crop raster closer to armadillo relocs
flood.lulc<- crop(flood.lulc, extent(dat.em %>% 
                                       summarize(xmin = min(x) - 3000,
                                                 xmax = max(x) + 3000,
                                                 ymin = min(y) - 3000,
                                                 ymax = max(y) + 3000) %>% 
                                       unlist()))
flood.lulc.df<- as.data.frame(flood.lulc, xy = TRUE)


dry.lulc<- raster('secann_UTM1.tif')
names(dry.lulc)<- 'lulc'
dry.lulc<- crop(dry.lulc, flood.lulc)
dry.lulc.df<- as.data.frame(dry.lulc, xy = TRUE)


lulc<- rbind(dry.lulc.df, flood.lulc.df)
lulc$season<- rep(c("Dry","Flood"), each = nrow(dry.lulc.df))


ggplot() +
  geom_raster(data = lulc, aes(x, y, fill = factor(lulc))) +
  scale_fill_manual("", values = c("darkgreen","burlywood4","darkolivegreen3","lightskyblue1"),
                    na.value = "transparent",
                    labels = c("Forest", "Closed Savanna", "Open Savanna", "Floodable","")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x="Easting", y="Northing") +
  theme_bw() +
  coord_equal() +
  theme(legend.position = "right",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold")) +
  facet_wrap(~ season)



### Extract Values of EVI by LULC Class (by season)
evi.red<- evi[[which(names(evi) %in% month.abb[c(5:6,9:11)])]]  #only select months w data for emanuel

# flood.evi<- evi.red[[which(names(evi.red) %in% month.abb[c(10:12,1:3)])]]
flood.lulc.list<- list()

for (i in 1:max(getValues(flood.lulc), na.rm = T)) {
  
  ind<- rasterToPoints(flood.lulc, fun=function(x){x == i}) 
  # ind<- which(getValues(flood.lulc) == i)
  
  flood.lulc.list[[i]]<- data.frame(extract(evi.red, ind[,1:2]), class = i)
}

flood.lulc.contvals<- bind_rows(flood.lulc.list)
flood.lulc.contvals$class<- factor(flood.lulc.contvals$class)
levels(flood.lulc.contvals$class)<- c("Forest", "Closed Savanna", "Open Savanna", "Floodable")
flood.lulc.contvals<- flood.lulc.contvals %>% 
  pivot_longer(cols = -class, names_to = "month", values_to = "value")



# dry.evi<- evi.red[[which(names(evi.red) %in% month.abb[4:9])]]
# dry.lulc.list<- list()
# 
# for (i in 1:max(getValues(dry.lulc), na.rm = T)) {
#   
#   ind<- which(getValues(dry.lulc) == i)
#   
#   dry.lulc.list[[i]]<- data.frame(extract(dry.evi, ind), class = i)
# }
# 
# dry.lulc.contvals<- bind_rows(dry.lulc.list)
# dry.lulc.contvals$class<- factor(dry.lulc.contvals$class)
# levels(dry.lulc.contvals$class)<- c("Forest", "Closed Savanna", "Open Savanna", "Floodable")
# dry.lulc.contvals<- dry.lulc.contvals %>% 
#   pivot_longer(cols = -class, names_to = "month", values_to = "value")
# 
# #combine both sets of results
# lulc.contvals<- rbind(flood.lulc.contvals, dry.lulc.contvals)
# lulc.contvals$season<- ifelse(lulc.contvals$month %in% month.abb[4:9], "Dry", "Flood")

ggplot(data=flood.lulc.contvals, aes(x=value, color = class)) +
  geom_density() +
  scale_color_viridis_d("") +
  labs(x = "Value", y = "Density") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  facet_wrap(~ month)



## Create polygons for Forest class to overlay on EVI
flood.poly<- rasterToPolygons(flood.lulc, n = 4, dissolve = TRUE)
flood.poly2<- st_as_sf(flood.poly) %>% filter(lulc == 1)
flood.poly3<- st_cast(flood.poly2, "POLYGON")
flood.poly4<- flood.poly3[as.numeric(st_area(flood.poly3)) > 30000,]

dry.poly<- rasterToPolygons(dry.lulc, n = 4, dissolve = TRUE)
dry.poly2<- st_as_sf(dry.poly) %>% filter(lulc == 1)
dry.poly3<- st_cast(dry.poly2, "POLYGON")
dry.poly4<- dry.poly3[as.numeric(st_area(dry.poly3)) > 30000,]

### Plot outline of Forest over EVI maps
ggplot() +
  geom_tile(data = evi.df %>% filter(month %in% month.abb[c(5:6,9:11)]), aes(x, y, fill = evi)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_fill_gradientn("EVI",
                       colors = c('#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718',
                                  '#74A901', '#66A000', '#529400', '#3E8601', '#207401', '#056201',
                                  '#004C00', '#023B01', '#012E01', '#011D01', '#011301'),
                       na.value = "transparent", limits = c(0,1)) +
  geom_sf(data = flood.poly4, color = "black", fill = "transparent") +
  # geom_sf(data = dry.poly4, color = "red", fill = "transparent") +
  coord_sf() +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12)) +
  facet_wrap(~ month)





#######################################################
### Extract values from raster layer for each track ###
#######################################################
plan(multisession)

# progressr::with_progress({  #to print progress bar
  path<- extract.covars(data = dat.em, layers = evi.s, state.col = "z.post.thresh",
                        dyn_names = "evi", ind = "month")
# })
#takes 54 s to run

future:::ClusterRegistry("stop")  #close all threads and memory used



###################
### Export data ###
###################

setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist")

# Armadillo Data
# write.csv(path, "Emanuel Resistance Data.csv", row.names = F)
