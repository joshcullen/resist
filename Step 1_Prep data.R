### Load armadillo telemetry data

library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(sp)

source('helper functions.R')

###################
### Import data ###
###################

setwd("~/Documents/Snail Kite Project/Data/armadillos")

dat<- read.csv("Modified Armadillo Data.csv", header = T, sep = ",")

dat$id<- as.character(dat$id)
dat$date<- as_datetime(dat$date)

# Separate tracks by region (N or S)
dat.N<- dat %>% filter(region == "N")
dat.S<- dat %>% filter(region == "S")


# Read in rasters
setwd("~/Documents/Snail Kite Project/Data/armadillos/Environ Data")

rast<- dir(getwd(), "*.tif$")
for (i in rast) assign(i, raster(i))

plot(classes_DL_padrao.tif)
plot(EucDist_cerc_Copy.tif); points(dat.N$x, dat.N$y)
plot(EucDist_cercaAm.tif); points(dat.S$x, dat.S$y)

#Need to project rasters same as tracks
EucDist_cerc_Copy.tif<- projectRaster(EucDist_cerc_Copy.tif, crs = "+init=epsg:32721")
crs(EucDist_cercaAm.tif)<- crs(EucDist_cerc_Copy.tif)


## Aggregate spatial data to same scale (which will likely be 30m)
res(EucDist_cercaAm.tif) #1m 1m
dist2rdS_30m<- raster::aggregate(EucDist_cercaAm.tif,
                                 fact = 30,
                                 fun = mean)

res(EucDist_cerc_Copy.tif) #13m 13m
#create dummy raster at same extent and proj, but with desired resolution
ex.ras<- raster(ext = extent(EucDist_cerc_Copy.tif), crs = "+init=epsg:32721", res = 30)
dist2rdN_30m<- resample(EucDist_cerc_Copy.tif, ex.ras, method = "bilinear")
# dist2rdN_30m<- raster::aggregate(EucDist_cerc_Copy.tif,
#                                  fact = 2,
#                                  fun = mean)


#######################################################
### Extract values from raster layer for each track ###
#######################################################

path.N<- extract.covars(dat.N, dist2rdN_30m, crs = "+init=epsg:32721")
path.S<- extract.covars(dat.S, dist2rdS_30m, crs = "+init=epsg:32721")

names(path.N)[3]<- "dist2rd"
names(path.S)[3]<- "dist2rd"


#double-check that extracted grid cells overlap with tracks

#N
bar<- dist2rdN_30m
bar[]<- NA
bar[path.N$cell]<- dist2rdN_30m[path.N$cell]
bar<- as.data.frame(bar, xy = T)
names(bar)[3]<- "dist"
# plot(bar)
# plot(st_as_sf(x = dat.N,                         
#                     coords = c("x", "y"),
#                     crs = "+init=epsg:32721"),
#      add = T)
ggplot() +
  geom_tile(data = bar, aes(x, y, fill = dist), na.rm = T) +
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "n") +
  geom_path(data = dat.N, aes(x, y, color = id)) +
  coord_cartesian() +
  theme_bw()

#S
bar<- dist2rdS_30m
bar[]<- NA
bar[path.S$cell]<- dist2rdS_30m[path.S$cell]
bar<- as.data.frame(bar, xy = T)
names(bar)[3]<- "dist"
# plot(bar)
# plot(st_as_sf(x = dat.S,                         
#                           coords = c("x", "y"),
#                           crs = "+init=epsg:32721"),
#      add = T)
ggplot() +
  geom_tile(data = bar, aes(x, y, fill = dist), na.rm = T) +
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "n") +
  # geom_path(data = dat.S, aes(x, y, color = id)) +
  coord_cartesian() +
  theme_bw()


##############################
### Modify and export data ###
##############################

# Adjust seg.id values for path.S
path.S<- df.to.list(path.S, "id")
for (i in 1:length(path.S)) {
  path.S[[i]]$seg.id<- path.S[[i]]$seg.id + max(path.N$seg.id)
}
path.S<- bind_rows(path.S)

# Merge datasets
resist.dat<- rbind(path.N, path.S)




# Export data
setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist")
# write.csv(resist.dat, "Armadillo Resistance Data.csv", row.names = F)
