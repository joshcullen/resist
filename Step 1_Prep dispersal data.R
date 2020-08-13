### Load armadillo telemetry data

library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(sp)
library(tictoc)
library(lunar)

source('helper functions.R')

###################
### Import data ###
###################

#dispersal locs from Bayesian HMM
dat<- read.csv("assigned Z MAP.csv", header = T, sep = ",")

dat$id<- as.character(dat$id)
dat$date<- as_datetime(dat$date)

# Separate tracks by region (N or S)
dat.N<- dat %>% filter(region == "N")
dat.S<- dat %>% filter(region == "S")


####################################
### Import distance-to-road data ###
####################################

setwd("~/Documents/Snail Kite Project/Data/armadillos/Environ Data")

rast<- dir(getwd(), "*.tif$")
for (i in rast) assign(i, raster(i))

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




###################
### Import DEMs ###
###################

#resample DEMs to 30m from 18m; they will now share the same dimensions and extent
dem.N<- resample(dem_N.tif, dist2rdN_30m, method = "bilinear")
compareRaster(dist2rdN_30m, dem.N)  #check if same extent, dimensions, projection, resolution,
#and origin
plot(dist2rdN_30m); plot(dem.N, add=T); points(dat.N$x, dat.N$y)

#resample DEMs to 30m from 18m; they will now share the same dimensions and extent
dem.S<- resample(dem_S.tif, dist2rdS_30m, method = "bilinear")
compareRaster(dist2rdS_30m, dem.S)
plot(dist2rdS_30m); plot(dem.S, add=T); points(dat.S$x, dat.S$y)



##########################
### Import NDVI layers ###
##########################

setwd("~/Documents/Snail Kite Project/Data/armadillos/NDVI")

#load files
ndvi.filenames<- list.files(getwd(), pattern = "*.grd$")
ndvi.N<- brick(ndvi.filenames[1])
ndvi.S<- brick(ndvi.filenames[2])

#change extent and dimensions of RasterBricks using resample()
ndvi.N<- resample(ndvi.N, dist2rdN_30m, method = "bilinear")
compareRaster(dist2rdN_30m, ndvi.N)
plot(dist2rdN_30m); plot(ndvi.N[[1]], add=T); points(dat.N$x, dat.N$y)

#change extent and dimensions of RasterBricks using resample()
ndvi.S<- resample(ndvi.S, dist2rdS_30m, method = "bilinear")
compareRaster(dist2rdS_30m, ndvi.S)
plot(dist2rdS_30m); plot(ndvi.S[[1]], add=T); points(dat.S$x, dat.S$y)


### *TEMPORARY* take mean NDVI for all rasters over study period at each site
ndvi.N.mean<- mean(ndvi.N, na.rm = T)
ndvi.S.mean<- mean(ndvi.S, na.rm = T)


###########################################
### Merge static covars as RasterBricks ###
###########################################

covars.N<- brick(dist2rdN_30m, dem.N, ndvi.N.mean)
names(covars.N)<- c("dist2rd", "elev", "ndvi")

covars.S<- brick(dist2rdS_30m, dem.S, ndvi.S.mean)
names(covars.S)<- c("dist2rd", "elev", "ndvi")


#######################################################
### Extract values from raster layer for each track ###
#######################################################
tic()
path.N<- extract.covars(dat.N, covars.N)
toc()

tic()
path.S<- extract.covars(dat.S, covars.S)
toc()

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
### Add lunar illumination ###
##############################

path.N$lunar<- lunar.illumination(path.N$date, shift = 12)
path.S$lunar<- lunar.illumination(path.S$date, shift = 12)


#############################################
### Explore relationships among variables ###
#############################################

PerformanceAnalytics::chart.Correlation(path.N[,c(2:4,8)])  #no strong corrs
PerformanceAnalytics::chart.Correlation(path.S[,c(2:4,8)])  #strong corr between dist2rd and elev


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

# write.csv(path.N, "N Armadillo Resistance Data_dispersal.csv", row.names = F)
# write.csv(path.S, "S Armadillo Resistance Data_dispersal.csv", row.names = F)

