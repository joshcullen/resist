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

#active locs from standard HMM (via momentuHMM)
dat<- read.csv("Armadillo HMM Results.csv", header = T, sep = ",")

dat<- dat %>% 
  rename(id = ID) %>% 
  mutate_at("id", as.character) %>% 
  mutate_at("state", as.factor) %>% 
  mutate_at("state", ~recode(., '1' = "Burrow",
                                '2' = "Foraging", '3' = "Transit"))
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

# Create dummy rasters for extent of each region (with buffer) to crop down rasters

#North
rast.N<- raster(ext=extent(c(min(dat.N$x) - 30, max(dat.N$x) + 30, min(dat.N$y) - 30,
                             max(dat.N$y + 30))),
                crs = "+init=epsg:32721",
                res = 30)
values(rast.N)<- 0
dist2rdN_30m<- crop(dist2rdN_30m, rast.N)
plot(dist2rdN_30m); points(dat.N$x, dat.N$y)

#South
rast.S<- raster(ext=extent(c(min(dat.S$x) - 30, max(dat.S$x) + 30, min(dat.S$y) - 30,
                             max(dat.S$y + 30))),
                crs = "+init=epsg:32721",
                res = 30)
values(rast.S)<- 0
dist2rdS_30m<- crop(dist2rdS_30m, rast.S)
plot(dist2rdS_30m); points(dat.S$x, dat.S$y)



#######################################
### Import DEMs and Calculate Slope ###
#######################################

#slope
slope.N = terrain(dem_N.tif, opt='slope', unit = "degrees")
slope.S = terrain(dem_S.tif, opt='slope', unit = "degrees")

#resample DEMs to 30m from 18m; they will now share the same dimensions and extent
slope.N<- resample(slope.N, dist2rdN_30m, method = "bilinear")
compareRaster(dist2rdN_30m, slope.N)  #check if same extent, dimensions, projection, resolution,
#and origin
plot(slope.N); points(dat.N$x, dat.N$y)

#resample DEMs to 30m from 18m; they will now share the same dimensions and extent
slope.S<- resample(slope.S, dist2rdS_30m, method = "bilinear")
compareRaster(dist2rdS_30m, slope.S)
plot(slope.S); points(dat.S$x, dat.S$y)



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
plot(ndvi.N[[1]]); points(dat.N$x, dat.N$y)

#change extent and dimensions of RasterBricks using resample()
ndvi.S<- resample(ndvi.S, dist2rdS_30m, method = "bilinear")
compareRaster(dist2rdS_30m, ndvi.S)
plot(ndvi.S[[1]]); points(dat.S$x, dat.S$y)


### *TEMPORARY* take mean NDVI for all rasters over study period at each site
ndvi.N.mean<- mean(ndvi.N, na.rm = T)
ndvi.S.mean<- mean(ndvi.S, na.rm = T)


###########################################
### Merge static covars as RasterBricks ###
###########################################

covars.N<- brick(dist2rdN_30m, slope.N, ndvi.N.mean)
names(covars.N)<- c("dist2rd", "slope", "ndvi")

covars.S<- brick(dist2rdS_30m, slope.S, ndvi.S.mean)
names(covars.S)<- c("dist2rd", "slope", "ndvi")


#######################################################
### Extract values from raster layer for each track ###
#######################################################
tic()
path.N<- extract.covars(dat.N, covars.N, "state")
toc()  #takes 18 min to run

tic()
path.S<- extract.covars(dat.S, covars.S, "state")
toc()  #take 5.5 min to run


##############################
### Add lunar illumination ###
##############################

path.N$lunar<- lunar.illumination(path.N$date, shift = 12)
path.S$lunar<- lunar.illumination(path.S$date, shift = 12)


##############################################
### Add temperature and rainfall by region ###
##############################################

setwd("~/Documents/Snail Kite Project/Data/armadillos/Environ Data")

#Load and wrangle data
temp<- read.csv("caceres_corumba_2014e15.csv", as.is = T)
temp<- temp %>% 
  mutate(date.round = as.POSIXct(strptime(paste(data, hora.utc), format = "%d-%m-%Y %H",
                                          tz = "UTC")),
         .before = everything())

#Create new col to store rounded datetimes
path.N<- path.N %>% 
  mutate(date.round = round_date(path.N$date, unit = "hour"), .before = state)
path.S<- path.S %>% 
  mutate(date.round = round_date(path.S$date, unit = "hour"), .before = state)

#Merge data
path.N<- left_join(path.N, temp[,c("date.round","t.ar","rain")], by = "date.round")
path.S<- left_join(path.S, temp[,c("date.round","t.ar","rain")], by = "date.round")


#############################################
### Explore relationships among variables ###
#############################################

PerformanceAnalytics::chart.Correlation(path.N[,c(2:4,10:12)])  #no strong corrs
PerformanceAnalytics::chart.Correlation(path.S[,c(2:4,10:12)])  #no strong corrs


###################
### Export data ###
###################

setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist")

# Armadillo Data
# write.csv(path.N, "N Armadillo Resistance Data.csv", row.names = F)
# write.csv(path.S, "S Armadillo Resistance Data.csv", row.names = F)


# Environmental Layers
#North
# writeRaster(dist2rdN_30m, filename='dist2rd_N.tif', format="GTiff", overwrite=TRUE)
# writeRaster(slope.N, filename='slope_N.tif', format="GTiff", overwrite=TRUE)
# writeRaster(ndvi.N.mean, filename='ndvi_N.tif', format="GTiff", overwrite=TRUE)
# 
# #South
# writeRaster(dist2rdS_30m, filename='dist2rd_S.tif', format="GTiff", overwrite=TRUE)
# writeRaster(slope.S, filename='slope_S.tif', format="GTiff", overwrite=TRUE)
# writeRaster(ndvi.S.mean, filename='ndvi_S.tif', format="GTiff", overwrite=TRUE)
