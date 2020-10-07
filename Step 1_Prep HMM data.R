### Load armadillo telemetry data

library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(sp)
library(furrr)
library(future)

source('helper functions.R')

#############################
### Import armadillo data ###
#############################

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


#########################
### Import LU/LC data ###
#########################

setwd("~/Documents/Snail Kite Project/Data/armadillos/Environ Data")

rast<- dir(getwd(), "*.tif$")
for (i in rast) assign(i, raster(i))


### Modify LU/LC TIFFs 

#Extract names for each LU/LC type and group similar ones together
class.N<- classes_DL_padrao.tif@data@attributes[[1]]$SPRCLASSE
class.S<- classes_ST.tif@data@attributes[[1]]$SPRCLASSE
class.S<- c("Campo", NA, "Mata", "Agua", "Pasto", NA, "Estrada")

classes_DL_padrao.tif<- projectRaster(classes_DL_padrao.tif, crs = "+init=epsg:32721")
crs(classes_ST.tif)<- crs(classes_DL_padrao.tif)

#Viz rasters
plot(classes_DL_padrao.tif)
plot(classes_ST.tif)


#Simplify LU/LC values of Southern site to 5 instead of 7 values  (change all to )
classes_ST.tif[classes_ST.tif == 2] <- 3
classes_ST.tif[classes_ST.tif == 6] <- 3

#create custom function to downsample raster for factor
w_agg <- function(x, na.rm=TRUE, ...) {
  i <- is.na(x)
  if ((!na.rm) | all(i)) {
    return(NA)
  }
  x <- x[!i]
  y <- rep(x, weight[x])
  # to get the same results as with your function you need
  # sort, such that the ties are treated the same way
  # y <- sort(y)
  modal(y)
}

res(classes_DL_padrao.tif) #1m 1m
weight<- c(1, 3, 10, 3, 3, 3)  # modify weighting so that Pasto doesn't dominate aggregation
                               # emphasize classification of roads by weighting highest
lulcN_30m<- raster::aggregate(classes_DL_padrao.tif,
                              fact = 30,
                              fun = w_agg)
plot(lulcN_30m); points(dat.N$x, dat.N$y)


res(classes_ST.tif) #1m 1m
weight<- c(1, 1, 1, 3, 1, 1, 5)  # modify weighting so that Campo, Mata, and Pasto don't dominate
lulcS_30m<- raster::aggregate(classes_ST.tif,
                              fact = 30,
                              fun = w_agg)
plot(lulcS_30m); points(dat.S$x, dat.S$y)



# Create dummy rasters for extent of each region (with buffer) to crop down rasters

#North
rast.N<- raster(ext=extent(c(min(dat.N$x) - 30, max(dat.N$x) + 30, min(dat.N$y) - 30,
                             max(dat.N$y + 30))),
                crs = "+init=epsg:32721",
                res = 30)
values(rast.N)<- 0

lulcN_30m<- crop(lulcN_30m, rast.N)
plot(lulcN_30m); points(dat.N$x, dat.N$y)

#South
rast.S<- raster(ext=extent(c(min(dat.S$x) - 30, max(dat.S$x) + 30, min(dat.S$y) - 30,
                             max(dat.S$y + 30))),
                crs = "+init=epsg:32721",
                res = 30)
values(rast.S)<- 0

lulcS_30m<- crop(lulcS_30m, rast.S)
plot(lulcS_30m); points(dat.S$x, dat.S$y)



#######################################
### Import DEMs and Calculate Slope ###
#######################################

#slope
slope.N = terrain(dem_N.tif, opt='slope', unit = "degrees")
slope.S = terrain(dem_S.tif, opt='slope', unit = "degrees")

#resample DEMs to 30m from 18m; they will now share the same dimensions and extent
slope.N<- resample(slope.N, lulcN_30m, method = "bilinear")
compareRaster(lulcN_30m, slope.N)  #check if same extent, dimensions, projection, resolution,
#and origin
plot(slope.N); points(dat.N$x, dat.N$y)

#resample DEMs to 30m from 18m; they will now share the same dimensions and extent
slope.S<- resample(slope.S, lulcS_30m, method = "bilinear")
compareRaster(lulcS_30m, slope.S)
plot(slope.S); points(dat.S$x, dat.S$y)





###########################################
### Merge static covars as RasterBricks ###
###########################################

covars.N<- brick(lulcN_30m, slope.N)
names(covars.N)<- c("lulc", "slope")

covars.S<- brick(lulcS_30m, slope.S)
names(covars.S)<- c("lulc", "slope")


#######################################################
### Extract values from raster layer for each track ###
#######################################################
plan(multisession)
path.N<- extract.covars(data = dat.N, layers = covars.N, state.col = "state")
#takes 6 min to run

path.S<- extract.covars(data = dat.S, layers = covars.S, state.col = "state")
#take 1.5 min to run

future:::ClusterRegistry("stop")  #close all threads and memory used



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

PerformanceAnalytics::chart.Correlation(path.N[,c(3,9:10)])  #no strong corrs
PerformanceAnalytics::chart.Correlation(path.S[,c(3,9:10)])  #no strong corrs


###################
### Export data ###
###################

setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist")

# Armadillo Data
# write.csv(path.N, "N Armadillo Resistance Data.csv", row.names = F)
# write.csv(path.S, "S Armadillo Resistance Data.csv", row.names = F)

# write.csv(path.N, "N Armadillo Resistance Data_LULC.csv", row.names = F)
# write.csv(path.S, "S Armadillo Resistance Data_LULC.csv", row.names = F)


# Environmental Layers
#North
# writeRaster(slope.N, filename='slope_N.tif', format="GTiff", overwrite=TRUE)
# writeRaster(lulcN_30m, filename='lulc_N.tif', format="GTiff", overwrite=TRUE)

# 
# #South
# writeRaster(slope.S, filename='slope_S.tif', format="GTiff", overwrite=TRUE)
# writeRaster(lulcS_30m, filename='lulc_S.tif', format="GTiff", overwrite=TRUE)
