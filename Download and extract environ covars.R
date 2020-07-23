# Download and extract DEMs for North and South study sites in Pantanal

library(elevatr)
library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(sp)


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


# Create dummy rasters for extent of each region (with buffer)

#North
rast.N<- raster(ext=extent(c(min(dat.N$x) - 30, max(dat.N$x) + 30, min(dat.N$y) - 30,
                             max(dat.N$y + 30))),
                crs = "+init=epsg:32721",
                res = 30)
values(rast.N)<- 0

#South
rast.S<- raster(ext=extent(c(min(dat.S$x) - 30, max(dat.S$x) + 30, min(dat.S$y) - 30,
                             max(dat.S$y + 30))),
                crs = "+init=epsg:32721",
                res = 30)
values(rast.S)<- 0


#check that points fit
plot(rast.N)
points(dat.N$x, dat.N$y)

plot(rast.S)
points(dat.S$x, dat.S$y)



###############################
### Download elevation data ###
###############################

dem.N<- get_elev_raster(rast.N, z = 12) #18 m res
dem.N<- crop(dem.N, rast.N)
plot(dem.N)
points(dat.N$x, dat.N$y)


dem.S<- get_elev_raster(rast.S, z = 12) #18 m res
dem.S<- crop(dem.S, rast.S)
plot(dem.S)
points(dat.S$x, dat.S$y)



# Visualize different features using elev (i.e., slope, aspect, hillshade)

#North
slope.N = terrain(dem.N, opt='slope')
aspect.N = terrain(dem.N, opt='aspect')
hill.N = hillShade(slope.N, aspect.N, 40, 270)

#plot elev
plot(dem.N, main="DEM for North", col=terrain.colors(25,alpha=0.7))
#plot slope
plot(slope.N, main="Slope for North", col=topo.colors(25,alpha=0.7))
#plot aspect
plot(aspect.N, main="Aspect for North", col=rainbow(25,alpha=0.7))
#plot DEM w/ hillshade
plot(hill.N,
     col=grey(1:100/100),  # create a color ramp of grey colors for hillshade
     legend=FALSE,         # no legend, we don't care about the grey of the hillshade
     main="DEM for North",
     axes=FALSE)           # makes for a cleaner plot, if the coordinates aren't necessary
plot(dem.N, 
     axes=FALSE,
     col=terrain.colors(12, alpha=0.35), add=TRUE)



#South
slope.S = terrain(dem.S, opt='slope')
aspect.S = terrain(dem.S, opt='aspect')
hill.S = hillShade(slope.S, aspect.S, 40, 270)

#plot elev
plot(dem.S, main="DEM for South", col=terrain.colors(25,alpha=0.7))
#plot slope
plot(slope.S, main="Slope for South", col=topo.colors(25,alpha=0.7))
#plot aspect
plot(aspect.S, main="Aspect for South", col=rainbow(25,alpha=0.7))
#plot DEM w/ hillshade
plot(hill.S,
     col=grey(1:100/100),  # create a color ramp of grey colors for hillshade
     legend=FALSE,         # no legend, we don't care about the grey of the hillshade
     main="DEM for South",
     axes=FALSE)           # makes for a cleaner plot, if the coordinates aren't necessary
plot(dem.S, 
     axes=FALSE,
     col=terrain.colors(12, alpha=0.35), add=TRUE)




###############################################
### Write and save rasters as geoTIFF files ###
###############################################

setwd("~/Documents/Snail Kite Project/Data/armadillos/Environ Data")

#North
writeRaster(dem.N, filename='dem_N.tif', format="GTiff", overwrite=TRUE)

#South
writeRaster(dem.S, filename='dem_S.tif', format="GTiff", overwrite=TRUE)
