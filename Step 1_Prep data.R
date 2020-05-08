### Load armadillo telemetry data

setwd("~/Documents/Snail Kite Project/Data/armadillos")

library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(sp)

###################
### Import data ###
###################

dat<- read.csv("three_banded_over20_for_Josh.csv", header = T, sep = ",")

dat$id<- as.character(dat$id)
dat$date<- as.POSIXct(strptime(dat$date, format = "%d/%m/%Y %H:%M"))
coords<- dat[c("x","y")]
dat.spdf<- SpatialPointsDataFrame(coords = coords, data = dat)
proj4string(dat.spdf)<- CRS("+init=epsg:32721")

dat.N<- dat[dat$y > 8100000,]
dat.S<- dat[dat$y < 8100000,]


### Read in rasters
setwd("~/Documents/Snail Kite Project/Data/armadillos/Environ Data")

rast <- dir(getwd(), "*.tif$")
for (i in rast) assign(i, raster(i))
plot(classes_DL_padrao.tif)
plot(EucDist_cerc_Copy.tif); points(dat.N$x, dat.N$y)
plot(EucDist_cercaAm.tif); points(dat.S$x, dat.S$y)

#Need to project rasters same as tracks
EucDist_cerc_Copy.tif<- projectRaster(EucDist_cerc_Copy.tif, crs = "+init=epsg:32721")
crs(EucDist_cercaAm.tif)<- crs(EucDist_cerc_Copy.tif)



# Convert DFs to linestrings by ID
dat.N_sf<- st_as_sf(dat.N, coords=c("x","y"), crs = "+init=epsg:32721") %>% 
  group_by(id) %>% 
  summarize(do_union = FALSE) %>% 
  st_cast("LINESTRING")
plot(dat.N_sf)

dat.S_sf<- st_as_sf(dat.S, coords=c("x","y"), crs = "+init=epsg:32721") %>% 
  group_by(id) %>% 
  summarize(do_union = FALSE) %>% 
  st_cast("LINESTRING")
plot(dat.S_sf)


## Aggregate spatial data to same scale (which will likely be 30m)
res(EucDist_cerc_Copy.tif) #13m 13m
dist2rdN_30m<- raster::aggregate(EucDist_cerc_Copy.tif,
                                 fact = 2,
                                 fun = mean)

res(EucDist_cercaAm.tif) #1m 1m
dist2rdS_30m<- raster::aggregate(EucDist_cercaAm.tif,
                                 fact = 30,
                                 fun = mean)


## Extract values from raster layer for each track

#N
path_N<- raster::extract(dist2rdN_30m, dat.N_sf, along = TRUE, cellnumbers = TRUE)
path_N_df = purrr::map_dfr(path_N, as_data_frame, .id = "id")
names(path_N_df)[3]<- "dist2rd"
path_N_coords = xyFromCell(dist2rdN_30m, path_N_df$cell)
# pair_dist = geosphere::distGeo(path_coords)[-nrow(path_coords)]
# transect_df$dist = c(0, cumsum(pair_dist)) 

#double-check that extracted grid cells overlap with tm14 path
bar<- dist2rdN_30m
bar[]<- NA
bar[path_N_df$cell]<- dist2rdN_30m[path_N_df$cell]
plot(bar)
plot(st_geometry(dat.N_sf), add=T)


#S
path_S<- raster::extract(dist2rdS_30m, dat.S_sf, along = TRUE, cellnumbers = TRUE)
path_S_df = purrr::map_dfr(path_S, as_data_frame, .id = "id")
names(path_S_df)[3]<- "dist2rd"
path_S_coords = xyFromCell(dist2rdS_30m, path_S_df$cell)
# pair_dist = geosphere::distGeo(path_coords)[-nrow(path_coords)]
# transect_df$dist = c(0, cumsum(pair_dist)) 

#double-check that extracted grid cells overlap with tm14 path
bar<- dist2rdS_30m
bar[]<- NA
bar[path_S_df$cell]<- dist2rdS_30m[path_S_df$cell]
plot(bar)
plot(st_geometry(dat.S_sf), add=T)




#Label all observations with cell number and time interval
tm14$cell<- cellFromXY(dist2rdN_30m, tm14[,c("x","y")])
tm14$dt<- tm14$date %>% seconds() %>% diff() %>% c(NA,.)

#Determine which cells (chronologically) are associated with those from path_df
ind<- vector()
for (i in 2:(nrow(path_df) - 1)) {
  if (path_df$cell[i] == path_df$cell[i+1])
    ind<- c(ind, i)
}
ind<- c(1, ind, nrow(path_df))

#Add time index to both DFs for merging
path_df$time1<- 1:nrow(path_df)
tm14$time1<- ind

#Merge datasets
resist.dat<- left_join(path_df, tm14, by = 'time1')

#Add seg.id column
resist.dat$seg.id<- NA
resist.dat$seg.id[1]<- 1  #need to define for 1st obs
for (i in 2:length(ind)){
  seq1=(ind[i-1] + 1):ind[i]
  resist.dat$seg.id[seq1]=i-1
}


# Export data
setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist")
# write.csv(resist.dat, "Armadillo Resistance Data.csv", row.names = F)
