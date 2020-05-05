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

dat$id<- as.factor(dat$id)
dat$date<- as.POSIXct(strptime(dat$date, format = "%d/%m/%Y %H:%M"))
coords<- dat[c("x","y")]
dat.spdf<- SpatialPointsDataFrame(coords = coords, data = dat)
proj4string(dat.spdf)<- CRS("+init=epsg:32721")

dat.N<- dat[dat$y > 8100000,]
dat.S<- dat[dat$y < 8100000,]


### Read in rasters
rast <- dir(getwd(), "*.tif$")
for (i in rast) assign(i, raster(i))
plot(classes_DL_padrao.tif)
plot(EucDist_cerc_Copy.tif); points(dat.N$x, dat.N$y)
plot(EucDist_cercaAm.tif); points(dat.S$x, dat.S$y)

plot(EucDist_cerc_Copy.tif); points(dat.N$x, dat.N$y)
dat.N %>% group_by(id) %>% tally()
## Stick with tm14 from northern site using EucDist_cerc_Copy.tif layer



tm14<- dat.N %>% filter(id == "tm14")
tm14.line<- tm14 %>%
  dplyr::select(x,y) %>% 
  as.matrix() %>% 
  st_linestring() %>% 
  st_sfc(crs = projection(EucDist_cerc_Copy.tif)) %>% 
  st_sf()

#Change res of EucDist layer closer to 30 m (26 m) fomr 13 m
dist2rdN_30m<- raster::aggregate(EucDist_cerc_Copy.tif,
                                 fact = 2,
                                 fun = mean)


path<- raster::extract(dist2rdN_30m, tm14.line, along = TRUE, cellnumbers = TRUE)
path_df = purrr::map_dfr(path, as_data_frame, .id = "ID")
path_coords = xyFromCell(dist2rdN_30m, path_df$cell)
# pair_dist = geosphere::distGeo(path_coords)[-nrow(path_coords)]
# transect_df$dist = c(0, cumsum(pair_dist)) 

#double-check that extracted grid cells overlap with tm14 path
bar<- dist2rdN_30m
bar[]<- NA
bar[path_df$cell]<- dist2rdN_30m[path_df$cell]
plot(bar)
plot(st_geometry(tm14.line), add=T)

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
