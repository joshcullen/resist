### Import and extract environ covars ###

library(rgee)
library(sf)
library(raster)
library(rasterVis)

ee_Initialize()

#############################
### Import armadillo data ###
#############################

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


#################################################################
### Download data from Google Earth Engine for North Pantanal ###
#################################################################

# setwd("~/Documents/Snail Kite Project/Data/armadillos/NDVI/North Pantanal")
# 
# ndvi.N<- dir(getwd(), "*.tif$")
# for (i in ndvi.N) assign(i, raster(i))



bounds.N<- st_as_sf(data.frame(rasterToPoints(rast.N)), coords = c("x","y"), 
                  crs = "+init=epsg:32721") %>%  #convert to sf object
  st_bbox() %>%  #extract bounding box
  st_as_sfc() %>%  #convert into polygon
  sf_as_ee()  #convert into GEE format 





# This function gets NDVI from Landsat 8 imagery
addNDVI<- function(image) {
  return(image$addBands(image$normalizedDifference(c("B5", "B4"))$
                          rename('NDVI')))
}

# This functions masks clouds and cloud shadows (bits 3 and 5)
maskL8sr<- function(image) {
  cloudShadowBitMask<- bitwShiftL(1, 3)
  cloudsBitMask<- bitwShiftL(1, 5)
  qa<- image$select('pixel_qa')  # Get the pixel QA band
  # Both flags should be set to zero, indicating clear conditions
  mask<- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$
    And(qa$bitwiseAnd(cloudsBitMask)$eq(0))
  return(image$updateMask(mask))
}






# Retrieve Landsat 8-based NDVI
l8.N<- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$
  filterBounds(bounds.N)$
  filterDate('2015-07-01', '2015-08-31')$
  map(function(image){image$clip(bounds.N)})$
  sort("system:time_start", TRUE)$  # Sort the collection in chronological order
  map(function(x) x$reproject("EPSG:32721"))

print(l8.N$size()$getInfo())  #check number of images


# Map the NDVI and cloud mask functions; select only the NDVI band
l8.N_ndvi<- l8.N$map(addNDVI)$
  map(maskL8sr)$
  select('NDVI')


# Plot the map of the median NDVI for the region
Map$setCenter(-58.18579, -16.17027, 14)
Map$addLayer(l8.N_ndvi$median(),
             visParams = list(
               min = 0.0,
               max = 1.0,
               bands = "NDVI",
               palette = c(
                 'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
                 '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
                 '012E01', '011D01', '011301'
               )
             )) +
  Map$addLayer(bounds)



# Download all images locally
setwd("~/Documents/Snail Kite Project/Data/armadillos/NDVI/North Pantanal")

ndvi.N<- ee_imagecollection_to_local(
  ic = l8.N_ndvi,
  scale = 30,
  region = bounds.N$geometry(),
  via = 'getInfo'
)


ndvi.N.stack<- raster::stack(ndvi.N)

breaks<- seq(0, 1, by=0.01)
cols<- colorRampPalette(c('#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718',
                           '#74A901', '#66A000', '#529400', '#3E8601', '#207401', '#056201',
                           '#004C00', '#023B01', '#012E01', '#011D01', '#011301'
                           ))(length(breaks)-1)

##plot
rasterVis::levelplot(ndvi.N.stack, at=breaks, col.regions=cols, main="NDVI")




######################################
### Import data for South Pantanal ###
######################################

bounds.S<- st_as_sf(data.frame(rasterToPoints(rast.S)), coords = c("x","y"), 
                  crs = "+init=epsg:32721") %>%  #convert to sf object
  st_bbox() %>%  #extract bounding box
  st_as_sfc() %>%  #convert into polygon
  sf_as_ee()  #convert into GEE format 




# Retrieve Landsat 8-based NDVI
l8.S<- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$
  filterBounds(bounds.S)$
  filterDate('2014-11-01', '2015-03-31')$
  map(function(image){image$clip(bounds.S)})$
  sort("system:time_start", TRUE)$  # Sort the collection in chronological order
  map(function(x) x$reproject("EPSG:32721"))

print(l8.S$size()$getInfo())  #check number of images



l8.S_ndvi<- l8.S$map(addNDVI)$
  map(maskL8sr)$
  select('NDVI')


Map$setCenter(-57.48481, -18.30878, 13)
Map$addLayer(l8.S_ndvi$median(),
             visParams = list(
               min = 0.0,
               max = 1.0,
               bands = "NDVI",
               palette = c(
                 'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
                 '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
                 '012E01', '011D01', '011301'
               )
             )) +
  Map$addLayer(bounds.S)






# Download all images locally
setwd("~/Documents/Snail Kite Project/Data/armadillos/NDVI/South Pantanal")

ndvi.S<- ee_imagecollection_to_local(
  ic = l8.S_ndvi,
  scale = 30,
  region = bounds.S$geometry(),
  via = 'getInfo'
)


ndvi.S.stack<- raster::stack(ndvi.S)

##plot
rasterVis::levelplot(ndvi.S.stack, at=breaks, col.regions=cols, main="NDVI")





#####################################
### Load Landsat files from local ###
#####################################

## Northern Pantanal
setwd("~/Documents/Snail Kite Project/Data/armadillos/NDVI/North Pantanal")

#load files as raster brick
ndvi.N<- list.files(getwd(), pattern = "*.tif$")
ndvi.N.stack<- stack(ndvi.N)
ndvi.N.brick<- brick(ndvi.N.stack)

#change values x == 0 to NA (these are masked pixels)
values(ndvi.N.brick)[values(ndvi.N.brick) == 0] <- NA

#rename images by date
names(ndvi.N.brick)<- gsub(pattern = "LC08_22.071_", replacement = "", names(ndvi.N.brick))


#visualize original rasters
breaks<- seq(0, 1, by=0.01)
cols<- colorRampPalette(c('#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718',
                          '#74A901', '#66A000', '#529400', '#3E8601', '#207401', '#056201',
                          '#004C00', '#023B01', '#012E01', '#011D01', '#011301'
))(length(breaks)-1)
rasterVis::levelplot(ndvi.N.brick, at=breaks, col.regions=cols, main="NDVI")



#get the date from the names of the layers and extract the month
ind<- format(as.Date(names(ndvi.N.brick), format = "X%Y%m%d"), format = "%m") %>% 
  as.numeric()

#median of layers by month
ndvi.N.monthly<- stackApply(ndvi.N.brick, ind, fun = median)
names(ndvi.N.monthly)<- month.abb[7:8]

#plot aggregated data
rasterVis::levelplot(ndvi.N.monthly, at=breaks, col.regions=cols, main="NDVI")








## Southern Pantanal
setwd("~/Documents/Snail Kite Project/Data/armadillos/NDVI/South Pantanal")

#load files as raster brick
ndvi.S<- list.files(getwd(), pattern = "*.tif$")
ndvi.S.stack<- stack(ndvi.S)
ndvi.S.brick<- brick(ndvi.S.stack)

#change values x == 0 to NA (these are masked pixels)
values(ndvi.S.brick)[values(ndvi.S.brick) == 0] <- NA

#rename images by date
names(ndvi.S.brick)<- gsub(pattern = "LC08_227073_", replacement = "", names(ndvi.S.brick))


#visualize original rasters
breaks<- seq(0, 1, by=0.01)
cols<- colorRampPalette(c('#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718',
                          '#74A901', '#66A000', '#529400', '#3E8601', '#207401', '#056201',
                          '#004C00', '#023B01', '#012E01', '#011D01', '#011301'
))(length(breaks)-1)
rasterVis::levelplot(ndvi.S.brick, at=breaks, col.regions=cols, main="NDVI")



#get the date from the names of the layers and extract the month
ind<- format(as.Date(names(ndvi.S.brick), format = "X%Y%m%d"), format = "%m") %>% 
  as.numeric()

#median of layers by month
ndvi.S.monthly<- stackApply(ndvi.S.brick, ind, fun = median)
names(ndvi.S.monthly)<- month.abb[c(11:12,1:3)]

#plot aggregated data
rasterVis::levelplot(ndvi.S.monthly, at=breaks, col.regions=cols, main="NDVI")




####################################################
### Write and save RasterBricks as geoTIFF files ###
####################################################

setwd("~/Documents/Snail Kite Project/Data/armadillos/NDVI")

##North

#if saving individual layers
# writeRaster(ndvi.N.monthly, filename=names(ndvi.N.monthly), bylayer=TRUE, format="GTiff")

#if saving RasterBrick as raster format
writeRaster(ndvi.N.monthly, filename = 'ndvi_N_monthly.grd', format="raster",
            overwrite=TRUE)

#if saving RasterBrick as geoTIFF
# writeRaster(ndvi.N.monthly, filename='ndvi_N_monthly.tif', format="GTiff", overwrite=TRUE,
#             options=c("INTERLEAVE=BAND","COMPRESS=LZW"))



##South

#if saving individual layers
# writeRaster(ndvi.S.monthly, filename=names(ndvi.S.monthly), bylayer=TRUE, format="GTiff")

#if saving RasterBrick as raster format
writeRaster(ndvi.S.monthly, filename = 'ndvi_S_monthly.grd', format="raster",
            overwrite=TRUE)

#if saving RasterBrick as geoTIFF
# writeRaster(ndvi.S.monthly, filename='ndvi_S_monthly.tif', format="GTiff", overwrite=TRUE,
#             options=c("INTERLEAVE=BAND","COMPRESS=LZW"))