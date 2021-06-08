library(reticulate)
eemont <- import('eemont')
library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(sp)
library(rgee)
library(rasterVis)
library(googledrive)

ee_Initialize(email = "joshcullen10@gmail.com", drive = T)



setwd("~/Documents/Snail Kite Project/Data/R Scripts/acceleration")
dat<- read.csv("Binned Armadillo Acceleration Data.csv", as.is = T)
dat$date<- as_datetime(dat$date)

# Filter out observations where coords are NA
dat<- dat %>% 
  filter(!is.na(x))


## Date range for giant armadillos
range(dat$date)  #2019-05-19 to 2020-01-24


#create dummy raster for AOI
rast<- raster(extent(min(dat$x) - 3000,
                     max(dat$x) + 3000,
                     min(dat$y) - 3000,
                     max(dat$y) + 3000),
              crs = "+init=epsg:32721")

bounds<- st_as_sf(data.frame(rasterToPoints(rast)), coords = c("x","y"), 
                  crs = "+init=epsg:32721") %>%  #convert to sf object
  st_bbox() %>%  #extract bounding box
  st_as_sfc() %>%  #convert into polygon
  sf_as_ee()  #convert into GEE format 



################################
### Retrieve Sentinel-2 Data ###
################################

s2<- ee$ImageCollection("COPERNICUS/S2_SR")$ 
  filterBounds(bounds)$
  filterDate('2019-05-01', '2020-01-31')$
  sort("system:time_start", TRUE)$  # Sort the collection in chronological order
  map(function(x) x$reproject("EPSG:32721"))$
  filterMetadata('CLOUD_COVERAGE_ASSESSMENT', 'less_than', 30)

print(s2$size()$getInfo())  #check number of images (34)


## Functions to map to ImageCollection

# This functions masks clouds and cirrus (bits 10 and 11)
maskcloud1<- function(image) {
  cloudsBitMask<- bitwShiftL(1, 10)
  cirrusBitMask<- bitwShiftL(1, 11)
  qa<- image$select('QA60')  # Get the pixel QA band
  # Both flags should be set to zero, indicating clear conditions
  mask<- qa$bitwiseAnd(cloudsBitMask)$eq(0)$
    And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
  return(image$updateMask(mask)$divide(10000))
}

# This function gets NDVI from Sentinel-2 imagery
addNDVI<- function(image) {
  return(image$addBands(image$normalizedDifference(c("B8", "B4"))$
                          rename('NDVI')))
}

# This function gets NDVI from Sentinel-2 imagery
addNDWI<- function(image) {
  return(image$addBands(image$normalizedDifference(c("B3", "B8"))$
                          rename('NDWI')))
}

# This function calculates EVI from Sentinel-2 imagery
addEVI<- function(image) {
  
  return(image$addBands(image$expression(
    expression = '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
    opt_map =  list(
      'NIR' = image$select('B8'),
      'RED' = image$select('B4'),
      'BLUE' = image$select('B2')
    )
  )$rename('EVI')$toFloat())  #needs to be float, not double
  )
  
}


# Map the NDVI and cloud mask functions; select only the NDVI band
s2_evi<- s2$
  maskClouds(prob = 65, cdi = -0.5, buffer = 300)$
  # map(maskcloud1)$
  map(addEVI)$
  select('EVI')

ee_print(s2_evi)
print(s2_evi$getInfo())


nimages<- s2_evi$size()$getInfo()
ic_date<- ee_get_date_ic(s2_evi)

# Plot the map of the median NDVI for the region
Map$setCenter(-55.76664, -19.19482, 11)
Map$addLayer(s2_evi$first(),
             visParams = list(
               min = -0.5,
               max = 1.0,
               bands = "EVI",
               palette = c(
                 'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
                 '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
                 '012E01', '011D01', '011301'
               )
             ))

# # Create a number ee$List where each element represent a month
# months <- ee$List$sequence(1, 12)
# 
# # Function to Calculate a monthly composite
# monthly_s2 <- function(m) {
#   s2_evi$
#     filter(ee$Filter$calendarRange(m, m, "month"))$
#     reduce(ee$Reducer$median()) %>%
#     ee$Image$select("EVI_median")
# }
# s2_monthly <- months$map(ee_utils_pyfunc(monthly_s2))
# 
# # Example: Display January and August median composite
# s2_mean_jan <- ee$Image(s2_monthly$get(0))
# s2_mean_aug <- ee$Image(s2_monthly$get(7))
# 
# ## Vis parameters.
# visparams <- list(
#   bands = "EVI_median",
#   min = -0.5,
#   max = 1,
#   palette = palette
# )
# 
# Map$addLayer(s2_mean_jan, visparams, name = "Jan") +
#   Map$addLayer(s2_mean_aug, visparams, name = "Aug")

pal1<- c(
  'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
  '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
  '012E01', '011D01', '011301'
)
Map$centerObject(bounds, zoom = 11)
s2_img_list <- list() 
for (index in seq_len(nimages)) {
  py_index <- index - 1
  s2_img <- ee$Image(s2_evi$toList(1, py_index)$get(0))
  s2_img_list[[index]] <- Map$addLayer(
    eeObject = s2_img,
    visParams = list(min = -0.5, max = 1.0, palette = pal1),
    name = ic_date$id[index]
  )
}
Reduce('+', s2_img_list)





## define flood and dry periods
floodFilter1 = ee$Filter$date('2019-05-01','2019-07-31')
floodFilter2 = ee$Filter$date('2020-01-01','2020-01-31')
floodFilter = ee$Filter$Or(floodFilter1, floodFilter2)
dryFilter = ee.Filter.date('2019-08-01','2019-12-31')


# Map the NDWI and cloud mask functions; select only the NDWI band
s2_ndwi_flood<- s2$
  filter(floodFilter)$
  map(maskcloud1)$
  map(addNDWI)$
  select('NDWI')

ee_print(s2_ndwi_flood)
print(s2_ndwi$getInfo())


nimages<- s2_ndwi$size()$getInfo()
ic_date<- ee_get_date_ic(s2_ndwi)

# Plot the map of the median NDWI for the region
Map$setCenter(-55.76664, -19.19482, 11)
Map$addLayer(s2_ndwi$median(),
             visParams = list(
               min = -0.5,
               max = 1.0,
               bands = "NDWI",
               palette = c("#ffffff", "#0000ff","#0000ff")
             ))

pal1<- c("#ffffff", "#0000ff","#0000ff")
Map$centerObject(bounds, zoom = 11)
s2_img_list <- list() 
for (index in seq_len(nimages)) {
  py_index <- index - 1
  s2_img <- ee$Image(s2_ndwi$toList(1, py_index)$get(0))
  s2_img_list[[index]] <- Map$addLayer(
    eeObject = s2_img,
    visParams = list(min = -0.1, max = 1.0, palette = pal1),
    name = ic_date$id[index]
  )
}
Reduce('+', s2_img_list)



###########################
### Export Data Locally ###
###########################

# Download all images locally
setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist/NDVI-Sentinel2")

ndvi<- ee_imagecollection_to_local(
  ic = s2_ndvi,
  scale = 10,
  region = bounds,
  via = 'drive'
)
