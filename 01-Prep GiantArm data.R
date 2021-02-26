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




#######################################
### Import Environmental Covariates ###
#######################################

setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist_avg")

## NDVI

ndvi<- brick('GiantArm_ndvi_season.grd')
ndvi<- crop(ndvi, extent(dat %>% 
                                 summarize(xmin = min(x) - 3000,
                                           xmax = max(x) + 3000,
                                           ymin = min(y) - 3000,
                                           ymax = max(y) + 3000) %>% 
                                 unlist()))

## AWEI

awei<- brick('GiantArm_awei_season.grd')
awei<- crop(awei, ndvi)


covars<- stack(ndvi, awei)


#######################################################
### Extract values from raster layer for each track ###
#######################################################
plan(multisession)

# progressr::with_progress({  #to print progress bar
  path<- extract.covars(data = dat, layers = covars, state.col = "z.post.thresh",
                        dyn_names = c("ndvi","awei"), ind = "season")
# })
#takes 2 min to run

future:::ClusterRegistry("stop")  #close all threads and memory used



###################
### Export data ###
###################

setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist")

# Armadillo Data
# write.csv(path, "Giant Armadillo Resistance Data.csv", row.names = F)
