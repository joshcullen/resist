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

## EVI

evi<- brick('GiantArm_evi_monthly.grd')
evi<- crop(evi, extent(dat %>% 
                                 summarize(xmin = min(x) - 3000,
                                           xmax = max(x) + 3000,
                                           ymin = min(y) - 3000,
                                           ymax = max(y) + 3000) %>% 
                                 unlist()))
evi.s<- scale(evi)

## NDWI

ndwi<- brick('GiantArm_ndwi_monthly.grd')
ndwi<- crop(ndwi, evi)
ndwi.s<- scale(ndwi)


covars<- stack(evi.s, ndwi.s)


#######################################################
### Extract values from raster layer for each track ###
#######################################################
plan(multisession)

dat.em<- dat %>% 
  filter(id == "emanuel")

# progressr::with_progress({  #to print progress bar
  path<- extract.covars(data = dat.em, layers = evi.s, state.col = "z.post.thresh",
                        dyn_names = "evi", ind = "month")
# })
#takes 54 s to run

future:::ClusterRegistry("stop")  #close all threads and memory used



###################
### Export data ###
###################

setwd("~/Documents/Snail Kite Project/Data/R Scripts/ValleLabUF/resist")

# Armadillo Data
# write.csv(path, "Emanuel Resistance Data.csv", row.names = F)
