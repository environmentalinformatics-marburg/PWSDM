library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# read data
main <- "E:/populationclimate/"
datapath <- paste0(main, "reprojected/")

##SECOND APPROACH
hdm2 <- readRDS(file=paste0(datapath, "HDM_dat2_per_y_and_c.rds"))
cdm2 <- readRDS(file=paste0(datapath, "CDM_dat2_per_y_and_c.rds"))