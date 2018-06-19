library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# read data
main <- "E:/populationclimate/"
datapath <- paste0(main, "reprojected/")
pop <- readRDS(file=paste0(datapath, "complete_population.rds"))
temp <- readRDS(file=paste0(datapath, "temp.rds"))
#read borders dataset
f <- list.files(paste0(main, "TM_WORLD_BORDERS-03/"), pattern=".shp", full.names=T)
bord <- readOGR(f)

#test
plot(pop[[1]])
plot(bord,add=T)

