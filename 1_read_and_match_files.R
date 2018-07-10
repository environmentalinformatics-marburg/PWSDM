library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# paths
main <- "E:/populationclimate/"
pathcru <- paste0(main, "CRU 4.01 TS/")
pathhyde <- paste0(main, "Hyde Data/population_data/")
pathgpw <- paste0(main, "GPWV3/")

# #unzip temperature data
# f <- list.files(pathcru, pattern="nc.gz", full.names=T)
# for(i in seq(f)){
#   R.utils::gunzip(f[i])
# }

###################### READ DATA ############################
# TEMPERATURE
files <- list.files(pathcru, pattern="nc$", full.names=T)
temp <- lapply(seq(files), function(i){
  stack(files[i])
})

#HYDE POPULATION
x <- list.files(pathhyde)
poppath <- paste0(pathhyde, x, "/")
#Time intervals are 10 yr till 2000, and from 2000 - 2015 1 year timesteps.
popc <- lapply(seq(poppath), function(i){
  pf <- list.files(poppath[i], pattern="popc", full.names=T)
  stack(pf)
})


#GRIDDED POPULATION OF THE WORLD 
fg <- list.files(pathgpw, full.names=T, pattern="half$")
#4 through 6: ag-Version selected (adjusted to UN, in order to match 1
#through 3)
gpw <- lapply(seq(fg), function(i){
  f <- list.files(fg[i], pattern="ag", full.names=T)
  raster(f)
})

###### Reproject CRU and HYDE to GPWV3 geometry ################

#assign WGS84 (EPSG: 4326) to gridded world population rasters
#as stated in readme-file
for(i in seq(gpw)){
  proj4string(gpw[[i]]) <- CRS("+init=epsg:4326")
}

#assign WGS84 (EPSG: 4326) to Hyde population data - no documentation on CRS
#available 
for(i in seq(popc)){
  proj4string(popc[[i]]) <- CRS("+init=epsg:4326")
}

#temperature is already in WGS84 (EPSG: 4326)

########## RESAMPLING ##########################################
popc <- stack(popc)
gpw <- stack(gpw)
tempstack <- stack(temp)

newex <- extent(c(-180,180,-58,85))

#adapt resolution of HYDE population to gridded population
popcrs <- resample(popc, gpw)
gpwex <- crop(gpw,newex)

#put data together into stacks
popstack <- stack(gpwex, popcrs)
popstack <- stack(popstack[[7]],popstack[[8]],popstack[[9]],popstack[[5]],popstack[[6]],
                  popstack[[4]],popstack[[1]],popstack[[2]],
                  popstack[[3]])
tempstack <- crop(tempstack, newex)

#write stacks
saveRDS(tempstack, file=paste0(main, "reprojected/temp.rds"))
saveRDS(popstack, file=paste0(main, "reprojected/pop.rds"))
