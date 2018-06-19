library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# read data
main <- "E:/populationclimate/"
datapath <- paste0(main, "reprojected/")
temp <- readRDS(file=paste0(datapath, "temp.rds"))

############# calculate HDM ##############################################
#HDM: 18.3-Temperatur; IF <= 0 set to 0
HDM <- list()
for(i in seq(nlayers(temp))){
  r <- 18.3-temp[[i]]
  r[r <= 0] <- 0
  HDM[[i]] <- r
}
HDM <- stack(unlist(HDM))

#CDM:  Temperatur - 18,3; IF <=0 set to 0
CDM <- list()
for(i in seq(nlayers(temp))){
  r <- temp[[i]]-18.3
  r[r <= 0] <- 0
  CDM[[i]] <- r
}
CDM <- stack(unlist(CDM))

saveRDS(HDM,  file=paste0(datapath, "HDM.rds"))
saveRDS(CDM,  file=paste0(datapath, "CDM.rds"))

