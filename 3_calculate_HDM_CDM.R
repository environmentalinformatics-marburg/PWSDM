library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# read data
main <- "E:/populationclimate/"
datapath <- paste0(main, "reprojected/")
temp <- readRDS(file=paste0(datapath, "temp.rds"))

############# calculate HDM and CDM  ##############################################
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

############ aggregate HDM and CDM to year on grid cells #########
HDM <- readRDS(file=paste0(datapath, "HDM.rds"))
CDM <- readRDS(file=paste0(datapath, "CDM.rds"))

#select 1970-2015
HDM <- HDM[[109:660]]
CDM <- CDM[[109:660]]

#column - year identification
lo <- NULL
for(i in seq(45)){
  lo[i] <- 1+(i*12)
}
lo <- c(1,lo)
hi <- NULL
hi <- lo+11

#HDM
hdmyeargrid <- lapply(seq(hi), function(i){
  mean(HDM[[lo[i]:hi[i]]])
})
hdmyg <- stack(hdmyeargrid)
names(hdmyg) <- c(1970:2015)

#CDM
cdmyeargrid <- lapply(seq(hi), function(i){
  mean(CDM[[lo[i]:hi[i]]])
})
cdmyg <- stack(cdmyeargrid)
names(cdmyg) <- c(1970:2015)

saveRDS(hdmyg,  file=paste0(datapath, "HDM_per_y_and_g.rds"))
saveRDS(cdmyg,  file=paste0(datapath, "CDM_per_y_and_g.rds"))
