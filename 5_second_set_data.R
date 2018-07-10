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
  r <- 15.5-temp[[i]]
  r[r <= 0] <- 0
  HDM[[i]] <- r
}
HDM <- stack(unlist(HDM))

#CDM:  Temperatur - 18,3; IF <=0 set to 0
CDM <- list()
for(i in seq(nlayers(temp))){
  r <- temp[[i]]-24.0
  r[r <= 0] <- 0
  CDM[[i]] <- r
}
CDM <- stack(unlist(CDM))

saveRDS(HDM,  file=paste0(datapath, "HDM_dat2.rds"))
saveRDS(CDM,  file=paste0(datapath, "CDM_dat2.rds"))


#CALCULATE HDM AND CDM PER COUNTRY
# read data
hdm <- readRDS(file=paste0(datapath, "HDM_dat2.rds"))
cdm <- readRDS(file=paste0(datapath, "CDM_dat2.rds"))

#cut hdm and cdm to 1970-2015
hdm <- hdm[[109:660]]
cdm <- cdm[[109:660]]

#read borders dataset
f <- list.files(paste0(main, "TM_WORLD_BORDERS-03/"), pattern=".shp", 
                full.names=T)
bord <- readOGR(f)
#new attibute ID in order to match to values in zoneraster
bord$ID <- c(1:246)

#function for merging
mymerge <- function(shape, zonal,i){
  m1 <- merge(shape, zonal[[i]],  by.x="ID", by.y="zone")
  names(m1)[length(names(m1))] <- names(hdm[[i]])
  return(m1)
}

#Make grid with country signatures. All borders polygon's variables are saved 
#as attributes in raster
zoneras <- rasterize(bord,hdm[[1]])
#55 small countries are missing (smaller than grid resolution), rasterize only
#transfers value if polygon covers center of raster cell.Better solution ???????


########## CALC MEAN HDM AND CDM ALL COUNTRIES; ALL MONTHS #####################

############ HDM ########################
#zonal statistics 
hdmcntr <- lapply(seq(nlayers(hdm)), function(i){
  zt <- zonal(hdm[[i]],zoneras)
  zt <- as.data.frame(zt)
  print(i)
  zt
})

#merge zonal statistics for each country to shapefile
hdmcntrm <- lapply(seq(hdmcntr), function(i){
  mymerge(bord, hdmcntr,i)
})

#make pattern for cbind all attribute tables
patcb <- lapply(c(2:552), function(i){
  noquote(paste0("hdmcntrm[[",i,"]]@data[ncol(hdmcntrm[[",i,"]]@data)],"))
})
patcb[[551]] <- substr(patcb[[551]], 1, nchar(patcb[[551]])-1)
pc <- unlist(patcb)
pc <- noquote(pc)

#combine all month's zonal statistics into one shapefile
HDMall <- eval(parse(text=paste0("cbind(hdmcntrm[[1]]@data, ", 
                                 paste0(pc, collapse=""),")") ))
bord@data[1:11] <- NULL
HDMshape <- merge(bord, HDMall,  by="ID")

######### CDM ######################
#zonal statistics
cdmcntr <- lapply(seq(nlayers(cdm)), function(i){
  zt <- zonal(cdm[[i]],zoneras)
  zt <- as.data.frame(zt)
  print(i)
  zt
})

#read clean shapefile
bord <- readOGR(f)
bord$ID <- c(1:246)

#merge to shapefile
cdmcntrm <- lapply(seq(cdmcntr), function(i){
  mymerge(bord, cdmcntr,i)
})

#make pattern for cbind
cpatcb <- lapply(c(2:552), function(i){
  noquote(paste0("cdmcntrm[[",i,"]]@data[ncol(cdmcntrm[[",i,"]]@data)],"))
})
cpatcb[[551]] <- substr(cpatcb[[551]], 1, nchar(cpatcb[[551]])-1)
cpc <- unlist(cpatcb)
cpc <- noquote(cpc)

#combine attribute tables
CDMall <- eval(parse(text=paste0("cbind(cdmcntrm[[1]]@data, ", 
                                 paste0(cpc, collapse=""),")") ))
bord@data[1:11] <- NULL
CDMshape <- merge(bord, CDMall,  by="ID")

#save
saveRDS(CDMshape, file=paste0(datapath, "CDM_dat2_per_m_and_c.rds"))
saveRDS(HDMshape, file=paste0(datapath, "HDM_dat2_per_m_and_c.rds"))

#write shapefiles
writeOGR(CDMshape, layer = "CDM_dat2_m_shape", driver="ESRI Shapefile",
         dsn = "E:/populationclimate/reprojected", overwrite_layer = TRUE)
writeOGR(HDMshape, layer = "HDM_dat2_m_shape", driver="ESRI Shapefile",
         dsn = "E:/populationclimate/reprojected", overwrite_layer = TRUE)


########## MAKE SUMS PER YEAR FOR ALL COUNTRIES ################################
cdmsh <- readRDS(paste0(datapath, "CDM_dat2_per_m_and_c.rds"))
hdmsh <- readRDS(paste0(datapath, "HDM_dat2_per_m_and_c.rds"))

#read clean shapefile
bord <- readOGR(f)
bord$ID <- c(1:246)


### CDM ######

#column - year identification
lo <- NULL
for(i in seq(45)){
  lo[i] <- 13+(i*12)
}
lo <- c(13,lo)

hi <- NULL
hi <- lo+11

csumyear <- lapply(seq(46), function(i){
  x <- cdmsh@data[lo[i]:hi[i]]
  na <- substr(names(x),2,5)[1]
  bord@data[,na] <- rowSums(x)
  bord@data[,na]
})

cbord <- bord
y <- as.character(c(1970:2015))
for(i in seq(y)){
  cbord@data[,y[i]] <- csumyear[[i]]
}

#save CDM per year shapefile
saveRDS(cbord, file=paste0(datapath, "CDM_dat2_per_y_and_c.rds"))
writeOGR(cbord, layer = "CDM_dat2_y_shape", driver="ESRI Shapefile",
         dsn = "E:/populationclimate/reprojected", overwrite_layer = TRUE)



### HDM ####
hsumyear <- lapply(seq(46), function(i){
  x <- hdmsh@data[lo[i]:hi[i]]
  na <- substr(names(x),2,5)[1]
  bord@data[,na] <- rowSums(x)
  bord@data[,na]
})

hbord <- bord
for(i in seq(y)){
  hbord@data[,y[i]] <- hsumyear[[i]]
}

#save HDM per year shapefile
saveRDS(hbord, file=paste0(datapath, "HDM_dat2_per_y_and_c.rds"))
writeOGR(hbord, layer = "HDM_dat2_y_shape", driver="ESRI Shapefile",
         dsn = "E:/populationclimate/reprojected", overwrite_layer = TRUE)


