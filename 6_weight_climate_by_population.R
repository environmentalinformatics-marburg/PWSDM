
library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# read data
main <- "E:/populationclimate/"
datapath <- paste0(main, "reprojected/")

#HDM and CDM per month and grid cell
hdm <- readRDS(file=paste0(datapath, "HDM.rds"))
cdm <- readRDS(file=paste0(datapath, "CDM.rds"))

#HDM and CDM per month and country
hdmm <- readRDS(file=paste0(datapath, "HDM_per_m_and_c.rds"))
cdmm <- readRDS(file=paste0(datapath, "CDM_per_m_and_c.rds"))

#HDM and CDM per year and grid cell 

# #column - year identification
# lo <- NULL
# for(i in seq(45)){
#   lo[i] <- 13+(i*12)
# }
# lo <- c(13,lo)
# 
# hi <- NULL
# hi <- lo+11
# 
# csumyear <- lapply(seq(46), function(i){
#   x <- cdmsh@data[lo[i]:hi[i]]
#   na <- substr(names(x),2,5)[1]
#   bord@data[,na] <- rowSums(x)
#   bord@data[,na]
# })
# 
# cbord <- bord
# y <- as.character(c(1970:2015))
# for(i in seq(y)){
#   cbord@data[,y[i]] <- csumyear[[i]]
# }
# 



#HDM and CDM per year and country
hdmy <- readRDS(file=paste0(datapath, "HDM_per_y_and_c.rds"))
cdmy <- readRDS(file=paste0(datapath, "CDM_per_y_and_c.rds"))

#population data: Raster Stack
pop <- readRDS(file=paste0(main, "reprojected/complete_population.rds"))

#weight climate data by population
######################## data structure ################
#hdm und cdm shapefile 1970:2015
names(hdm@data)
#population: rasterstack 1970:2015
names(pop)


######################## weight ####################################

##### HDM // CDM per country and year * population ###########

#output in shapefile (for table)


#output in raster (for map)
#make rasterstack out of hdm and cdm per year and country
#shapefile (rasterize function)

nam <- c(names(hdmy)[13:58])

hdmras <- lapply(seq(nam), function(j){
  x <- rasterize(hdmy, pop[[1]], field=nam[j])
  names(x) <- nam[j]
  x
})

cdmras <- lapply(seq(nam), function(j){
  x <- rasterize(cdmy, pop[[1]], field=nam[j])
  names(x) <- nam[j]
  x
})

#save
hdmrasst <- stack(hdmras)
saveRDS(hdmrasst,  file=paste0(datapath, "hdm_ras_years.rds"))
cdmrasst <- stack(cdmras)
saveRDS(cdmrasst,  file=paste0(datapath, "cdm_ras_years.rds"))

#weight by population
hdmpop <- lapply(seq(nam), function(i){
  x <- pop[[i]]*hdmrasst[[i]]
  names(x) <- nam[i]
  x
})

cdmpop <- lapply(seq(nam), function(i){
  x <- pop[[i]]*cdmrasst[[i]]
  names(x) <- nam[i]
  x
})

#save
hdmpopst <- stack(hdmpop)
cdmpopst <- stack(cdmpop)
saveRDS(hdmpopst,  file=paste0(datapath, "hdm_ras_years_weight.rds"))
saveRDS(cdmpopst,  file=paste0(datapath, "cdm_ras_years_weight.rds"))


#HDM // CDM per gridcell and year * population

