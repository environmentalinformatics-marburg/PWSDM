
################## CALCULATE HDM and CMD ######################################
###############################################################################

library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# read data
#main <- "E:/populationclimate/"
main <- "C:/Users/mleza/Documents/Jobs/populationclimate/"
datapath <- paste0(main, "run/")
temp <- readRDS(file=paste0(datapath, "temp.rds"))
source(paste0(main, "scripts/functions.R"))

############# calculate HDM and CDM for each month and grid cells #############

  #cut temp to 1970-2015
  temp <- temp[[109:660]]

  #HDM: 18.3-Temperatur; IF <= 0 set to 0
  HDM <- list()
  for(i in seq(nlayers(temp))){
    r <- 18.3-temp[[i]]
    r[r <= 0] <- 0
    HDM[[i]] <- r
  }
  hdm <- stack(unlist(HDM))
  
  #CDM:  Temperatur - 18,3; IF <=0 set to 0
  CDM <- list()
  for(i in seq(nlayers(temp))){
    r <- temp[[i]]-18.3
    r[r <= 0] <- 0
    CDM[[i]] <- r
  }
  cdm <- stack(unlist(CDM))
  
  #save
  saveRDS(hdm,  file=paste0(datapath, "hd_grid_m.rds"))
  saveRDS(cdm,  file=paste0(datapath, "cd_grid_m.rds"))

########## aggregate HDM and CDM to countries for all months ###################
  
  #read borders dataset
  f <- list.files(paste0(main, "TM_WORLD_BORDERS-03/"), pattern=".shp", 
                  full.names=T)
  bord <- readOGR(f)
  #new attibute ID in order to match to values in zoneraster
  bord$ID <- c(1:246)
  
  #create grid with country signature (ID from shapefile bord)
  zoneras <- rasterize(bord,hdm[[1]], field="ID")
  #! 55 small countries are missing (because they are smaller than grid resolution), 
  #rasterize only transfers value if polygon covers center of raster cell.
  
  #calculate mean h and c deviations for each country for all months
  hdmcntr <- lapply(seq(nlayers(hdm)), function(i){ 
    zt <- zonal(hdm[[i]],zoneras)
    zt <- as.data.frame(zt)
    print(i)
    zt
  })
  cdmcntr <- lapply(seq(nlayers(cdm)), function(i){
    zt <- zonal(cdm[[i]],zoneras)
    zt <- as.data.frame(zt)
    print(i)
    zt
  })
  
  #merge mean h and c deviation values (h/cdmcntr) for each month and 
  #country to world borders shapefile
  hdmcntrm <- lapply(seq(hdmcntr), function(i){
    mymerge(bord, hdmcntr,i, hdm)
  })
  
  cdmcntrm <- lapply(seq(cdmcntr), function(i){
    mymerge(bord, cdmcntr,i, cdm)
  })
  
  #make pattern for cbind (bind last column of all attribute tables into one shape)  
  pch <- makpat("h", 552)
  pcc <- makpat("c", 552)
  
  #execute cbind: combine all month's country means into one data frame
  HDMall <- eval(parse(text=paste0("cbind(hdmcntrm[[1]]@data, ", 
                                   paste0(pch, collapse=""),")") ))
  CDMall <- eval(parse(text=paste0("cbind(cdmcntrm[[1]]@data, ", 
                                   paste0(pcc, collapse=""),")") ))
  
  
  #merge data frame with all month's data and world borders shape
  bord@data[1:11] <- NULL #all country specifications coming from H/CDMall
  cdmsh <- merge(bord, HDMall,  by="ID") 
  hdmsh <- merge(bord, CDMall,  by="ID")

  
  #save
  saveRDS(cdmsh, file=paste0(datapath, "CDM_per_m_and_c.rds"))
  saveRDS(hdmsh, file=paste0(datapath, "HDM_per_m_and_c.rds"))

############ AGGREGATE mean c and h deviations per countries TO YEARS ######
  #hi and lo: auxiliary variable to identify which columns in
  #shapefiles enclose the respective year
  lo <- NULL
  for(i in seq(45)){
    lo[i] <- 13+(i*12)
  }
  lo <- c(13,lo)
  
  hi <- NULL
  hi <- lo+11
  
  #read clean shapefile
  bord <- readOGR(f)
  bord$ID <- c(1:246)

  #generate list of sums from c and h mean differences per country and month
  hsumyear <- lapply(seq(46), function(i){
    x <- hdmsh@data[lo[i]:hi[i]] #summarize all months for the respective year
    na <- substr(names(x),2,5)[1] #extract year from column names
    bord@data[,na] <- rowSums(x) #sums from all months per country
    bord@data[,na]
  })
  
  csumyear <- lapply(seq(46), function(i){
    x <- cdmsh@data[lo[i]:hi[i]] 
    na <- substr(names(x),2,5)[1] 
    bord@data[,na] <- rowSums(x)  
    bord@data[,na]
  })
  

  #write sum of monthly values to shapefile
  y <- as.character(c(1970:2015)) #name template

  hbord <- bord
  for(i in seq(y)){
    hbord@data[,y[i]] <- hsumyear[[i]]
  }
  
  cbord <- bord
  for(i in seq(y)){
    cbord@data[,y[i]] <- csumyear[[i]] 
  }
  
  #save HDM / CDM per year shapefiles
  saveRDS(cbord, file=paste0(datapath, "CDM_per_y_and_c.rds"))
  saveRDS(hbord, file=paste0(datapath, "HDM_per_y_and_c.rds"))
  
  