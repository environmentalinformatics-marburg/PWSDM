
################## WEIGHT HDM and CMD BY POPULATION ###########################
###############################################################################

library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

main <- "C:/Users/mleza/Documents/Jobs/populationclimate/"
datapath <- paste0(main, "run_15/")
source(paste0(main, "scripts/functions.R"))

#read population data: Raster Stack
pop <- readRDS(file=paste0(main, "run/complete_population.rds"))

############### Weight by population AFTER aggregation to country level #######
  #read data: HDM and CDM per year and country
  hdmy <- readRDS(file=paste0(datapath, "HDM_per_y_and_c.rds"))
  cdmy <- readRDS(file=paste0(datapath, "CDM_per_y_and_c.rds"))
  
  #make Raster with codes for zones in order to calculate 
  #zonal statistics on Raster. coderas = template
  coderas <- rasterize(cdmy, pop[[1]], field="ID")
  
  #sum up population per country
  popsumcntr <- as.data.frame(zonal(pop, coderas,'sum'))
  
  #merge zonal statistics to HDM and CDM shapefiles 
  namzon <- c("zone" ,paste0("psum", c(1970:2015)))
  names(popsumcntr) <- namzon
  
  #merge population sums per country to HDM and CDM per year and country shape
  hdmpop1 <- merge(hdmy, popsumcntr, by.x='ID', by.y='zone')
  cdmpop1 <- merge(cdmy, popsumcntr, by.x='ID', by.y='zone')
  
  #multiply HDM and CDM per year and country with population sums
  clim <- c(13:58) #identify columns in shape where HDM / CDM data are located
  popu <- c(59:104) #identify columns in shape where population sums are located
  y <- paste0("w", c(1970:2015)) #template for new column names
  for(i in seq(1970:2015)){
    hdmpop1@data[,y[i]] <- hdmpop1@data[clim[i]]*hdmpop1@data[popu[i]]
    cdmpop1@data[,y[i]] <- cdmpop1@data[clim[i]]*cdmpop1@data[popu[i]]
  }
  
  saveRDS(hdmpop1,  file=paste0(datapath, "hdm_shape_years_weight.rds"))
  saveRDS(cdmpop1,  file=paste0(datapath, "cdm_shape_years_weight.rds"))


############### Weight by population BEFORE aggregation to country level #######
  #read data: hot and cool deviations from threshold (grid and months)
  hgm <- readRDS(file=paste0(datapath, "hd_grid_m.rds")) 
  cgm <- readRDS(file=paste0(datapath, "cd_grid_m.rds"))
  #multiplication of hot and cool deviations from threshold * population
  hgmw <- hgm*pop
  cgmw <- cgm*pop
  
  #mean weighted h and c deviations/country
  #read borders dataset
  f <- list.files(paste0(main, "TM_WORLD_BORDERS-03/"), pattern=".shp", 
                  full.names=T)
  bord <- readOGR(f)
  #new attibute ID in order to match to values in zoneraster
  bord$ID <- c(1:246)
  
  #create grid with country signature (ID from shapefile bord)
  zoneras <- rasterize(bord,hgmw[[1]], field="ID")
  #! 55 small countries are missing (because they are smaller than grid resolution), 
  #rasterize only transfers value if polygon covers center of raster cell.
  
  #calculate mean h and c deviations for each country for all months
  hdmcntr <- lapply(seq(nlayers(hgmw)), function(i){ 
    zt <- zonal(hgmw[[i]],zoneras)
    zt <- as.data.frame(zt)
    print(i)
    zt
  })
  cdmcntr <- lapply(seq(nlayers(cgmw)), function(i){
    zt <- zonal(cgmw[[i]],zoneras)
    zt <- as.data.frame(zt)
    print(i)
    zt
  })
  
  #merge mean weight h and c deviation values (h/cdmcntr) for each month and 
  #country to world borders shapefile
  hdmcntrm <- lapply(seq(hdmcntr), function(i){
    mymerge(bord, hdmcntr,i, hgmw)
  })
  
  cdmcntrm <- lapply(seq(cdmcntr), function(i){
    mymerge(bord, cdmcntr,i, cgmw)
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
  saveRDS(cdmsh, file=paste0(datapath, "CDM_w_bef_agg_per_m_and_c.rds"))
  saveRDS(hdmsh, file=paste0(datapath, "HDM_w_bef_agg_per_m_and_c.rds"))

############ AGGREGATE mean weight c and h deviations per countries TO YEARS ######
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
  
  #generate list of sums from weight c and h mean differences per country and month
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
  saveRDS(cbord, file=paste0(datapath, "CDM_w_bef_agg_per_y_and_c.rds"))
  saveRDS(hbord, file=paste0(datapath, "HDM_w_bef_agg_per_y_and_c.rds"))
  
  