
##################### PRESENTATION OF RESULTS ##########################
########################################################################

library(raster)
library(ncdf4)
library(rgdal)
library(mapview)
library(ggplot2)
library(tidyr)
library(ggmaps)
require(maptools)
require(RColorBrewer)
library(classInt)

# paths
main <- "C:/Users/mleza/Documents/Jobs/populationclimate/"
datapath <- paste0(main, "run/")
version <- c("version18/", "version15/") #version with threshold of 18.3°C and 
                                         #15.5 and 24°C, respectively
respath <- paste0(main, "results/", version[1])
tabpath <- paste0(respath, "tables/")
shpath <- paste0(respath, "shapefiles/")
mappath <- paste0(respath, "maps/")
source(paste0(main, "scripts/functions.r"))

############################ TABLE OUTPUT ##############################

##### h and c per country and month (not weight) ####
  hdmcm <- readRDS(file=paste0(datapath, "HDM_per_m_and_c.rds"))
  cdmcm <- readRDS(file=paste0(datapath, "CDM_per_m_and_c.rds"))
  
  hdmcmtab <- hdmcm@data[!is.na(hdmcm@data[25]),]  #drop too small countries
  cdmcmtab <- cdmcm@data[!is.na(cdmcm@data[25]),]
  
  write.csv(hdmcmtab, paste0(tabpath, "table_hdm_c_m.csv"))
  write.csv(cdmcmtab, paste0(tabpath, "table_cdm_c_m.csv"))
  
##### h and c per country and year (not weight) ####

  hdmcy <- readRDS(file=paste0(datapath, "HDM_per_y_and_c.rds"))
  cdmcy <- readRDS(file=paste0(datapath, "CDM_per_y_and_c.rds"))
  
  hdmcytab <- hdmcy@data[!is.na(hdmcy@data[25]),]  #drop too small countries
  cdmcytab <- cdmcy@data[!is.na(cdmcy@data[25]),]
  
  write.csv(hdmcytab, paste0(tabpath, "table_hdm_c_y.csv"))
  write.csv(cdmcytab, paste0(tabpath, "table_cdm_c_y.csv"))

#### HDM and CDM / country / year, weight after aggregation ####

  hdmcyw <- readRDS(file=paste0(datapath, "hdm_shape_years_weight.rds"))
  cdmcyw <- readRDS(file=paste0(datapath, "cdm_shape_years_weight.rds"))
  
  hdmcywtab <- hdmcyw@data[!is.na(hdmcyw@data[25]),] #drop too small countries
  cdmcywtab <- cdmcyw@data[!is.na(cdmcyw@data[25]),]
  
  write.csv(hdmcywtab, paste0(tabpath, "table_hdm_c_y_w.csv"))
  write.csv(cdmcywtab, paste0(tabpath, "table_cdm_c_y_w.csv"))

#### HDM and CMD / country / year, weight before aggregation ####

  CDMshape <- readRDS(file=paste0(datapath, "CDM_w_bef_agg_per_y_and_c.rds"))
  HDMshape <- readRDS(file=paste0(datapath, "HDM_w_bef_agg_per_y_and_c.rds"))
  
  hdmcywbeftab <- HDMshape@data[!is.na(HDMshape@data[25]),] #drop too small countries
  cdmcywbeftab <- CDMshape@data[!is.na(CDMshape@data[25]),]
  
  write.csv(hdmcywbeftab, paste0(tabpath, "table_hdm_c_y_w_bef_agg.csv"))
  write.csv(cdmcywbeftab, paste0(tabpath, "table_cdm_c_y_w_bef_agg.csv"))

  
############################ SHAPEFILE OUTPUT #################################

  #### WRITE ALL TO RESULT DIRECTORY

  #write shapefiles
  whichfile <- c(CDMshape, HDMshape, hdmcyw, cdmcyw, hdmcy, cdmcy, hdmcm, cdmcm)
  wfchar <- c("CDMshape", "HDMshape", "hdmcyw", "cdmcyw", "hdmcy", "cdmcy", 
              "hdmcm", "cdmcm")
  for(i in seq(whichfile)){
    writeOGR(eval(parse(text=wfchar[i])), layer = wfchar[i], driver="ESRI Shapefile",
             dsn = paste0(shpath, wfchar[i],".shp"), overwrite_layer = TRUE)
    print(i)
  }
  
############################ MAP OUTPUT #####################################
#############################################################################
yearsnam <- as.character(c(1970:2015)) #year string for plot names
#prepare colours to use
cols4 <- colorRampPalette(brewer.pal(9,"YlOrRd"))(100) 
cols5 <- colorRampPalette(brewer.pal(9,"YlGnBu"))(100)

####### HDM AND CDM per year and country (no weight) ###########################
    names(hdmcy) <- paste0("x",names(hdmcy)) #rename columns so that spplot understands
    names(cdmcy) <- paste0("x",names(cdmcy))
    
    years <- names(cdmcy)[13:length(names(cdmcy))] #make string of all colnames for input
    yearsnam <- substr(years, 2,6) #make string without X for naming plots
    
  ##### HDM ######
    #make quantiles based on year with largest value range and use as breaks 
    #for color classification
    maxval <- sapply(c(13:length(names(cdmcy))), function(i){ #compute max values for all years
      max(hdmcy@data[,i], na.rm=T)
    })
    wmax <- which(maxval==max(maxval)) #which year has largest range?
    
    #prepare classification of weight HDM plot per country and colorkey, orientation: quantiles
    breaks.qt <- classIntervals(hdmcy@data[,wmax+12], n = 9, style = "quantile", intervalClosure = "right")
    breaks.qt$brks
    #my.at=c(-5,0,1,6623.498,23393663,2963034198,26750285612,214632654869,787797316175, max(hdmcyw$xw1970, na.rm=T))
    my.at <- breaks.qt$brks
    valmax <- my.at[length(my.at)]
    my.brks=seq(0, valmax, by=valmax/9)
    my.lab=as.character(round(my.brks,0))
    myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.lab))
    
    #plot and write
    pdf(paste0(mappath, "HDM_year_country.pdf"),width=16.54, height=11.69)
    spplot(hdmcy, years, main="HDM per country", cex.main = 2,
           names.attr=yearsnam, lwd=0.2, at = my.at, 
           colorkey=myColorkey)
    dev.off()
  
  ##### CDM ######
  
    #make quantiles based on year with largest value range and use as breaks 
    #for color classification
    maxval <- sapply(c(13:length(names(cdmcy))), function(i){ #compute max values for all years
      max(cdmcy@data[,i], na.rm=T)
    })
    wmax <- which(maxval==max(maxval)) #which year has largest range?
    
    #prepare classification of weight HDM plot per country and colorkey, orientation: quantiles
    breaks.qt <- classIntervals(cdmcy@data[,wmax+12], n = 9, style = "quantile", intervalClosure = "right")
    breaks.qt$brks
    my.at <- breaks.qt$brks
    valmax <- my.at[length(my.at)]
    my.brks=seq(0, valmax, by=valmax/9)
    my.lab=as.character(round(my.brks,0))
    myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.lab))
    
    #plot and write
    pdf(paste0(mappath, "CDM_year_country.pdf"),width=16.54, height=11.69)
    spplot(cdmcy, years, main="CDM per country", cex.main = 2,
           names.attr=yearsnam, lwd=0.2, at = my.at, 
           colorkey=myColorkey)
    dev.off()

### HDM and CDM weight by population AFTER aggregation to country ##############
    years <- names(hdmcyw)[105:length(names(hdmcyw))] #make string of all 
    #colnames for input
    
  ##### HDM ######
    #make quantiles based on year with largest value range and use as breaks 
    #for color classification
    maxval <- sapply(c(105:length(names(hdmcyw))), function(i){ #compute max values for all years
      max(hdmcyw@data[,i], na.rm=T)
    })
    wmax <- which(maxval==max(maxval)) #which year has largest range?
    
    #prepare classification of weight HDM plot per country and colorkey, orientation: quantiles
    breaks.qt <- classIntervals(hdmcyw@data[,wmax+104], n = 9, style = "quantile", intervalClosure = "right")
    breaks.qt$brks
    my.at <- breaks.qt$brks
    valmax <- my.at[length(my.at)]
    my.brks=seq(0, valmax, by=valmax/9)
    my.lab=as.character(round(my.brks,0))
    myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.lab))
    
    #plot and write
    pdf(paste0(mappath, "HDM_year_country_weight_after_agg.pdf"),width=16.54, height=11.69)
    spplot(hdmcyw, years, main="HDM per country, weight after aggregation to 
           countries", cex.main = 2,
           names.attr=yearsnam, lwd=0.2, at = my.at, 
           colorkey=myColorkey)
    dev.off()
    
  ##### CDM ######
    
    #make quantiles based on year with largest value range and use as breaks 
    #for color classification
    maxval <- sapply(c(105:length(names(cdmcyw))), function(i){ #compute max values for all years
      max(cdmcyw@data[,i], na.rm=T)
    })
    wmax <- which(maxval==max(maxval)) #which year has largest range?
    
    #prepare classification of weight HDM plot per country and colorkey, orientation: quantiles
    breaks.qt <- classIntervals(cdmcyw@data[,wmax+104], n = 9, style = "quantile", 
                                intervalClosure = "right")
    
    max(cdmcyw@data[,20], na.rm = T)
    breaks.qt$brks
    my.at <- breaks.qt$brks[3:length(breaks.qt$brks)]
    valmax <- my.at[length(my.at)]
    my.brks=seq(0, valmax, by=valmax/9)
    my.lab=as.character(round(my.brks,0))
    myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.lab))
    
    #plot and write
    pdf(paste0(mappath, "CDM_year_country_weight_after_agg.pdf"),width=16.54, height=11.69)
    spplot(cdmcyw, years, main="CDM per country, weight after aggregation to 
           countries", cex.main = 2,
           names.attr=yearsnam, lwd=0.2, at = my.at, 
           colorkey=myColorkey)
    dev.off()
  
### HDM and CDM weight by population BEFORE aggregation to country ############
    wbcdmcy <- readRDS(file=paste0(datapath, "CDM_w_bef_agg_per_y_and_c.rds")) 
    wbhdmcy <- readRDS(file=paste0(datapath, "HDM_w_bef_agg_per_y_and_c.rds")) 
    
    names(wbcdmcy) <- paste0("x",names(wbcdmcy)) #rename columns so that spplot understands
    names(wbhdmcy) <- paste0("x",names(wbhdmcy))
    
    years <- names(wbcdmcy)[13:length(names(wbcdmcy))] #make string of all 
    #colnames for input
    
    ##### HDM ######
    #make quantiles based on year with largest value range and use as breaks 
    #for color classification
    maxval <- sapply(c(13:length(names(wbhdmcy))), function(i){ #compute max values for all years
      max(wbhdmcy@data[,i], na.rm=T)
    })
    wmax <- which(maxval==max(maxval)) #which year has largest range?
    
    #prepare classification of weight HDM plot per country and colorkey, orientation: quantiles
    breaks.qt <- classIntervals(wbhdmcy@data[,wmax+12], n = 9, style = "quantile", intervalClosure = "right")
    breaks.qt$brks
    my.at <- breaks.qt$brks
    valmax <- my.at[length(my.at)]
    my.brks=seq(0, valmax, by=valmax/9)
    my.lab=as.character(round(my.brks,0))
    myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.lab))
    
    #plot and write
    pdf(paste0(mappath, "HDM_year_country_weight_before_agg.pdf"),width=16.54, height=11.69)
    spplot(wbhdmcy, years, main="HDM per country, weight before aggregation to 
           countries", cex.main = 2,
           names.attr=yearsnam, lwd=0.2, at = my.at, 
           colorkey=myColorkey)
    dev.off()
    
    ##### CDM ######
    
    #make quantiles based on year with largest value range and use as breaks 
    #for color classification
    maxval <- sapply(c(13:length(names(wbcdmcy))), function(i){ #compute max values for all years
      max(wbcdmcy@data[,i], na.rm=T)
    })
    wmax <- which(maxval==max(maxval)) #which year has largest range?
    
    #prepare classification of weight HDM plot per country and colorkey, orientation: quantiles
    breaks.qt <- classIntervals(wbcdmcy@data[,wmax+12], n = 9, style = "quantile", 
                                intervalClosure = "right")
    breaks.qt$brks
    my.at <- breaks.qt$brks[3:length(breaks.qt$brks)]
    valmax <- my.at[length(my.at)]
    my.brks=seq(0, valmax, by=valmax/9)
    my.lab=as.character(round(my.brks,0))
    myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.lab))
    
    #plot and write
    pdf(paste0(mappath, "CDM_year_country_weight_before_agg.pdf"),width=16.54, height=11.69)
    spplot(wbcdmcy, years, main="CDM per country, weight before aggregation to 
           countries", cex.main = 2,
           names.attr=yearsnam, lwd=0.2, at = my.at, 
           colorkey=myColorkey)
    dev.off()
    
