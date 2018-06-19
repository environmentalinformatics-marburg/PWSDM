library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# read data
main <- "E:/populationclimate/"
datapath <- paste0(main, "reprojected/")
pop <- readRDS(file=paste0(datapath, "pop.rds"))
temp <- readRDS(file=paste0(datapath, "temp.rds"))

##### linear interpolation of population data 1970-2015 #######################

#create NA-raster as template
template <- pop[[4]]
names(template) <- "template"
template[] <- NA
plot(template)

#construct stack for all years 1970-2015
allpop <- stack(pop[[1]], stack(mget(rep("template",9))) , pop[[2]],
                stack(mget(rep("template",9))), pop[[3]],pop[[4]],
                stack(mget(rep("template",4))), pop[[5]],
                stack(mget(rep("template",4))), pop[[6]],
                stack(mget(rep("template",4))), pop[[7]],
                stack(mget(rep("template",4))), pop[[8]],
                stack(mget(rep("template",4))), pop[[9]])
nam <- names(allpop) 
nam[2:10] <- paste0("linint_", c(71:79))
nam[12:20] <- paste0("linint_", c(81:89))
nam[23:26] <- paste0("linint_", c(91:94))
nam[28:31] <- paste0("linint_", c(96:99))
nam[33:36] <- paste0("linint_0", c(1:4))
nam[38:41] <- paste0("linint_0", c(6:9))
nam[43:46] <- paste0("linint_0", c(11:14))

names(allpop) <- nam

#interpolation. Distance between timesteps = 1/nlayers of input stack, 
#for that reason calculation in segments
int70 <- raster::approxNA(allpop[[1:11]], method="linear")
int80 <- raster::approxNA(allpop[[11:21]], method="linear")
int90 <- raster::approxNA(allpop[[22:27]], method="linear")
int95 <- raster::approxNA(allpop[[27:32]], method="linear")
int00 <- raster::approxNA(allpop[[32:37]], method="linear")
int05 <- raster::approxNA(allpop[[37:42]], method="linear")
int10 <- raster::approxNA(allpop[[42:47]], method="linear")

popcomp <- stack(int70[[1:10]],int80[[1:10]],int90[[1:5]], int95[[1:5]],
      int00[[1:5]],int05[[1:5]],int10[[1:6]])

saveRDS(popcomp, file=paste0(main, "reprojected/complete_population.rds"))

