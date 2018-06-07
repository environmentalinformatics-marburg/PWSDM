library(raster)
library(ncdf4)
library(rgdal)
library(mapview)

# Test CRU
path = "C:/Users/tnauss/permanent/plygrnd/populationclimate/CRU 4.01 TS/cru_ts4.01.1971.1980.tmp.dat.nc"
nv = stack(paste0(path, "/cru_ts4.01.1971.1980.tmp.dat.nc"))


# Test Hyde
path = "C:/Users/tnauss/permanent/plygrnd/populationclimate/Hyde Data"
hyde = stack(paste0(path, "/popd_1970AD.asc"))
hyde
plot(hyde[[1]])

# Test GPWV3
path = "C:/Users/tnauss/permanent/plygrnd/populationclimate/GPWV3/gl_gpwfe_pdens_05_ascii_half"
gpwv = stack(paste0(path, "/glds05ag30.asc"))
gpwv
plot(gpwv[[1]])

# Reproject CRU and HYDE to GPWV3 geometry