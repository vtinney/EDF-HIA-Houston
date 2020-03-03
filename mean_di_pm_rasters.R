library(raster)
library(rgdal)

setwd('C:/Users/vtinney/Google Drive/EDF_Texas/di_annual_pm/')
list.files()


x2013 <- raster("2013.tif")
x2014 <- raster("2014.tif")
x2015 <- raster("2015.tif")
x2016 <- raster("2016.tif")

x.13.16 <- (x2013+x2014+x2015+x2016)/4
x.14.16 <- (x2014+x2015+x2016)/3
x.15.16 <- (x2015+x2016)/2
x.13.15 <- (x2013+x2014+x2015)/3

setwd('C:/Users/vtinney/Google Drive/EDF_Texas/conc1/')

writeRaster(x.15.16, filename='di.pm.mean.15.16.tif',format="GTiff",overwrite=TRUE)
writeRaster(x.14.16, filename='di.pm.mean.14.16.tif', format="GTiff", overwrite=TRUE)
writeRaster(x.13.16, filename='di.pm.mean.13.16.tif',format="GTiff",overwrite=TRUE)
writeRaster(x.13.15, filename='di.pm.mean.13.15.tif',format="GTiff",overwrite=TRUE)
