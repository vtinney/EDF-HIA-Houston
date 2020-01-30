# Create median and minimum concentration files
# 1.22.2020
library(raster)
library(rgdal)

setwd('C:/Users/vtinney/Google Drive/EDF_Texas/conc1/')
list.files()


bc <- raster("conc.vanDonkelaar.bc.2016.tif")
no2 <- raster("no2.clip.houston.tif")
pm <- raster("conc.mean.15.16.di.warp.houston.tif")

pm.14.16 <- raster('conc.pm.mean.14.16.tif')
pm.13.16 <- raster('conc.pm.mean.13.16.tif')

bc[bc == 0] <- NA
min.bc <- minValue(bc)
max.bc <- maxValue(bc)
med.bc <- quantile(bc, probs=0.5)

bc2 <- bc
bc3 <- bc

bc2[bc2 >= min.bc] <- min.bc
bc3[bc3 >= med.bc] <- med.bc

writeRaster(bc2, filename='conc.min.bc.ho.tif', format="GTiff", overwrite=TRUE)
writeRaster(bc3, filename='conc.med.bc.ho.tif', format="GTiff", overwrite=TRUE)

pm[pm == 0] <- NA
min.pm <- minValue(pm)
max.pm <- maxValue(pm)
med.pm <- quantile(pm, probs=0.5)

pm2 <- pm
pm3 <- pm

pm2[pm2 >= min.pm] <- min.pm
pm3[pm3 >= med.pm] <- med.pm

writeRaster(pm2, filename='conc.min.pm.ho.tif', format="GTiff", overwrite=TRUE)
writeRaster(pm3, filename='conc.med.pm.ho.tif', format="GTiff", overwrite=TRUE)


pm.14.16[pm.14.16 == 0] <- NA
min.pm.14.16 <- minValue(pm.14.16)
max.pm.14.16 <- maxValue(pm.14.16)
med.pm.14.16 <- quantile(pm.14.16, probs=0.5)

pm2.2 <- pm.14.16
pm3.2 <- pm.14.16

pm2.2[pm2.2 >= min.pm.14.16] <- min.pm.14.16
pm3.2[pm3.2 >= med.pm.14.16] <- med.pm.14.16

writeRaster(pm2.2, filename='conc.min.pm.ho.14.16.tif', format="GTiff", overwrite=TRUE)
writeRaster(pm3.2, filename='conc.med.pm.ho.14.16tif', format="GTiff", overwrite=TRUE)


pm.13.16[pm.13.16 == 0] <- NA
min.pm.13.16 <- minValue(pm.13.16)
max.pm.13.16 <- maxValue(pm.13.16)
med.pm.13.16 <- quantile(pm.13.16, probs=0.5)

pm2.3 <- pm.13.16
pm3.3 <- pm.13.16

pm2.3[pm2.3 >= min.pm.13.16] <- min.pm.13.16
pm3.3[pm3.3 >= med.pm.13.16] <- med.pm.13.16

writeRaster(pm2.3, filename='conc.min.pm.ho.13.16.tif', format="GTiff", overwrite=TRUE)
writeRaster(pm3.3, filename='conc.med.pm.ho.13.16tif', format="GTiff", overwrite=TRUE)




no2[no2 == 0] <- NA
min.no2 <- minValue(no2)
max.no2 <- maxValue(no2)
med.no2 <- quantile(no2, probs=0.5)

no2.2 <- no2
no2.3 <- no2

no2.2[no2.2 >= min.no2] <- min.no2
no2.3[no2.3 >= med.no2] <- med.no2

writeRaster(no2.2, filename='conc.min.no2.ho.tif', format="GTiff", overwrite=TRUE)
writeRaster(no2.3, filename='conc.med.no2.ho.tif', format="GTiff", overwrite=TRUE)
