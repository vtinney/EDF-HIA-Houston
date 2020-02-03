# Create median and minimum concentration files
# 1.22.2020
library(raster)
library(rgdal)
library(ncdf4)

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

#/////////////////////////////////////////////////////////////////////////

vd.pm <- raster('GlobalGWRwUni_PM25_GL_201601_201612-RH35_Median.nc')
writeRaster(vd.pm, filename='vd.pm.2016.tif',format="GTiff",overwrite=TRUE)


vd.pm <- raster('conc.vd.pm.2016.tif')

vd.pm[vd.pm < 0] <- NA
vd.pm[vd.pm == 0] <- NA

min.vd <- minValue(vd.pm)
max.vd <- maxValue(vd.pm)
med.vd <- quantile(vd.pm, probs=0.5)

vd.2 <- vd.pm
vd.3 <- vd.pm

vd.2[vd.2 >= min.vd] <- min.vd
vd.3[vd.3 >= med.vd] <- med.vd

writeRaster(vd.2, filename='conc.min.vd.pm.tif',format="GTiff",overwrite=TRUE)
writeRaster(vd.3, filename='conc.med.vd.pm.tif',format="GTiff",overwrite=TRUE)



#///////////////////////////////////////////////////////////////////////// - not run need to fix bc outputs
bc.low <- raster("conc.gsv.bc.lower.tif")

bc.low[bc.low == 0] <- NA
min.bc.low <- minValue(bc.low)
med.bc.low <- quantile(bc.low,probs=0.5)
bc.low.2 <- bc.low
bc.low.3 <- bc.low

bc.low.2[bc.low.2 >= min.bc.low] <- min.bc.low
bc.low.3[bc.low.3 >= med.bc.low] <- med.bc.low

writeRaster(bc.low.2, filename='conc.min.bc.gsv.lower.tif',format="GTiff",overwrite=TRUE)
writeRaster(bc.low.3, filename='conc.med.bc.gsv.lower.tif',format="GTiff",overwrite=TRUE)

bc.med <- raster("conc.gsv.bc.med.tif")

bc.med[bc.med == 0] <- NA
min.bc.med <- minValue(bc.med)
med.bc.med <- quantile(bc.med,probs=0.5)
bc.med.2 <- bc.med
bc.med.3 <- bc.med

bc.med.2[bc.med.2 >= min.bc.med] <- min.bc.med
bc.med.3[bc.med.3 >= med.bc.med] <- med.bc.med

writeRaster(bc.med.2, filename='conc.min.bc.gsv.med.tif',format="GTiff",overwrite=TRUE)
writeRaster(bc.med.3, filename='conc.med.bc.gsv.med.tif',format="GTiff",overwrite=TRUE)


bc.upper <- raster("conc.gsv.bc.upper.tif") 

bc.upper[bc.upper == 0] <- NA
min.bc.upper <- minValue(bc.upper)
med.bc.upper <- quantile(bc.upper,probs=0.5)
bc.upper.2 <- bc.upper
bc.upper.3 <- bc.upper

bc.upper.2[bc.upper.2 >= min.bc.upper] <- min.bc.upper
bc.upper.3[bc.upper.3 >= med.bc.upper] <- med.bc.upper

writeRaster(bc.upper.2, filename='conc.min.bc.gsv.upper.tif',format="GTiff",overwrite=TRUE)
writeRaster(bc.upper.3, filename='conc.med.bc.gsv.upper.tif',format="GTiff",overwrite=TRUE)

#///////////////////////////////////////////////////////////////////////// 
no2.low <- raster("conc.gsv.no2.lower.tif")

min.no2.low <- minValue(no2.low)
med.no2.low <- quantile(no2.low, probs=0.5)

no2.low.2 <- no2.low
no2.low.3 <- no2.low

no2.low.2[no2.low.2 >= min.no2.low] <- min.no2.low
no2.low.3[no2.low.3 >= med.no2.low] <- med.no2.low

writeRaster(no2.low.2, filename='conc.min.no2.gsv.lower.tif',format="GTiff",overwrite=TRUE)
writeRaster(no2.low.3, filename='conc.med.no2.gsv.lower.tif',format="GTiff",overwrite=TRUE)


no2.med <- raster("conc.gsv.no2.med.tif")     

min.no2.med <- minValue(no2.med)
med.no2.med <- quantile(no2.med, probs=0.5)

no2.med.2 <- no2.med
no2.med.3 <- no2.med

no2.med.2[no2.med.2 >= min.no2.med] <- min.no2.med
no2.med.3[no2.med.3 >= med.no2.med] <- med.no2.med

writeRaster(no2.med.2, filename='conc.min.no2.gsv.med.tif',format="GTiff",overwrite=TRUE)
writeRaster(no2.med.3, filename='conc.med.no2.gsv.med.tif',format="GTiff",overwrite=TRUE)




no2.upper <- raster("conc.gsv.no2.upper.tif")           

min.no2.upper <- minValue(no2.upper)
med.no2.upper <- quantile(no2.upper, probs=0.5)

no2.upper.2 <- no2.upper
no2.upper.3 <- no2.upper

no2.upper.2[no2.upper.2 >= min.no2.upper] <- min.no2.upper
no2.upper.3[no2.upper.3 >= med.no2.upper] <- med.no2.upper

writeRaster(no2.upper.2, filename='conc.min.no2.gsv.upper.tif',format="GTiff",overwrite=TRUE)
writeRaster(no2.upper.3, filename='conc.med.no2.gsv.upper.tif',format="GTiff",overwrite=TRUE)
