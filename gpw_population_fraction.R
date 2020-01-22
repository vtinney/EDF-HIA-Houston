library(raster)
library(rgdal)

# Create population fractions using LandScan USA for 2017 at 100m resolution
# and GPW v4 age fractions for 2010 (1km reprojected to 100m using Gdal)

#===================================================================================
# Adult and Elderly
#===================================================================================

setwd('C:/Users/vtinney/Google Drive/EDF_Texas/pop/')

x20.24 <- raster("crop.20.24.tif")
x25.29 <- raster("crop.25.29.tif")
x30.34 <- raster("crop.30.34.tif") 
x35.39 <- raster("crop.35.39.tif")
x40.44 <- raster("crop.40.44.tif")
x45.49 <- raster("crop.45.49.tif")
x50.54 <- raster("crop.50.54.tif")
x55.59 <- raster("crop.55.59.tif")
x60.64 <- raster("crop.60.64.tif")
x65.69 <- raster("crop.65.69.tif")
x.65.99 <- raster("crop.65.99.tif")
x70.74 <- raster("crop.70.74.tif")
x75.79 <- raster("crop.75.79.tif")
x80.84 <- raster("crop.80.84.tif")

pop_tot <- raster('total.pop.ho.tif')

#create one for 25-99
x25.99 <- x25.29 + x30.34 + x35.39 + x40.44 + x45.49 + x50.54 + x55.59 + x60.64 + x65.69 + x70.74 + x75.79 + x80.84

#create population fraction by dividing raster stack by the gpw population total
pop_frac_25.99 <- x25.99 / pop_tot
pop_frac_65.99 <- x.65.99 / pop_tot


#import LandScan raster
night <- raster('night_houston.tif')

# Multiply age fractions times total age
night.25.99 <- pop_frac_25.99*night
night.popfrac.65.99 <- pop_frac_65.99*night

#write out conus
writeRaster(night.25.99, filename='ls.night.25.99.tif', format="GTiff", overwrite=TRUE)
writeRaster(night.popfrac.65.99, filename='ls.night.65.99.tif', format="GTiff", overwrite=TRUE)

#===================================================================================
# Pediatric
#===================================================================================

x0.4 <- raster("crop.0.4.tif")
x5.9 <- raster("crop.5.9.tif")  
x10.14 <- raster("crop.10.14.tif") 
x15.19 <- raster("crop.15.19.tif")


#create ages 0-17
frac <- x15.19/5 #divide 5 year age group by 1
frac2 <- frac*2
frac17 <- x15.19-frac2 #subtract one year from age group 15-19 
x0.17 <- x0.4 + x5.9 + x10.14 + frac17 #sum for ages 0-17


#create population fraction by dividing raster stack by the gpw population total

pop_frac_0.17 <- x0.17 / pop_tot

night.popfrac.0.17 <- pop_frac_0.17*night

#write out conus
writeRaster(night.popfrac.0.17, filename='ls.night.17.tif', format="GTiff", overwrite=TRUE)
