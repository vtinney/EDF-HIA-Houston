library(raster)
library(rgdal)


# Functions
myZonal <- function (x, z, stat, digits = 0, na.rm = TRUE, 
                     ...) {
  library(data.table)
  fun <- match.fun(stat) 
  vals <- getValues(x) 
  zones <- round(getValues(z), digits = digits) 
  rDT <- data.table(vals, z=zones) 
  setkey(rDT, z) 
  rDT[, lapply(.SD, fun, na.rm = TRUE), by=z] 
} 

ZonalPipe<- function (zone.in, raster.in, shp.out=NULL, stat){
  require(raster)
  require(rgdal)
  require(plyr)
  
  # Load raster
  r <- raster.in
  # Load zone shapefile
  shp <- zone.in
  # Project 'zone' shapefile into the same coordinate system than the input raster
  shp <- spTransform(shp, crs(r))
  
  # Add ID field to Shapefile
  shp@data$ID<-c(1:length(shp@data[,1]))
  
  # Crop raster to 'zone' shapefile extent
  r <- crop(r, extent(shp))	
  # Rasterize shapefile
  zone <- rasterize(shp, r, field="ID", dataType = "INT1U") # Change dataType if nrow(shp) > 255 to INT2U or INT4U
  
  # Zonal stats
  Zstat<-data.frame(myZonal(r, zone, stat))
  colnames(Zstat)<-c("ID", paste0(names(r), "_", c(1:(length(Zstat)-1)), "_",stat))
  
  # Merge data in the shapefile and write it
  shp@data <- plyr::join(shp@data, Zstat, by="ID")
  
  if (is.null(shp.out)){
    return(shp)
  }else{
    writeOGR(shp, shp.out, layer= sub("^([^.]*).*", "\\1", basename(zone.in)), driver="ESRI Shapefile")
  }
}


setwd('/GWSPH/home/vtinney/ho/clip1/')

shp1 <- readOGR(dsn=getwd(), layer='houston_co_1984')
crs(shp1) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


shp2 <- readOGR(dsn=getwd(), layer='gsv_no2_100m_grid')
crs(shp2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


setwd('/GWSPH/home/vtinney/ho/pop1/')


pop.25 <- raster('pop.ls.night.25.ho.tif')
pop.65 <- raster('pop.ls.night.65.ho.tif')


# Ages 25-99 years all of Houston Counties
zone.in <- shp1
raster.in <- pop.25

shp1_df <- ZonalPipe(zone.in, raster.in, stat="sum")

sum(shp1_df$"pop.ls.night.25.ho_1_sum")
#4246967


# Ages 25-99 years GSV drives

zone.in <- shp2
raster.in <- pop.25

shp2_df <- ZonalPipe(zone.in, raster.in, stat="sum")

sum(shp2_df$"pop.ls.night.25.ho_1_sum")
#62872

# Ages 65-99 years all of Houston Counties
zone.in <- shp1
raster.in <- pop.65

shp1_df <- ZonalPipe(zone.in, raster.in, stat="sum")
sum(shp1_df$"pop.ls.night.65.ho_1_sum")
#581817

# Ages 65-99 years GSV drives

zone.in <- shp2
raster.in <- pop.65

shp2_df <- ZonalPipe(zone.in, raster.in, stat="sum")
sum(shp2_df$"pop.ls.night.65.ho_1_sum")
#19262


