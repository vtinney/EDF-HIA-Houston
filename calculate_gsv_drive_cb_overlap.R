library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(sp)
library(FRK)


setwd('C:/Users/vtinney/Google Drive/EDF_Texas/clip/')
list.files()

cb <- readOGR(dsn=getwd(), layer="cb_houston_gsv_overlap")

no2 <- readOGR(dsn=getwd(), layer='gsv_no2_100m_grid')

no2$area <- round(area(no2,2))

intersection1 <- gIntersection(cb,no2)
intersection2 <- gIntersection(cb,cb)
x <- round(area(intersection1)/area(intersection2)*100,2)
x
#46.37% area overlap between CB's and gsv drive grid cells.