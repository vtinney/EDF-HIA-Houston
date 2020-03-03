# Combined all run files 
# ==========================================================================================================================================
# Created: 2020-02-02
#
# All read-in files are rasters with the following characteristics: 
# 2367, 2909, 6885603  (nrow, ncol, ncell)
# -123.6325, -121.2083, 36.8925, 38.865  (xmin, xmax, ymin, ymax)
# 0.0008333333, 0.0008333333  (x, y)
# CRS: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
# 
#==================================================================
#Set working directory and load files
setwd('/GWSPH/home/vtinney/ho/results3/no2/')
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(scales)
library(extrafont)
library(maptools)
loadfonts()
library(dplyr)
library(sf)
library(OpenStreetMap)
library(rJava)
library(gridExtra)
library(rgeos)
library(ggpubr)
library(spatialEco)
library(grid)
# ==================================================================
# Specify where the files are
pops <- '/GWSPH/home/vtinney/ho/pop1/'
rates <- '/GWSPH/home/vtinney/ho/rates1/'
concs <- '/GWSPH/home/vtinney/ho/conc1/'
poptotal <- '/GWSPH/home/vtinney/ho/pop1/'
shps <- '/GWSPH/home/vtinney/ho/clip1/'

cbg.groups <- c('cbg_houston')
city.groups <- c('houston_cities')
co.groups <- c('houston_co_1984')

theme_map <- function(...) {
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
    plot.subtitle=element_text(hjust=0, size=9,family="DejaVu Sans Light"),
    plot.caption = element_text(hjust=0, size=7,family="DejaVu Sans Light"),
    legend.title=element_text(size=11, family="DejaVu Sans Light"),
    legend.text=element_text(size=11, family="DejaVu Sans Light"),
    axis.title=element_blank(),
    legend.position = 'bottom',
    legend.justification='center',
    legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    rect = element_blank())
}
#///////////////////////////////////////////////////////////////////////////////////////////////////
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
#///////////////////////////////////////////////////////////////////////////////////////////////////

# ==================================================================
# All health outcomes and NO2
# ==================================================================



# beta.groups <- c(0.022931377,0.009307235,0.031799565, #asthma incidence
#                  0.003907117,0.001124629,0.006649023, #all cause
#                  0.003907117,0.001124629,0.006649023, #all cause
#                  0.005557055,0.003722894,0.009172551, #cvd
#                  0.005557055,0.003722894,0.009172551, #cvd
#                  0.00242825,0.001498016,0.003353905, #asthma er
#                  0.002371653,0.000498754,0.004210118, #asthma er
#                  0.003353905,0.00242825,0.004275024, #asthma er
#                  0.003922071,9.995E-05,0.007788654, #asthma er
#                  0.00227394869694893,0.00207825391825284,0.00256677467485778, #all cause
#                  0.00227394869694893,0.00207825391825284,0.00256677467485778, #all cause
#                  0.00980337402713654,0.00944006754214843,0.0102556588325092, #cvd
#                  0.00980337402713654,0.00944006754214843,0.0102556588325092) #cvd
# names(beta.groups) <- c('Khreis et al. 2017, point estimate', # 1
#                         'Khreis et al. 2017, lower CI',
#                         'Khreis et al. 2017, upper CI',
#                         'Atkinson & Butland 2018, point estimate', # 4
#                         'Atkinson & Butland 2018, lower CI',
#                         'Atkinson & Butland 2018, upper CI',
#                         'Atkinson & Butland 2018, point estimate', #7
#                         'Atkinson & Butland 2018, lower CI',
#                         'Atkinson & Butland 2018, upper CI',
#                         'Atkinson et al. 2018, point estimate', #10
#                         'Atkinson et al. 2018, lower CI',
#                         'Atkinson et al. 2018, upper CI',
#                         'Atkinson et al. 2018, point estimate', #13
#                         'Atkinson et al. 2018, lower CI',
#                         'Atkinson et al. 2018, upper CI',
#                         'Zheng et al. 2015, point estimate', #16
#                         'Zheng et al. 2015, lower CI',
#                         'Zheng et al. 2015, upper CI',
#                         'Orellano et al. 2017, point estimate', #19
#                         'Orellano et al. 2017, lower CI',
#                         'Orellano et al. 2017, upper CI',
#                         'Zheng et al. 2015, point estimate', #22
#                         'Zheng et al. 2015, lower CI',
#                         'Zheng et al. 2015, upper CI',
#                         'Orellano et al. 2017, point estimate', #25
#                         'Orellano et al. 2017, lower CI',
#                         'Orellano et al. 2017, upper CI',
#                         'Eum et al. 2019, point estimate', #28
#                         'Eum et al. 2019, lower CI',
#                         'Eum et al. 2019, upper CI',
#                         'Eum et al. 2019, point estimate', #31
#                         'Eum et al. 2019, lower CI',
#                         'Eum et al. 2019, upper CI',
#                         'Eum et al. 2019, point estimate', #33
#                         'Eum et al. 2019, lower CI',
#                         'Eum et al. 2019, upper CI',
#                         'Eum et al. 2019, point estimate', #36
#                         'Eum et al. 2019, lower CI',
#                         'Eum et al. 2019, upper CI')


beta.groups <- c(0.003907117,0.001124629,0.006649023, #all cause
                 0.005557055,0.003722894,0.009172551, #cvd
                 0.00227394869694893,0.00207825391825284,0.00256677467485778, #all cause
                 0.00980337402713654,0.00944006754214843,0.0102556588325092) #cvd


names(beta.groups) <- c('Atkinson & Butland 2018, point estimate', # 1
                        'Atkinson & Butland 2018, lower CI',
                        'Atkinson & Butland 2018, upper CI',
                        'Atkinson et al. 2018, point estimate', #4
                        'Atkinson et al. 2018, lower CI',
                        'Atkinson et al. 2018, upper CI',
                        'Eum et al. 2019, point estimate', #7
                        'Eum et al. 2019, lower CI',
                        'Eum et al. 2019, upper CI',
                        'Eum et al. 2019, point estimate', #10
                        'Eum et al. 2019, lower CI',
                        'Eum et al. 2019, upper CI')

outcome.groups <- c('All-cause mortality',
                    'All-cause mortality',
                    'All-cause mortality',
                    'CVD mortality',
                    'CVD mortality',
                    'CVD mortality',
                    'All-cause mortality',
                    'All-cause mortality',
                    'All-cause mortality',
                    'CVD mortality',
                    'CVD mortality',
                    'CVD mortality')

rate.groups <- c('ho.all.25',
                 'ho.cvd.25',
                 'ho.all.65',
                 'ho.cvd.65')

names(rate.groups) <- c('ages 25-99 years, County baseline disease rates',
                        'ages 25-99 years, County baseline disease rates',
                        'ages 65-99 years, County baseline disease rates',
                        'ages 65-99 years, County baseline disease rates')


conc.groups <- c("conc.lark.ho","conc.med.no2.ho","conc.min.no2.ho",
                 "conc.no2.gsv.med","conc.no2.gsv.lower","conc.no2.gsv.upper",
                 "conc.med.no2.gsv.med","conc.med.no2.gsv.lower","conc.med.no2.gsv.upper",
                 "conc.min.no2.gsv.lower","conc.min.no2.gsv.med","conc.min.no2.gsv.upper")

names(conc.groups) <- c('Larkin et al. 2017','Larkin et al. 2017 median concentrations','Larkin et al. 2017 minimum concentrations',
                        'GSV','GSV lower CI','GSV upper CI',
                        'GSV median concentrations','GSV median concentrations lower CI','GSV median concentrations upper CI',
                        'GSV minimum concentrations lower CI','GSV minimum concentrations','GSV minimum concentrations upper CI')

pop.groups <- c('pop.ls.night.25.ho','pop.ls.night.65.ho')

names(pop.groups) <- c('LandScan USA, GPWv4 age fractions',
                       'LandScan USA, GPWv4 age fractions')

pdf(NULL)


for (i in 1:length(beta.groups)){
  print(beta.groups[i])
  
  for (j in 1:length(conc.groups)){
    print(conc.groups[j])
    
    if((i == 1 | i == 2 | i == 3 | i == 4 | i == 5 | i == 6 | i == 7 | i == 8 | i == 9 |
        i == 10 | i == 11 | i == 12 ) & (j == 1 | j == 2 | j == 3 )){
      clip.groups <- c('houston_co_1984')
      names(clip.groups) <- c('Houston area')
    }
    
    
    if((i == 1 | i == 2 | i == 3 | i == 4 | i == 5 | i == 6 | i == 7 | i == 8 | i == 9 |
        i == 10 | i == 11 | i == 12 )& (j == 4 | j == 5 | j == 6 | j == 7
                                        | j == 8 | j == 9 | j == 10 | j == 11 | j == 12)){
      clip.groups <- c('gsv_no2_100m_grid')
      names(clip.groups) <- c('GSV drives')
    }
    
    for (m in 1:length(clip.groups)){    
      
      
      if(i == 1 | i == 2 | i == 3){
        b <- raster(paste(rates,rate.groups[1],'.tif',sep=''))
        c = raster(paste(pops,pop.groups[1],'.tif',sep=''))
        rate.names <- names(rate.groups[1])
        pop.names <- names(pop.groups[1])
      }
      
      if(i == 4 | i == 5 | i == 6){
        b <- raster(paste(rates,rate.groups[2],'.tif',sep=''))
        c = raster(paste(pops,pop.groups[1],'.tif',sep=''))
        rate.names <- names(rate.groups[2])
        pop.names <- names(pop.groups[1])
        
      }
      
      if(i == 7 | i == 8 | i == 9){
        b <- raster(paste(rates,rate.groups[3],'.tif',sep=''))
        c = raster(paste(pops,pop.groups[2],'.tif',sep=''))
        rate.names <- names(rate.groups[3])
        pop.names <- names(pop.groups[2])
        
      }
      
      if(i == 10 | i == 11 | i == 12){
        b <- raster(paste(rates,rate.groups[4],'.tif',sep=''))
        c = raster(paste(pops,pop.groups[2],'.tif',sep=''))
        rate.names <- names(rate.groups[4])
        pop.names <- names(pop.groups[2])
        
      }
      
      shp <- readOGR(dsn=shps, layer=paste(clip.groups[m]))
      crs(shp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      
      a = raster(paste(concs,conc.groups[j],'.tif',sep=''))
      a[a==0]<-NA
      
      a <- crop(a, shp)
      a <- mask(a, shp)
      af <- 1-exp(-beta.groups[i]*a)
      
      af2 <- af
      af <- af*100
      
      b[b==0] <- NA
      b <- crop(b, shp)
      b <- mask(b, shp)
      
      mr <- af2*b
      mr[mr==0]<-NA
      
      c[c==0]<- NA
      c <- crop(c, shp)
      c <- mask(c, shp)
      
      hia = overlay(c, b, a, fun=function(r1, r2, r3){return(r1*r2*(10^-4)*(1-exp(-beta.groups[i]*r3)))})
      hia[hia==0]<-NA
      
      
      
      shp.f <- fortify(shp) %>% 
        mutate(id = as.numeric(id))    
      
      af.iqr <- quantile(af)
      af.iqr <- as.matrix(af.iqr)
      af.iqr <- t(af.iqr)
      af.mean <- cellStats(af, 'mean')
      af.df <- cbind(af.mean, af.iqr)
      
      print(paste(names(clip.groups[m]),', attributable fraction, ',names(conc.groups[j]),', ',names(beta.groups[i]),sep=''))
      print(af.df)
      
      f1 = paste('/GWSPH/home/vtinney/ho/results3/no2/af/',names(clip.groups[m]),', ',outcome.groups[i],', ',names(conc.groups[j]),', ',names(beta.groups[i]),'.tif',sep='')
      
      writeRaster(af, filename=f1, format="GTiff", overwrite=TRUE)
      
      min.af <- minValue(af)
      max.af <- maxValue(af)
      min.af.label <- round(minValue(af),2)
      max.af.label <- round(maxValue(af),2)
      mean.af <- (min.af+max.af)/2
      mean.af.label <- round(mean.af,2)
      
      af.log <- log(af)
      z.af <- scale(af.log)
      
      af.df <- rasterToPoints(af)
      af.df <- data.frame(af.df)
      colnames(af.df) <- c('lon','lat','val')
      
      base <- openmap(c(ymin(shp),xmin(shp)),c(ymax(shp),xmax(shp)),
                      type = "esri-topo",
                      mergeTiles = TRUE)
      base <- openproj(base, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      if((i == 1 | i == 4 | i == 7 | i == 10) & (j == 1 | j == 2 | j == 4 | j == 7)){
        
        autoplot(base)  +
          geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                       fill="grey50",alpha=0.5)+
          geom_tile(data=af.df,aes(lon, lat, fill = val),alpha=0.8) +
          scale_fill_gradient2("Attributable Fraction (%)",
                               low = "#3ec267", 
                               mid = "#fff429",  #ff7e29
                               high = "#fc0339", ##ff1f40
                               midpoint = mean.af,
                               breaks=c(min.af,mean.af,max.af),
                               labels=c(min.af.label,mean.af.label,max.af.label),
                               limits=c(min.af, max.af),
                               na.value = 'grey50',
                               guide = guide_colourbar(
                                 direction = "horizontal",
                                 label=TRUE,
                                 keyheight = unit(2, units = "mm"),
                                 title.position = 'top',
                                 title.hjust = 0.5,
                                 label.hjust = 0.5,
                                 barwidth = 15,
                                 nrow = 1,
                                 byrow = T,
                                 label.position = "bottom"))+
          theme_map()+
          geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                    color = "grey60", size = 0.5)+
          labs(title='Attributable fraction',
               caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),'.',sep=''),
               subtitle=paste0('Range: ',min.af.label,' to ',max.af.label,'%.'),sep='')
        ggsave(paste0(names(clip.groups[m]),' AF ',outcome.groups[i],' ',names(beta.groups[i]),' ',names(conc.groups[j]),'.af.png',sep=''),dpi=320)
        print('af')
      }
      #==============================================================================================================================================================    
      
      
      f2 = paste('/GWSPH/home/vtinney/ho/results3/no2/mr/',names(clip.groups[m]),', ',outcome.groups[i],', ',names(conc.groups[j]),', ',names(beta.groups[i]),', ',rate.names,'.tif',sep='')
      
      writeRaster(mr, filename=f2, format="GTiff", overwrite=TRUE)
      
      min.mr <- minValue(mr)
      min.mr.label <- round(minValue(mr),2)
      max.mr <- maxValue(mr)
      max.mr.label <- round(maxValue(mr),2)
      mean.mr <- (min.mr+max.mr)/2
      mean.mr.label <- round(mean.mr,2)
      
      mr.df <- rasterToPoints(mr)
      mr.df <- data.frame(mr.df)
      colnames(mr.df) <- c('lon','lat','val')
      
      if((i == 1 | i == 4 | i == 7 | i == 10) & (j == 1 | j == 2 | j == 4 | j == 7)){
        autoplot(base)  +
          geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                       fill="grey50",alpha=0.5)+
          geom_tile(data=mr.df,aes(lon, lat, fill = val),alpha=0.8) +
          scale_fill_gradient2("Risk per 10,000",
                               low = "#3ec267", 
                               mid = "#fff429",  #ff7e29
                               high = "#fc0339", ##ff1f40
                               midpoint = mean.mr,
                               breaks=c(min.mr,mean.mr,max.mr),
                               labels=c(min.mr.label,mean.mr.label,max.mr.label),
                               limits=c(min.mr, max.mr),
                               na.value = 'grey50',
                               guide = guide_colourbar(
                                 direction = "horizontal",
                                 label=TRUE,
                                 keyheight = unit(2, units = "mm"),
                                 title.position = 'top',
                                 title.hjust = 0.5,
                                 label.hjust = 0.5,
                                 barwidth = 15,
                                 nrow = 1,
                                 byrow = T,
                                 label.position = "bottom"))+
          theme_map()+
          geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                    color = "grey60", size = 0.5)+
          labs(title=paste0('Risk of ',outcome.groups[i],sep=''),
               caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',rate.names,'.',sep=''),
               subtitle=paste0('Range: ',min.mr.label,' to ',max.mr.label,' per 10,000. '),sep='')
        ggsave(paste0(names(clip.groups[m]),' MR ',outcome.groups[i],' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',rate.names,'.af.png',sep=''),dpi=320)
        print('mr')
      }
      #===========================================================================================================================      
      # Crop and map HIA files
      
      hia <- crop(hia, shp)
      hia <- mask(hia, shp)
      
      f3 = paste('/GWSPH/home/vtinney/ho/results3/no2/paf/',names(clip.groups[m]),', ',outcome.groups[i],', ',names(conc.groups[j]),', ',names(beta.groups[i]),', ',rate.names,', ',pop.names,'.tif',sep='')
      writeRaster(hia, filename=f3, format="GTiff", overwrite=TRUE)
      
      hia.df <- rasterToPoints(hia)
      hia.df <- data.frame(hia.df)
      colnames(hia.df) <- c('lon','lat','val')
      
      
      min.hia <- minValue(hia)
      max.hia <- maxValue(hia)
      min.hia.label <- round(minValue(hia),2)
      max.hia.label <- round(maxValue(hia),2)
      mean.hia <- (min.hia+max.hia)/2
      mean.hia.label <- round(mean.hia,2)
      
      if((i == 1 | i == 4 | i == 7 | i == 10) & (j == 1 | j == 2 | j == 4 | j == 7)){
        autoplot(base)  +
          geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                       fill="grey50",alpha=0.5)+
          geom_tile(data=hia.df,aes(lon, lat, fill = val),alpha=0.8) +
          scale_fill_gradient2("Excess cases (n)",
                               low = "#3ec267", 
                               mid = "#fff429",  #ff7e29
                               high = "#fc0339", ##ff1f40
                               midpoint = mean.hia,
                               breaks=c(min.hia,mean.hia,max.hia),
                               labels=c(min.hia.label,mean.hia.label,max.hia.label),
                               limits=c(min.hia, max.hia),
                               na.value = 'grey50',
                               guide = guide_colourbar(
                                 direction = "horizontal",
                                 label=TRUE,
                                 keyheight = unit(2, units = "mm"),
                                 title.position = 'top',
                                 title.hjust = 0.5,
                                 label.hjust = 0.5,
                                 barwidth = 15,
                                 nrow = 1,
                                 byrow = T,
                                 label.position = "bottom"))+
          theme_map()+
          geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                    color = "grey60", size = 0.5)+
          labs(title=paste0(outcome.groups[i],' cases attributable to nitrogen dioxide.',sep=''),
               caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',rate.names,',\n',pop.names,'.',sep=''),
               subtitle=paste0('Range: ',min.hia.label,' to ',max.hia.label,' per grid cell. '),sep='')
        ggsave(paste0(names(clip.groups[m]),' PAF ',outcome.groups[i],' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',pop.names,' ',rate.names,'.af.png',sep=''),dpi=300)
        print('hia')
      }
      #=========================================================================================================================
      if ((i == 1 | i == 2 | i == 3 | i == 4 | i == 5 | i == 6 | i == 7 | i == 8 | i == 9 |
           i == 10 | i == 11 | i == 12 ) & (j == 1 | j == 2)){
        
        
        cbg.shp <- readOGR(dsn=shps, layer=paste(cbg.groups))
        crs(cbg.shp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        
        cbg.shp <- crop(cbg.shp, shp)
        
        cbg.shp.f <- fortify(cbg.shp) %>% 
          mutate(id = as.numeric(id))
        
        zone.in <- cbg.shp
        raster.in <- hia
        
        shp2 <- ZonalPipe(zone.in, raster.in, stat="sum")
        shp2@data <- shp2@data %>% mutate(id = row.names(.))
        shp_df <- fortify(shp2, region = "id")
        shp_df <- shp_df %>% left_join(shp2@data, by = c("id"="id"))
        shp_df <- as.data.frame(shp_df)
        shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA
        r.min <- min(shp_df[,ncol(shp_df)],na.rm=TRUE)
        r.max <- max(shp_df[,ncol(shp_df)],na.rm=TRUE)
        r.med <- median(shp_df[,ncol(shp_df)],na.rm=TRUE)
        colnames(shp_df)[ncol(shp_df)] <- 'hia.val'
        
        r.mean <- (r.min+r.max)/2
        r.mean.label <- round(r.mean,2)
        r.min.label <- round(r.min,2)
        r.max.label <- round(r.max,2)
        r.med.label <- round(r.med,2)
        
        zone.in <- cbg.shp 
        raster.in <- c
        
        shp3 <- ZonalPipe(zone.in, raster.in, stat="sum")
        shp3@data <- shp3@data %>% mutate(id = row.names(.))
        pop_df <- fortify(shp3, region = "id")
        pop_df <- pop_df %>% left_join(shp3@data, by = c("id"="id"))
        pop_df <- as.data.frame(pop_df)
        pop_df[,ncol(pop_df)][pop_df[,ncol(pop_df)] == 0] <- NA
        colnames(pop_df)[ncol(pop_df)] <- "pop.val"
        
        rate_df <- merge(shp_df,pop_df,by='order')
        rate_df <- as.data.frame(rate_df)
        rate_df$rate <- NA
        rate_df$rate <- (rate_df$hia.val*100000)/rate_df$pop.val
        #rate_df$rate[rate_df$rate==0]<-NA
        rate.min <- min(rate_df[,ncol(rate_df)],na.rm=TRUE)
        rate.max <- max(rate_df[,ncol(rate_df)],na.rm=TRUE)
        rate.med <- median(rate_df[,ncol(rate_df)],na.rm=TRUE)
        rate.min.label <- round(rate.min,2)
        rate.max.label <- round(rate.max,2)
        rate.med.label <- round(rate.med,2)
        rate.mean <- (rate.min+rate.max)/2
        rate.mean.label <- round(rate.mean,2)
        
        write.csv(rate_df, paste(names(clip.groups[m]),',',outcome.groups[i],',',names(beta.groups[i]),',',names(conc.groups[j]),',',pop.names,',',rate.names,'cbg.results.csv'))
      }
      if((i == 1 | i == 4 | i == 7 | i == 10) & (j == 1 | j == 2)){
        
        # Map of excess per grid cell
        e <- autoplot(base)  +
          geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = shp_df[,ncol(shp_df)]),alpha=0.7)+
          scale_fill_gradient2("Count (n) cases \n per Census Block Group",
                               low = "#3ec267", 
                               mid = "#fff429",  #ff7e29
                               high = "#fc0339", ##ff1f40
                               midpoint = r.mean,
                               na.value='grey50',
                               breaks=c(r.min,r.mean,r.max),
                               labels=c(r.min.label,r.mean.label,r.max.label),
                               limits=c(r.min, r.max),
                               guide = guide_colourbar(
                                 direction = "horizontal",
                                 label=TRUE,
                                 keyheight = unit(2, units = "mm"),
                                 title.position = 'top',
                                 title.hjust = 0.5,
                                 label.hjust = 0.5,
                                 barwidth = 15,
                                 nrow = 1,
                                 byrow = T,
                                 label.position = "bottom"))+
          theme_map()+
          geom_path(data = cbg.shp.f, aes(x = long, y = lat, group = group), 
                    color = "grey60", size = 0.1)+
          labs(title=paste0(outcome.groups[i],' cases attributable to nitrogen dioxide.',sep=''),
               caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',rate.names,', \n',pop.names,'.',sep=''),
               subtitle=paste0('Range: ',r.min.label,' to ',r.max.label,' per CBG. '),sep='')
        ggsave(paste0(names(clip.groups[m]),' PAF ',outcome.groups[i],' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',pop.names,' ',rate.names,'.count.cbg.png',sep=''),dpi=320)
        print('count.cbg')
        
        autoplot(base)  +
          geom_polygon(data = rate_df, aes(x = long.x, y = lat.x, group = group.x, fill = rate_df$rate),alpha=0.7)+
          scale_fill_gradient2("Rate per 100,000\nper Census Block Group",
                               low = "#3ec267", 
                               mid = "#fff429",  #ff7e29
                               high = "#fc0339", ##ff1f40
                               midpoint = rate.mean,
                               na.value='grey50',
                               breaks=c(rate.min,rate.mean,rate.max),
                               labels=c(rate.min.label,rate.mean.label,rate.max.label),
                               limits=c(rate.min, rate.max),
                               guide = guide_colourbar(
                                 direction = "horizontal",
                                 label=TRUE,
                                 keyheight = unit(2, units = "mm"),
                                 title.position = 'top',
                                 title.hjust = 0.5,
                                 label.hjust = 0.5,
                                 barwidth = 15,
                                 nrow = 1,
                                 byrow = T,
                                 label.position = "bottom"))+
          theme_map()+
          geom_path(data = cbg.shp.f, aes(x = long, y = lat, group = group), 
                    color = "grey60", size = 0.1)+
          labs(title=paste0(outcome.groups[i],' cases attributable to nitrogen dioxide.',sep=''),
               caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',rate.names,', \n',pop.names,'.',sep=''),
               subtitle=paste0('Range: ',rate.min.label,' to ',rate.max.label,' per 100,000. '),sep='')
        ggsave(paste0(names(clip.groups[m]),' PAF ',outcome.groups[i],' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',pop.names,' ',rate.names,'.rate.cbg.png',sep=''),dpi=320)
        print('rate.cbg')
      }
      
      
      #///////////////////////////////////////////////////////////////////////////
      # County aggregation
      
      if ((i == 1 | i == 2 | i == 3 | i == 4 | i == 5 | i == 6 |
           i == 10 | i == 11 | i == 12 ) & (j == 1 | j == 2)){
        
        co.groups <- c('houston_co_1984')
        
        co.shp <- readOGR(dsn=shps, layer=paste(co.groups))
        crs(co.shp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        
        co.shp <- crop(co.shp, shp)
        
        co.shp.f <- fortify(co.shp) %>% 
          mutate(id = as.numeric(id))
        
        zone.in <- co.shp
        raster.in <- hia
        
        shp2 <- ZonalPipe(zone.in, raster.in, stat="sum")
        shp2@data <- shp2@data %>% mutate(id = row.names(.))
        shp_df <- fortify(shp2, region = "id")
        shp_df <- shp_df %>% left_join(shp2@data, by = c("id"="id"))
        shp_df <- as.data.frame(shp_df)
        shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA
        r.min <- min(shp_df[,ncol(shp_df)],na.rm=TRUE)
        r.max <- max(shp_df[,ncol(shp_df)],na.rm=TRUE)
        r.med <- median(shp_df[,ncol(shp_df)],na.rm=TRUE)
        colnames(shp_df)[ncol(shp_df)] <- 'hia.val'
        
        r.mean <- (r.min+r.max)/2
        r.mean.label <- round(r.mean,2)
        r.min.label <- round(r.min,2)
        r.max.label <- round(r.max,2)
        r.med.label <- round(r.med,2)
        
        zone.in <- co.shp #======CHANGE=====#
        raster.in <- c
        
        shp3 <- ZonalPipe(zone.in, raster.in, stat="sum")
        shp3@data <- shp3@data %>% mutate(id = row.names(.))
        pop_df <- fortify(shp3, region = "id")
        pop_df <- pop_df %>% left_join(shp3@data, by = c("id"="id"))
        pop_df <- as.data.frame(pop_df)
        pop_df[,ncol(pop_df)][pop_df[,ncol(pop_df)] == 0] <- NA
        colnames(pop_df)[ncol(pop_df)] <- "pop.val"
        
        rate_df <- merge(shp_df,pop_df,by='order')
        rate_df <- as.data.frame(rate_df)
        rate_df$rate <- NA
        rate_df$rate <- (rate_df$hia.val*100000)/rate_df$pop.val
        #rate_df$rate[rate_df$rate==0]<-NA
        rate.min <- min(rate_df[,ncol(rate_df)],na.rm=TRUE)
        rate.max <- max(rate_df[,ncol(rate_df)],na.rm=TRUE)
        rate.med <- median(rate_df[,ncol(rate_df)],na.rm=TRUE)
        rate.min.label <- round(rate.min,2)
        rate.max.label <- round(rate.max,2)
        rate.med.label <- round(rate.med,2)
        rate.mean <- (rate.min+rate.max)/2
        rate.mean.label <- round(rate.mean,2)
        
        write.csv(rate_df, paste(names(clip.groups[m]),',',outcome.groups[i],',',names(beta.groups[i]),',',names(conc.groups[j]),',',pop.names,',',rate.names,'county.results.csv'))
        
        rm(co.shp)
      }
      
      
    }
    rm(shp)
    rm(cbg.shp)
    rm(c)
    rm(hia)
    rm(b)
    rm(mr)
    rm(af)
    rm(af2)
    rm(a)
  }
  rm(clip.groups)
}





