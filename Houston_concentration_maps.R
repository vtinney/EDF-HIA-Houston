library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(scales)
library(extrafont)
loadfonts()
library(dplyr)
library(sf)
library(OpenStreetMap)
library(rJava)

# input shapefiles
setwd('/GWSPH/home/vtinney/ho/clip1/')

ho <- readOGR(dsn=getwd(), layer='houston_co_1984')
ho.f <- fortify(ho) %>% 
  mutate(id = as.numeric(id))


# input shapefiles
setwd('/GWSPH/home/vtinney/ho/conc1/')
ho.lark <- raster('conc.lark.ho.tif')
ho.pm <- raster('conc.mean.15.16.ho.tif')
bc.vd <- raster('conc.bc.ho.tif')

ho.lark <- crop(ho.lark, ho)
ho.lark <- mask(ho.lark, ho)
ho.pm <- crop(ho.pm, ho)
ho.pm <- mask(ho.pm, ho)

ho.bc.vd <- crop(bc.vd, ho)
ho.bc.vd <- mask(ho.bc.vd, ho)


# NO2 Lark: 1-37
# PM: 1.96-14

map <- openmap(c(28.825000,-96.622500), c(30.630833,-94.353333),
               type = "esri-topo",
               mergeTiles = TRUE)

map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#============================================================================================

r <- ho.lark

r[r == 0] <- NA
r.min <- minValue(r)
r.max <- maxValue(r)
min.r.label <- round(r.min,0)
max.r.label <- round(r.max,0)
df <- rasterToPoints(r)
df <- as.data.frame(df)
colnames(df) <- c('lon','lat','val')
r.mean <- round((r.min+r.max)/2,0)


autoplot(map.latlon)  +
  geom_polygon(data = ho.f, aes(x = long, y = lat, group = group), 
               fill="grey50",alpha=0.5)+
  geom_tile(data=df,aes(lon, lat, fill = val),alpha=0.8) +
  scale_fill_gradient2(expression(paste(ppb)),
                       low = "#3ec267", 
                       mid = "#fff429",  #ff7e29
                       high = "#fc0339", ##ff1f40
                       midpoint = r.mean,
                       breaks=c(min.r.label,r.mean,max.r.label),
                       limits=c(r.min, r.max),
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
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(hjust = 0, size=13),
        plot.subtitle=element_text(hjust=0, size=11),
        plot.caption = element_text(hjust=0, size=11),
        legend.title=element_text(size=11),
        legend.text=element_text(size=11),
        axis.title=element_blank(),
        legend.position = 'bottom',
        legend.justification='center',
        legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        rect = element_blank())+
  geom_path(data = ho.f, aes(x = long, y = lat, group = group), 
            color = "grey60", size = 0.5)+
  labs(title='Houston area nitrogen dioxide concentrations.',
       subtitle='Larkin et al. 2017, annual average concentrations for 2011.',
       caption=paste0('Range: ',min.r.label,' to ',max.r.label,' ppb. Extent: -96.6225, -94.35333, 28.82566, 30.63017.',sep=''))
ggsave('ho.larkin.png',dpi=320)
ggsave('ho.larkin.pdf')


#============================================================================================
r <- ho.bc.vd

r[r == 0] <- NA
df <- rasterToPoints(r)
df <- as.data.frame(df)
colnames(df) <- c('lon','lat','val')

autoplot(map.latlon)  +
  geom_polygon(data = ho.f, aes(x = long, y = lat, group = group), 
               fill="grey50",alpha=0.5)+
  geom_tile(data=df,aes(lon, lat, fill = val),alpha=0.8) +
  scale_fill_gradient2(expression(paste(µg/m^3)),
                       low = "#3ec267", 
                       mid = "#fff429",  #ff7e29
                       high = "#fc0339", ##ff1f40
                       midpoint = 0.5,
                       breaks=c(0.3,0.5,0.7),
                       limits=c(0.3, 0.7),
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
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(hjust = 0, size=13),
        plot.subtitle=element_text(hjust=0, size=11),
        plot.caption = element_text(hjust=0, size=11),
        legend.title=element_text(size=11),
        legend.text=element_text(size=11),
        axis.title=element_blank(),
        legend.position = 'bottom',
        legend.justification='center',
        legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        rect = element_blank())+
  geom_path(data = ho.f, aes(x = long, y = lat, group = group), 
            color = "grey60", size = 0.5)+
  labs(title='Houston area black carbon concentrations.',
       subtitle='van Donkelaar et al. 2019, annual average concentrations for 2016.',
       caption=expression(paste('Range: 0.3 to 0.7 ',µg/m^3,'. Extent: -96.6225, -94.35333, 28.82566, 30.63017.',sep='')))
ggsave('ho.vd.png',dpi=320)
ggsave('ho.vd.pdf')


#============================================================================================

r <- ho.pm

r[r == 0] <- NA
r.min <- minValue(r)
r.max <- maxValue(r)
min.r.label <- round(r.min,0)
max.r.label <- round(r.max,0)
df <- rasterToPoints(r)
df <- as.data.frame(df)
colnames(df) <- c('lon','lat','val')
r.mean <- round((r.min+r.max)/2,0)


autoplot(map.latlon)  +
  geom_polygon(data = ho.f, aes(x = long, y = lat, group = group), 
               fill="grey50",alpha=0.5)+
  geom_tile(data=df,aes(lon, lat, fill = val),alpha=0.8) +
  scale_fill_gradient2(expression(paste(µg/m^3)),
                       low = "#3ec267", 
                       mid = "#fff429",  #ff7e29
                       high = "#fc0339", ##ff1f40
                       midpoint = 9.5,
                       breaks=c(5.5,9.5,13.75),
                       limits=c(5.5, 13.75),
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
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(hjust = 0, size=13),
        plot.subtitle=element_text(hjust=0, size=11),
        plot.caption = element_text(hjust=0, size=11),
        legend.title=element_text(size=11),
        legend.text=element_text(size=11),
        axis.title=element_blank(),
        legend.position = 'bottom',
        legend.justification='center',
        legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        rect = element_blank())+
  geom_path(data = ho.f, aes(x = long, y = lat, group = group), 
            color = "grey60", size = 0.5)+
  labs(title='Houston area fine particulate matter concentrations.',
       subtitle='Di et al. 2016, annual average concentrations for 2015-2016.',
       caption=expression(paste('Range: 5.5, to 13.7 ',µg/m^3,'. Extent: -96.6225, -94.35333, 28.82566, 30.63017.',sep='')))
ggsave('ho.pm.png',dpi=320)
ggsave('ho.pm.pdf')
