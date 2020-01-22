library(raster)
library(rgdal)
library(glue)
library(dplyr)
library(plyr)
library(Rcpp)


#===================================================================================================
# Overall summary statistics
#===================================================================================================
dir <- c('/GWSPH/home/vtinney/ho/results3/bc/af/',
         '/GWSPH/home/vtinney/ho/results3/bc/paf/',
         '/GWSPH/home/vtinney/ho/results3/bc/mr/')


names(dir) <- c('AF','Cases','Risk')

for (i in 1:length(dir)){
  print(dir[i])
  setwd(dir[i])
  files <- list.files(pattern = "\\.tif*", full.names=TRUE)
  for (k in 1:length(files)){
    print(files[k])
    p <- raster(paste(files[k]))
    p[p == 0] <- NA
    p.iqr <- quantile(p)
    p.iqr <- as.matrix(p.iqr)
    p.iqr <- t(p.iqr)
    p.sum <- cellStats(p, 'sum')
    p.mean <- cellStats(p, 'mean')
    paf <- cbind(p.sum, p.mean, p.iqr)
    fout = paste0('/GWSPH/home/vtinney/ho/results3/bc/df/',files[k],names(dir[i]),'.csv',sep='')
    write.csv(paf, fout)
    rm(p)
  }}

setwd('/GWSPH/home/vtinney/ho/results3/bc/df/')
filenames <- list.files(path='/GWSPH/home/vtinney/ho/results3/bc/df/',pattern="*.csv", full.names=TRUE)
dataset.names <- do.call("rbind",llply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")})) #get header names
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
d <- cbind(dataset, dataset.names)
write.csv(d, "bc.results.csv")




#===================================================================================================
# CBG, City and County summary statistics
#===================================================================================================


dir <- c('/GWSPH/home/vtinney/ho/results3/bc/')


allpatterns <- function(fnames, patterns) {
  i <- sapply(fnames, function(fn) all(sapply(patterns, grepl, fn)) )
  fnames[i]
}


# CBG
for (i in 1:length(dir)){
  print(dir[i])
  setwd(dir[i])
  files <- list.files(pattern = "\\cbg.results.csv*", full.names=TRUE)
  for (k in 1:length(files)){
    print(files[k])
    p <- read.csv(paste(files[k]))
    p <- as.data.frame(p)
    p <- distinct(p,GEOID.x, .keep_all= TRUE)
    p$filenames <- p$GEOID.x
    p$filenames <- paste0(files[k],names(dir[i]))
    fout = paste0('/GWSPH/home/vtinney/ho/results3/bc/df/cbg/',files[k],'.csv',sep='')
    write.csv(p, fout)
    rm(p)
  }}

setwd('/GWSPH/home/vtinney/ho/results3/bc/df/cbg/')
filenames <- list.files(path='/GWSPH/home/vtinney/ho/results3/bc/df/cbg/',pattern="*.csv", full.names=TRUE)
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
write.csv(dataset, "bc.cbg.results.csv")

# City 
for (i in 1:length(dir)){
  print(dir[i])
  setwd(dir[i])
  files <- list.files(pattern = "\\city.results.csv*", full.names=TRUE)
  for (k in 1:length(files)){
    print(files[k])
    p <- read.csv(paste(files[k]))
    p <- as.data.frame(p)
    p <- distinct(p,NAMELSAD.x, .keep_all= TRUE)
    p$filenames <- p$NAMELSAD.x
    p$filenames <- paste0(files[k],names(dir[i]))
    fout = paste0('/GWSPH/home/vtinney/ho/results3/bc/df/city/',files[k],'.csv',sep='')
    write.csv(p, fout)
    rm(p)
  }}


setwd('/GWSPH/home/vtinney/ho/results3/bc/df/city/')
filenames <- list.files(path='/GWSPH/home/vtinney/ho/results3/bc/df/city/',pattern="*.csv", full.names=TRUE)
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
write.csv(dataset, "bc.city.results.csv")


# County

for (i in 1:length(dir)){
  print(dir[i])
  setwd(dir[i])
  files <- list.files(pattern = "\\county.results.csv*", full.names=TRUE)
  for (k in 1:length(files)){
    print(files[k])
    p <- read.csv(paste(files[k]))
    p <- as.data.frame(p)
    p <- distinct(p,NAMELSAD.x, .keep_all= TRUE)
    p$filenames <- p$NAMELSAD.x
    p$filenames <- paste0(files[k],names(dir[i]))
    fout = paste0('/GWSPH/home/vtinney/ho/results3/bc/df/county/',files[k],names(dir[i]),'.csv',sep='')
    write.csv(p, fout)
    rm(p)
  }}


setwd('/GWSPH/home/vtinney/ho/results3/bc/df/county/')
filenames <- list.files(path='/GWSPH/home/vtinney/ho/results3/bc/df/county/',pattern="*.csv", full.names=TRUE)
dataset <- do.call("rbind",lapply(filenames,FUN=function(files){read.csv(files, header=TRUE, sep=",")}))
write.csv(dataset, "bc.county.results.csv")