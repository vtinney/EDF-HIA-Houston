#/////////////////////////////////////////////////////////////////////////////////////////////////

# This is a script to: 1) text to columns of 02.df.R files and
# 2) create a long DF (estimates spread across columns per result)
# for both full results and CBG results for BC

# Date: 2020-03-03
# Author: V Southerland

#/////////////////////////////////////////////////////////////////////////////////////////////////

library(splitstackshape)
library(raster)
library(rgdal)
library(dplyr)
library(tidyverse)
library(readxl)


setwd("/GWSPH/home/vtinney/ho/results3/no2/df/Final/")


# list.files()
#[1] "no2.cbg.results.csv"    "no2.city.results.csv"   "no2.county.results.csv"
#[4] "no2.results.csv"


#/////////////////////////////////////////////////////////////////////////////////////////////////
# Part 1 - text to columns of all aggregated results
df <- read.csv('no2.results.csv')

# Text to columns
full0 <- cSplit(df, 1:ncol(df), sep="/", stripWhite=TRUE, type.convert=FALSE)
full <- cSplit(full0, 90:ncol(full0), sep=",", stripWhite=TRUE, type.convert=FALSE)


# Keep these column names
#p.sum_01 #21
#p.mean_01 #31
#X0._01 #41
#X25._01 #51
#X50._01 #61
#X75._01 #71
#X100._01 #81

# [178] "dataset.names_09_8" "dataset.names_09_9" "dataset.names_10_1" <- start here
# [181] "dataset.names_10_2" "dataset.names_10_3" "dataset.names_10_4"
# [184] "dataset.names_10_5" "dataset.names_10_6" "dataset.names_10_7"
# [187] "dataset.names_10_8" "dataset.names_10_9"


full2 <- full[,c(21,31,41,51,61,71,81,180:188)]

# Rename
names(full2) <- c('sum','mean','min','q25','median','q75','max','Extent','Outcome','Concentrations','CRF','Estimates','Age groups',
                  'Baseline disease rates','Population dataset','Population fraction')

# Make new analysis column
full2$Analysis[full2$Estimates == 'lower CI.tifAF.csv'] <- 'AF'
full2$Analysis[full2$Estimates == 'upper CI.tifAF.csv'] <- 'AF'
full2$Analysis[full2$Estimates == 'point estimate.tifAF.csv'] <- 'AF'

full2$Estimates[full2$Estimates == 'lower CI.tifAF.csv'] <- 'lower CI'
full2$Estimates[full2$Estimates == 'upper CI.tifAF.csv'] <- 'upper CI'
full2$Estimates[full2$Estimates == 'point estimate.tifAF.csv'] <- 'point estimate'

full2$Analysis[full2$'Baseline disease rates' == 'CBG baseline disease rates.tifRisk.csv'] <- 'Risk'
full2$Analysis[full2$'Baseline disease rates' == 'County baseline disease rates.tifRisk.csv'] <- 'Risk'

full2$'Baseline disease rates'[full2$'Baseline disease rates' == 'CBG baseline disease rates.tifRisk.csv'] <- 'CBG baseline disease rates'
full2$'Baseline disease rates'[full2$'Baseline disease rates' == 'County baseline disease rates.tifRisk.csv'] <- 'County baseline disease rates'

full2$Analysis[full2$'Population fraction' == 'GPWv4 age fractions.tifCases.csv'] <- 'Cases'
full2$'Population fraction'[full2$'Population fraction' == 'GPWv4 age fractions.tifCases.csv'] <- 'GWPv4 age fractions'

comb <- full2


#/////////////////////////////////////////////////////////////////////////////////////////////////

#Alameda County	 ages 0-17 years		377886.9
#Alameda County	 ages 25-99 years		1090503
#Alameda County	 ages 65-99 years		183904.1
#Alameda County	all ages 1663085
#Bay area	 ages 0-17 years		1721994
#Bay area	 ages 25-99 years		5166662
#Bay area	 ages 65-99 years		947944.7
#Bay area all ages 7755519 
#East Oakland	 ages 0-17 years		9308.006
#East Oakland	 ages 25-99 years		42323.16
#East Oakland	 ages 65-99 years		7981.142
#East Oakland all ages  58906
#Oakland	 ages 0-17 years		28152.86
#Oakland	 ages 25-99 years		78936.75
#Oakland	 ages 65-99 years		13140.52
#Oakland all ages 122246

# Associate each area of analysis with a total population


#comb$pop.total[comb$'Age groups' == "ages 0-17 years" & comb$Extent == 'Alameda County'] <- 377886.9
comb$pop.total[comb$'Age groups' == "ages 25-99 years" & comb$Extent == 'GSV drives'] <- 62872
comb$pop.total[comb$'Age groups' == "ages 65-99 years" & comb$Extent == 'GSV drives'] <- 19262
#comb$pop.total[comb$'Age groups' == "all ages" & comb$Extent == 'Alameda County'] <- 1663085

#comb$pop.total[comb$'Age groups' == "ages 0-17 years" & comb$Extent == 'Bay area'] <- 1721994
comb$pop.total[comb$'Age groups' == "ages 25-99 years" & comb$Extent == 'Houston area'] <- 4246967
comb$pop.total[comb$'Age groups' == "ages 65-99 years" & comb$Extent == 'Houston area'] <- 581817
#comb$pop.total[comb$'Age groups' == "all ages" & comb$Extent == 'Bay area'] <- 7755519


#/////////////////////////////////////////////////////////////////////////////////////////////////

# Create a rate per 100,000
comb$sum <- as.numeric(comb$sum)
comb$rate.100 <- (comb$sum*100000)/comb$pop.total

comb <- comb[,c(8:11,13:18,12,1:7,19)]

# Gather results by type
long_DF <- comb %>% gather(Anal, Val, "sum":"rate.100")

# Spread by confidence intervals
x2 <- long_DF %>% spread(Estimates, Val)
#x2 <- x2[,c(1:11,16:18)]
long_DF2 <- x2[complete.cases(x2), ]

# Create a rounded version for tables
x2$"point estimate" <- as.numeric(x2$"point estimate")
x2$"lower CI" <- as.numeric(x2$"lower CI")
x2$"upper CI" <- as.numeric(x2$"upper CI")

x2$point.t <- round(x2$"point estimate",0)
x2$lower.t <- round(x2$"lower CI",0)
x2$upper.t <- round(x2$"upper CI",0)

# Column with combined estimates
x2$estimate.t <- paste0(x2$point.t,' (',x2$lower.t,'-',x2$upper.t,')',sep='')

write.csv(x2,'no2.results.tables.csv')


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Replicate for CBG tables

cbg <- read.csv("no2.cbg.results.csv")

#Keep these columns
# COUNTYFP.x - 12
# TRACTCE.x - 13
# GEOID.x - 16
# hia.val - 24
# pop.val - 44
# rate - 45
# filenames - 46

cbg <- cbg[,c(12,13,16,24,44,45,45,46)]
cbg2 <- cSplit(cbg, 8:ncol(cbg), sep=",", stripWhite=TRUE, type.convert=FALSE)
cbg2[cbg2, c(1:6,8:16)]

names(cbg2) <- c('County','Tract','GEOID','Cases','Population','Rate per 100,000','Rate per 100,000','Extent',
                 'Outcome','CRF','Estimates','Concentrations','Population dataset','Population fraction',
                 'Age groups','Baseline disease rates')


cbg2$'Baseline disease rates'[cbg2$'Baseline disease rates' == 'CBG baseline disease rates cbg.results.csv'] <- 'CBG baseline disease rates'
cbg2$'Baseline disease rates'[cbg2$'Baseline disease rates' == 'County baseline disease rates cbg.results.csv'] <- 'County baseline disease rates'
cbg2$'Baseline disease rates'[cbg2$'Baseline disease rates' == 'Zip-code rates cbg.results.csv'] <- 'Zip-code baseline disease rates'
cbg2$'Baseline disease rates'[cbg2$'Baseline disease rates' == 'State of California disease rate cbg.results.csv'] <- 'State of California disease rate'

cbg2$Extent[cbg2$Extent == './Houston area'] <- 'Houston area'

# Spread to Long DF

cbg <- cbg2[,c(1:4,8:10,12:16,11,4:7)]
cbg3 <- cbg[,c(1:3,5:16)]

long_DF <- cbg3 %>% gather(Analysis, Val, "Cases":"Rate per 100,000")
x4 <- long_DF %>% spread(Estimates, Val)
x4 <- x4[complete.cases(x4), ]
x4$point.t <- round(x4$"point estimate",0)
x4$lower.t <- round(x4$"lower CI",0)
x4$upper.t <- round(x4$"upper CI",0)

x4$estimate.t <- paste0(x4$point.t,' (',x4$lower.t,'-',x4$upper.t,')',sep='') 

write.csv(x4,'no2.cbg.results.tables.csv')



#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Replicate for County tables

co <- read.csv('no2.county.results.csv')

#Keep these columns
# NAMELSAD.x -16
# hia.val - 41
# pop.val - 78
# rate - 79
# filenames - 80


co <- co[,c(16,41,78,79,80)]
co2 <- cSplit(co, 5:ncol(co), sep=",", stripWhite=TRUE, type.convert=FALSE)

names(co2) <- c('County','Cases','Population','Rate per 100,000','Extent','Outcome',
                'CRF','Estimates','Concentrations','Population dataset','Population fraction',
                'Age groups','Baseline disease rates')

co2$Extent[co2$Extent == './Oakland'] <- 'Oakland'
co2$Extent[co2$Extent == './West and Downtown Oakland'] <- 'West and Downtown Oakland'
co2$Extent[co2$Extent == './Bay area'] <- 'Bay area'
co2$Extent[co2$Extent == './Alameda County'] <- 'Alameda County'

co2$'Baseline disease rates'[co2$'Baseline disease rates' == 'CBG baseline disease rates county.results.csv'] <- 'CBG baseline disease rates'
co2$'Baseline disease rates'[co2$'Baseline disease rates' == 'County baseline disease rates county.results.csv'] <- 'County baseline disease rates'
co2$'Baseline disease rates'[co2$'Baseline disease rates' == 'Zip-code rates county.results.csv'] <- 'Zip-code baseline disease rates'
co2$'Baseline disease rates'[co2$'Baseline disease rates' == 'State of California disease rate county.results.csv'] <- 'State of California disease rate'


co <- co2[,c(1,5:7,9:13,8,2:4)]

long_DF <- co %>% gather(Analysis, Val, "Cases":"Rate per 100,000")
x4 <- long_DF %>% spread(Estimates, Val)


x4$Extent[x4$Extent == 'Alameda County' & x4$County != 'Alameda County'] <- 'NA'
x4$Extent[x4$Extent == 'Oakland'] <- 'NA'
x4$Extent[x4$Extent == 'West and Downtown Oakland'] <- 'NA'
x4 <- x4[complete.cases(x4), ]


x4$point.t <- round(x4$"point estimate",0)
x4$lower.t <- round(x4$"lower CI",0)
x4$upper.t <- round(x4$"upper CI",0)

x4$estimate.t <- paste0(x4$point.t,' (',x4$lower.t,'-',x4$upper.t,')',sep='') 

write.csv(x4,'no2.co.results.tables.csv')

#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Replicate for City tables

df <- read.csv("no2.city.results.csv")

#Keep these columns
# NAMELSAD.y - 44
# hia.val - 31
# pop.val - 58
# rate - 59
# filenames - 60 


city <- df[,c(44,31,58,59,60)]
city2 <- cSplit(city, 5:ncol(city), sep=",", stripWhite=TRUE, type.convert=FALSE)

names(city2) <- c('City','Cases','Population','Rate per 100,000','Extent','Outcome',
                'CRF','Estimates','Concentrations','Population dataset','Population fraction',
                'Age groups','Baseline disease rates')

city2$Extent[city2$Extent == './Bay area'] <- 'Bay area'

city2$'Baseline disease rates'[city2$'Baseline disease rates' == 'CBG baseline disease rates city.results.csv'] <- 'CBG baseline disease rates'
city2$'Baseline disease rates'[city2$'Baseline disease rates' == 'County baseline disease rates city.results.csv'] <- 'County baseline disease rates'
city2$'Baseline disease rates'[city2$'Baseline disease rates' == 'Zip-code rates city.results.csv'] <- 'Zip-code baseline disease rates'
city2$'Baseline disease rates'[city2$'Baseline disease rates' == 'State of California disease rate city.results.csv'] <- 'State of California disease rate'

list.files()


city <- city2[,c(1,5:7,9:13,8,2:4)]

long_DF2 <- city %>% gather(Analysis, Val, "Cases":"Rate per 100,000")

x4 <- long_DF2 %>% spread(Estimates, Val)

x4 <- x4[complete.cases(x4), ]

x4$point.t <- round(x4$"point estimate",)
x4$lower.t <- round(x4$"lower CI",0)
x4$upper.t <- round(x4$"upper CI",0)

x4$estimate.t <- paste0(x4$point.t,' (',x4$lower.t,'-',x4$upper.t,')',sep='') 
write.csv(x4,'no2.city.results.tables.csv')

