#/////////////////////////////////////////////////////////////////////////////////////////////////

# This is a script to: 1) text to columns of 02.df.R files and
# 2) create a long DF (estimates spread across columns per result)
# for both full results and CBG results for BC

# Date: 2019-11-20
# Author: V Southerland

#/////////////////////////////////////////////////////////////////////////////////////////////////

library(splitstackshape)
library(raster)
library(rgdal)
library(dplyr)
library(tidyverse)
library(readxl)


setwd("/GWSPH/home/vtinney/ho/results3/no2/df/final/")


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
#p.sum_01 #19
#p.mean_01 #28
#X0._01 #37
#X25._01 #46
#X50._01 #55
#X75._01 #64
#X100._01 #73
#[82] "dataset.names_1"    "dataset.names_2"    "dataset.names_3"
#[85] "dataset.names_4"    "dataset.names_5"    "dataset.names_6"
#[88] "dataset.names_7"    "dataset.names_8"    "dataset.names_9_01"
#[91] "dataset.names_9_02" "dataset.names_9_03" "dataset.names_9_04"
#[94] "dataset.names_9_05" "dataset.names_9_06" "dataset.names_9_07"
#[97] "dataset.names_9_08" "dataset.names_9_09"


full2 <- full[,c(19,28,37,46,55,64,73,90:98)]

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


comb$pop.total[comb$'Age groups' == "ages 0-17 years" & comb$Extent == 'Alameda County'] <- 377886.9
comb$pop.total[comb$'Age groups' == "ages 25-99 years" & comb$Extent == 'Alameda County'] <- 1090503
comb$pop.total[comb$'Age groups' == "ages 65-99 years" & comb$Extent == 'Alameda County'] <- 183904.1
comb$pop.total[comb$'Age groups' == "all ages" & comb$Extent == 'Alameda County'] <- 1663085

comb$pop.total[comb$'Age groups' == "ages 0-17 years" & comb$Extent == 'Bay area'] <- 1721994
comb$pop.total[comb$'Age groups' == "ages 25-99 years" & comb$Extent == 'Bay area'] <- 5166662
comb$pop.total[comb$'Age groups' == "ages 65-99 years" & comb$Extent == 'Bay area'] <- 947944.7
comb$pop.total[comb$'Age groups' == "all ages" & comb$Extent == 'Bay area'] <- 7755519

comb$pop.total[comb$'Age groups' == "ages 0-17 years" & comb$Extent == 'Oakland'] <- 28152.86
comb$pop.total[comb$'Age groups' == "ages 25-99 years" & comb$Extent == 'Oakland'] <- 78936.75
comb$pop.total[comb$'Age groups' == "ages 65-99 years" & comb$Extent == 'Oakland'] <- 13140.52
comb$pop.total[comb$'Age groups' == "all ages" & comb$Extent == 'Oakland'] <- 122246

comb$pop.total[comb$'Age groups' == "ages 0-17 years" & comb$Extent == 'West and Downtown Oakland'] <- 9308.006
comb$pop.total[comb$'Age groups' == "ages 25-99 years" & comb$Extent == 'West and Downtown Oakland'] <- 42323.16
comb$pop.total[comb$'Age groups' == "ages 65-99 years" & comb$Extent == 'West and Downtown Oakland'] <- 7981.142
comb$pop.total[comb$'Age groups' == "all ages" & comb$Extent == 'West and Downtown Oakland'] <- 58906

#/////////////////////////////////////////////////////////////////////////////////////////////////

# Create a rate per 100,000
comb$sum <- as.numeric(comb$sum)
comb$rate.100 <- (comb$sum*100000)/comb$pop.total

comb <- comb[,c(8:11,13:18,12,1:7,19)]

# Gather results by type
long_DF <- comb %>% gather(Anal, Val, "sum":"rate.100")

# Spread by confidence intervals
x2 <- long_DF %>% spread(Estimates, Val)
x2 <- x2[,c(1:11,16:18)]
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
# GEOID.x - 15
# GEOID_Data.x - 25
# hia.val - 27
# pop.val - 50
# rate - 51
# filenames - 52

cbg <- cbg[,c(12,13,15,25,27,50,51,52)]
cbg2 <- cSplit(cbg, 8:ncol(cbg), sep=",", stripWhite=TRUE, type.convert=FALSE)

names(cbg2) <- c('County','Tract','GEOID','GEOID_Full','Cases','Population','Rate per 100,000','Extent',
                 'Outcome','CRF','Estimates','Concentrations','Population dataset','Population fraction',
                 'Age groups','Baseline disease rates')


cbg2$'Baseline disease rates'[cbg2$'Baseline disease rates' == 'CBG baseline disease rates cbg.results.csv'] <- 'CBG baseline disease rates'
cbg2$'Baseline disease rates'[cbg2$'Baseline disease rates' == 'County baseline disease rates cbg.results.csv'] <- 'County baseline disease rates'
cbg2$'Baseline disease rates'[cbg2$'Baseline disease rates' == 'Zip-code rates cbg.results.csv'] <- 'Zip-code baseline disease rates'
cbg2$'Baseline disease rates'[cbg2$'Baseline disease rates' == 'State of California disease rate cbg.results.csv'] <- 'State of California disease rate'

cbg2$Extent[cbg2$Extent == './Oakland'] <- 'Oakland'
cbg2$Extent[cbg2$Extent == './West and Downtown Oakland'] <- 'West and Downtown Oakland'
cbg2$Extent[cbg2$Extent == './Bay area'] <- 'Bay area'
cbg2$Extent[cbg2$Extent == './Alameda County'] <- 'Alameda County'

# Spread to Long DF

cbg <- cbg2[,c(1:4,8:10,12:16,11,5:7)]

long_DF <- cbg %>% gather(Analysis, Val, "Cases":"Rate per 100,000")
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

