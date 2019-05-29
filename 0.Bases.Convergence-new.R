############################################################################
### Description: This script create the filtered series to be used later ###
############################################################################

### This file is run on cluster

unlink(".RData") 

library("data.table")
library("dplyr")
library(lfe)


##### Create database for all data #####

## Load database
dbM <- fread("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/2018_dbase_with_super.csv", data.table = F)
head(dbM)

## 1) Filter: competition + same producer
dbM$RCompBrand <- as.numeric(felm(moda ~ 1 |as.factor(competition) + as.factor(variety) | 0 | 0, dbM)$residuals)
## 2) Filter: Supermarket + chain + city
dbM$RStoreCC <- as.numeric(felm(moda ~ 1 | as.factor(Super) +
                                 as.factor(chain.number) + as.factor(city.number) | 0 | 0, dbM)$residuals)
## 3) Filter: Supermarket * Time + Category * Time
dbM$RSTCT <- as.numeric(felm(moda ~ 1 |as.factor(Super)*as.factor(Time) + as.factor(Time)*as.factor(Category) | 0 | 
                              0, dbM)$residuals)

## Save database
head(dbM)
saveRDS(dbM, file = "c://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals-new.rds")
rm (list = ls( ))
gc()



##### Create databases for original supermarkets #####

## Load database
dbM <- fread("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/2018_dbase_with_super.csv", data.table = F)
OS <- unique(dbM[dbM$Year == 2007,]$Super)
dbM <- dbM[dbM$Super %in% OS,]

## 1) Filter: competition + same producer
dbM$RCompBrand <- as.numeric(felm(moda ~ 1 |as.factor(competition) + as.factor(variety) | 0 | 0, dbM)$residuals)
## 2) Filter: Supermarket + chain + city
dbM$RStoreCC <- as.numeric(felm(moda ~ 1 | as.factor(Super) +
                                  as.factor(chain.number) + as.factor(city.number) | 0 | 0, dbM)$residuals)
## 3) Filter: Supermarket * Time + Category * Time
dbM$RSTCT <- as.numeric(felm(moda ~ 1 |as.factor(Super)*as.factor(Time) + as.factor(Time)*as.factor(Category) | 0 | 
                               0, dbM)$residuals)

## Save database
head(dbM)
saveRDS(dbM, file = "c://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuper-new.rds")
rm (list = ls( )) 
gc()



##### Create database for Montevideo #####

## Load database
dbM <- fread("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/2018_dbase_with_super.csv", data.table = F)
dbM <- dbM[dbM$depto.number == 10,] # Number 10 is Montevideo

## 1) Filter: competition + same producer
dbM$RCompBrand <- as.numeric(felm(moda ~ 1 |as.factor(competition) + as.factor(variety) | 0 | 0, dbM)$residuals)
## 2) Filter: Supermarket + chain + city
dbM$RStoreCC <- as.numeric(felm(moda ~ 1 | as.factor(Super) +
                                  as.factor(chain.number) | 0 | 0, dbM)$residuals)
## 3) Filter: Supermarket * Time + Category * Time
dbM$RSTCT <- as.numeric(felm(moda ~ 1 |as.factor(Super)*as.factor(Time) + as.factor(Time)*as.factor(Category) | 0 | 
                               0, dbM)$residuals)

## Save database
head(dbM)
saveRDS(dbM, file = "c://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsMontevideo-new.rds")
rm (list = ls( )) 
gc()



##### Create databases for original supermarkets in Montevideo #####

## Load database
dbM <- fread("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/2018_dbase_with_super.csv", data.table = F)
OS <- unique(dbM[dbM$Year == 2007,]$Super)
dbM <- dbM[dbM$Super %in% OS,]
dbM <- dbM[dbM$depto.number == 10,] # Number 10 is Montevideo

## 1) Filter: competition + same producer
dbM$RCompBrand <- as.numeric(felm(moda ~ 1 |as.factor(competition) + as.factor(variety) | 0 | 0, dbM)$residuals)
## 2) Filter: Supermarket + chain + city
dbM$RStoreCC <- as.numeric(felm(moda ~ 1 | as.factor(Super) +
                                  as.factor(chain.number) | 0 | 0, dbM)$residuals)
## 3) Filter: Supermarket * Time + Category * Time
dbM$RSTCT <- as.numeric(felm(moda ~ 1 |as.factor(Super)*as.factor(Time) + as.factor(Time)*as.factor(Category) | 0 | 
                               0, dbM)$residuals)

## Save database
head(dbM)
saveRDS(dbM, file = "c://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuperMdeo-new.rds")
rm (list = ls( ))
gc()

################## End of script ##################