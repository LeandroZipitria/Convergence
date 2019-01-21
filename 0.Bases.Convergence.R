############################################################################
### Description: This script create the filtered series to be used later ###
############################################################################

### This file is run on cluster

unlink(".RData") 

library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib'))
library("dplyr", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib'))


###### Define functions for all cities ######

functAll <- function(){
  ## 1) Filter: Supermarket
  dbM$RSuper <-residuals(lm(moda ~ 0 + as.factor(Super), data = dbM))
  
  ## 2) Filter: Supermarket + competition + same producer
  dbM$RSuperCV <-residuals(lm(moda ~ 0 + as.factor(Super) + as.factor(competition) + as.factor(variety), data = dbM))
  
  ## 3) Filter: competition + same producer
  dbM$RCompVar <-residuals(lm(moda ~ as.factor(competition) + as.factor(variety), data = dbM))
  
  ## 4) Filter: Supermarket + chain + city
  dbM$RSuperCC <-residuals(lm(moda ~ 0 + as.factor(Super) +
                                as.factor(chain.number) + as.factor(city.number), data = dbM))
  
  ## 5) Filter: Supermarket + chain + city + competition + same producer
  dbM$RSuperTodo <-residuals(lm(moda ~ 0 + as.factor(Super) + as.factor(chain.number) 
                                + as.factor(city.number) + as.factor(competition) + as.factor(variety), data = dbM))
  
  ## 6) Filter: Chain + city + competition + same producer
  dbM$R_CCCV <-residuals(lm(moda ~ 0 + as.factor(chain.number) + as.factor(city.number)
                                + as.factor(competition) + as.factor(variety), data = dbM))
}



##### Create database for all data #####

## Load database
dbM <- fread("/clusteruy/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)

dbM <- functAll()

## Save database
saveRDS(dbM, file = "/clusteruy/home/leandroz/Bases/Convergence/2018BaseResiduals.rds")
rm (list = ls( ))


##### Create databases for original supermarkets #####

## Load database
dbM <- fread("/clusteruy/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
OS <- unique(dbM[dbM$Year == 2007,]$Super)
dbM <- dbM[dbM$Super %in% OS,]

dbM <- functAll()

## Save database
saveRDS(dbM, file = "/clusteruy/home/leandroz/Bases/Convergence/2018BaseResidualsOrigSuper.rds")
rm (list = ls( )) 



###### Define functions for Montevideo ######

functMdeo <- function(){
  ## 1) Filter: Supermarket
  dbM$RSuper <-residuals(lm(moda ~ 0 + as.factor(Super), data = dbM))
  
  ## 2) Filter: Supermarket + competition + same producer
  dbM$RSuperCV <-residuals(lm(moda ~ 0 + as.factor(Super) + as.factor(competition) + as.factor(variety), data = dbM))
  
  ## 3) Filter: competition + same producer
  dbM$RCompVar <-residuals(lm(moda ~ as.factor(competition) + as.factor(variety), data = dbM))
  
  ## 4) Filter: Supermarket + chain
  dbM$RSuperCC <-residuals(lm(moda ~ 0 + as.factor(Super) +
                                as.factor(chain.number), data = dbM))
  
  ## 5) Filter: Supermarket + chain + competition + same producer
  dbM$RSuperTodo <-residuals(lm(moda ~ 0 + as.factor(Super) + as.factor(chain.number) 
                                + as.factor(competition) + as.factor(variety), data = dbM))
  
  ## 6) Filter: Chain + competition + same producer
  dbM$R_CCCV <-residuals(lm(moda ~ 0 + as.factor(chain.number) 
                                + as.factor(competition) + as.factor(variety), data = dbM))
}


##### Create database for Montevideo #####

## Load database
dbM <- fread("/clusteruy/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
dbM <- dbM[dbM$depto.number == 10,] # Number 10 is Montevideo

dbM <- functMdeo()

## Save database
saveRDS(dbM, file = "/clusteruy/home/leandroz/Bases/Convergence/2018BaseResidualsMontevideo.rds")
rm (list = ls( )) 


##### Create databases for original supermarkets in Montevideo #####

## Load database
dbM <- fread("/clusteruy/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
OS <- unique(dbM[dbM$Year == 2007,]$Super)
dbM <- dbM[dbM$Super %in% OS,]
dbM <- dbM[dbM$depto.number == 10,] # Number 10 is Montevideo

dbM <- functMdeo()

## Save database
saveRDS(dbM, file = "/clusteruy/home/leandroz/Bases/Convergence/2018BaseResidualsOrigSuperMdeo.rds")
rm (list = ls( ))

################## End of script ##################