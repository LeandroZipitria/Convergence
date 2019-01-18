unlink(".RData") 

<<<<<<< HEAD
library("data.table", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib'))
library("dplyr", lib.loc = .libPaths('/clusteruy/home/leandroz/R/lib'))
=======
library("data.table", lib.loc = .libPaths('~/R/lib'))
library("dplyr", lib.loc = .libPaths('~/R/lib'))
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d


##### Create database for whole data ####

## Load database
<<<<<<< HEAD
dbM <- fread("/clusteruy/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
=======
dbM <- fread("/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d

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
dbM$RSuperCCCV <-residuals(lm(moda ~ 0 + as.factor(chain.number) + as.factor(city.number)
                              + as.factor(competition) + as.factor(variety), data = dbM))


## Save database
<<<<<<< HEAD
save(dbM, file = "/clusteruy/home/leandroz/Bases/Convergence/2018BaseResiduals.Rdata")
=======
save(dbM, file = "/home/leandroz/Bases/Convergence/2018BaseResiduals.Rdata")
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d
rm (list = ls( ))


##### Create databases for Montevideo ####

## Load database
<<<<<<< HEAD
dbM <- fread("/clusteruy/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
=======
dbM <- fread("/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d
dbM <- dbM[dbM$depto.number == 10,] # Number 10 is Montevideo

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
dbM$RSuperCCCV <-residuals(lm(moda ~ 0 + as.factor(chain.number) + as.factor(city.number)
                              + as.factor(competition) + as.factor(variety), data = dbM))


## Save database
<<<<<<< HEAD
save(dbM, file = "/clusteruy/home/leandroz/Bases/Convergence/2018BaseResidualsMontevideo.Rdata")
=======
save(dbM, file = "/home/leandroz/Bases/Convergence/2018BaseResidualsMontevideo.Rdata")
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d
rm (list = ls( )) 


##### Create databases for original supermarkets ####

## Load database
<<<<<<< HEAD
dbM <- fread("/clusteruy/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
=======
dbM <- fread("/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d
OS <- unique(dbM[dbM$Year == 2007,]$Super)

dbM <- dbM[dbM$Super %in% OS,]

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
dbM$RSuperCCCV <-residuals(lm(moda ~ 0 + as.factor(chain.number) + as.factor(city.number)
                              + as.factor(competition) + as.factor(variety), data = dbM))


## Save database
<<<<<<< HEAD
save(dbM, file = "/clusteruy/home/leandroz/Bases/Convergence/2018BaseResidualsOrigSuper.Rdata")
=======
save(dbM, file = "/home/leandroz/Bases/Convergence/2018BaseResidualsOrigSuper.Rdata")
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d
rm (list = ls( )) 



##### Create databases for original supermarkets in Montevideo ####

## Load database
<<<<<<< HEAD
dbM <- fread("/clusteruy/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
=======
dbM <- fread("/home/leandroz/Bases/Convergence/2018_dbase_with_super.csv", data.table = F)
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d
OS <- unique(dbM[dbM$Year == 2007,]$Super)

dbM <- dbM[dbM$Super %in% OS,]
dbM <- dbM[dbM$depto.number == 10,] # Number 10 is Montevideo

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
dbM$RSuperCCCV <-residuals(lm(moda ~ 0 + as.factor(chain.number) + as.factor(city.number)
                              + as.factor(competition) + as.factor(variety), data = dbM))

## Save database
<<<<<<< HEAD
save(dbM, file = "/clusteruy/home/leandroz/Bases/Convergence/2018BaseResidualsOrigSuperMdeo.Rdata")
=======
save(dbM, file = "/home/leandroz/Bases/Convergence/2018BaseResidualsOrigSuperMdeo.Rdata")
>>>>>>> ce4a202398905b893133c084235fdf5c483ff24d
rm (list = ls( ))
