### Areg ####

library("multiwayvcov") ## Fing
library("lmtest") ## Fing
library("data.table") ## Fing
library("dplyr") ## Fing
library("lmtest")
library("lfe")

#setwd("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Rutinas/")


dbF <- get(load("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))
dbF <- get(load("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))


### Create as factors ####

dbF$Super <- as.factor(dbF$Super)
dbF$Category <-as.factor(dbF$Category)
dbF <- dbF[dbF$Time < 90,]
dbF$Time <-as.factor(dbF$Time)
dbF$competition2 <- dbF$competition * dbF$competition
dbF$variety2 <- dbF$variety * dbF$variety


### Price Regressions ####


# con efectos fijos por super, categoría, super tiempo y categoría tiempo 
areg1 <-   summary(felm(moda ~ variety + competition |Super * Time + Category * Time| 0, dbF, 
                        exactDOF = exactDOF = 2046404)) ## ok
areg1

areg1b <- summary(felm(moda ~ variety + competition + variety2 + competition2
                       |Super * Time + Category * Time| 0, dbF, exactDOF = 2046404)) ## ok
areg1b

# con efectos fijos super, cateogría, super*cateogría, super tiempo y categoría tiempo
areg2 <-summary(felm(moda ~ variety + competition |(Super + Category) * Time| 0 | Super , 
                     dbF, exactDOF = 2046404)) ## ok
areg2

areg2b <-summary(felm(moda ~ variety + variety2 + competition + competition2 
                     |(Super + Category) * Time| 0 | Super , dbF, exactDOF = 2046404)) ## ok
areg2b


areg3 <- summary(felm(moda ~ variety + competition |(Super + Category) * Time| 0 | Super + Category, dbF))
areg3

areg3b <- summary(felm(moda ~ variety + variety2 + competition + competition2 
                      |(Super + Category) * Time| 0 | Super + Category, dbF, exactDOF = 2046203))
areg3b
