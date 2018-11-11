### Areg ####

library("multiwayvcov") ## Fing
library("lmtest") ## Fing
library("data.table") ## Fing
library("dplyr") ## Fing
library("lmtest")
library("lfe")

#setwd("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Rutinas/")


dbF <- get(load("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))
#dbF <- get(load("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))


### Create as factors ####

dbF$Super <- as.factor(dbF$Super)
dbF$Category <-as.factor(dbF$Category)
dbF <- dbF[dbF$Time < 90,]
dbF$Time <-as.factor(dbF$Time)
#dbF$competition2 <- dbF$competition * dbF$competition
#dbF$variety2 <- dbF$variety * dbF$variety


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


areg3 <- summary(felm(moda ~ variety + competition |(Super + Category) * Time| 0 | Super + Time,
                      dbF, exactDOF = T))
areg3

#areg3b <- summary(felm(moda ~ variety + variety2 + competition + competition2 
#                      |(Super + Category) * Time| 0 | Super + Category, dbF, exactDOF = 2046203))
#areg3b


####################################
######## Create instruments ########

colnames(dbF)
# Add CCZ information
supers <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/Establecimientos.csv", header = T, sep = ',')
colnames(supers)
supers <- supers[,c(1,8)]
dbF <- merge(dbF, supers, by = "Super")

z <- c(1,4)
dbF1 <- dbF[dbF$Super %in% z | dbF$Time %in% z,]
dbF <- dbFPosta
rm(dbF1, dbFP, dbFPosta) 

dbF$Inst.Var <- dbF$Inst.Comp <- 0


for (i in unique(dbF$Super)) {
  for (t in unique(dbF$Time)) {
    for (p in unique(dbF$Product)) {
      Sub <- subset(dbF, dbF$Product == p & dbF$city.number == x &
                    dbF$Time == t)
      if (nrow(Sub[Sub$Super == i,]) < 1) {next}
      if (Sub[Sub$Super == i,]$city.number != 30) {
        x <- unique(get("dbF")[dbF$Super == i,]$city.number)
        dbF[which(dbF$Super == i & dbF$Time == t & dbF$Product == p),]$Inst.Comp <- 
          mean(Sub[Sub$Super != i,]$competition)
        dbF[which(dbF$Super == i & dbF$Time == t & dbF$Product == p),]$Inst.Var <-
          mean(Sub[Sub$Super != i,]$variety)
      }
      else {
        x <- unique(get("dbF")[dbF$Super == i,]$ccz)
        dbF[which(dbF$Super == i & dbF$Time == t & dbF$Product == p),]$Inst.Comp <- 
          mean(Sub[Sub$Super != i,]$competition)
        dbF[which(dbF$Super == i & dbF$Time == t & dbF$Product == p),]$Inst.Var <-
          mean(Sub[Sub$Super != i,]$variety)
      }
    }
  }
}
    

gc()

fivenum(dbF$Inst.Var)

areg4 <- summary(felm(moda ~ variety + competition | Super + Category + Time | 
                         (variety|competition ~ Inst.Comp + Inst.Var) | Super + Time, dbF, exactDOF=TRUE))
areg4

ef <- efactory(areg4b)
