#library("data.table", lib.loc="~/R/lib/") # FING FING
library("data.table") # CASA CASA
library("dplyr")
library("speedglm")
library("plm")
library("multiwayvcov")
library("lmtest")
library(boot)
library(sandwich)

#setwd("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Rutinas/")

# ## Add supermarket cashiers
# supers <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/Establecimientos.csv", header = T, sep = ',')
# colnames(supers)
# supers <- supers[,c(1,13)]
# db <- merge(db, supers, by = "Super") # Merge cola and supermarket bases
# rm(supers)


####################################################
######## (1) Regressions for whole database ########

# Load database
#dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)
dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)

head(dbF)


#### Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
#dbF$Time3 <- dbF$Time * dbF$Time2
#dbF$Time4 <- dbF$Time2 * dbF$Time2
dbF <- dbF[dbF$Time <90,]


#### Run base regressions
#reg1 <- lm(SD.Base ~ Time + Time2 + PAve + as.factor(Product), data = dbF)
reg1b <- plm(SD.Base ~ Time + Time2 + PAve, dbF, index = ("Product"))
summary(reg1b)
# Test for autocorrelation
reg1C <- coeftest(reg1b,vcov = vcovHC(reg1b,method = "arellano")) #, vcovHC(reg1, type = "HAC"))
reg1C

#### With competition and variety
reg4b <- plm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar,dbF, index=("Product"))
## Test for autocorrelation
reg4C <- coeftest(reg4b,vcov = vcovHC(reg4b,method = "arellano")) #, vcovHC(reg1, type = "HAC"))
reg4C


#### Clustered standard errors

### Base regression
reg1 <- lm(SD.Base ~ Time + Time2 + PAve + as.factor(Product), data = dbF)
vcov_R1 <- cluster.vcov(reg1, dbF$Product) #cbind(, SD.BaseM$Product))
reg1C <- coeftest(reg1, vcov_R1)
reg1C
summary(reg1)

### Filtered by competition and variety
reg2 <- lm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar + as.factor(Product), data = dbF)
vcov_R2 <- cluster.vcov(reg2, dbF$Product) #cbind(, SD.BaseM$Product))
reg2C <- coeftest(reg2, vcov_R2)
reg2C
summary(reg2)

#sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaAll.txt", append = T)
sink("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
print("##########################################
      Regresion for all database #####")
print("#### salida base")
print(summary(reg1), include.rownames=F)
print("#### salida base: cluster")
print(reg1C, include.rownames=F)
sink()

#sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaAll.txt", append = T)
sink("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
print("##########################################
      Regresion for all database #####")
print("#### Filtrada competencia y variedad")
print(summary(reg2), include.rownames=F)
print("#### salida base: cluster")
print(reg2C, include.rownames=F)
sink()



########################################################
#### (2) Regressions for supermarkets in Montevideo ####

# Load database
#dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedMontevideo.csv", data.table = F)
dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedMontevideo.csv", data.table = F)

head(dbF)


## Run base regression 

dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
#dbF$Time3 <- dbF$Time * dbF$Time2
#dbF$Time4 <- dbF$Time2 * dbF$Time2
dbF <- dbF[dbF$Time <90,]


#reg1 <- lm(SD.Base ~ Time + Time2 + PAve + as.factor(Product), data = dbF)
reg1b <- plm(SD.Base ~ Time + Time2 + PAve, dbF, index = ("Product"))
summary(reg1b)

## Test for autocorrelation

reg1C <- coeftest(reg1b,vcov = vcovHC(reg1b,method = "arellano")) #, vcovHC(reg1, type = "HAC"))
reg1C

## with competition and variety


reg4b <- plm(SD.RCompVar ~ Time + Time2 +
               PAve.RCompVar,dbF, index=("Product"))

## Test for autocorrelation

reg4C <- coeftest(reg4b,vcov = vcovHC(reg4b,method = "arellano")) #, vcovHC(reg1, type = "HAC"))
reg4C

#############################################################################################################

#### Otra prueba por cluster !!!

reg1 <- lm(SD.Base ~ Time + Time2 + PAve + as.factor(Product), data = dbF)
vcov_R1 <- cluster.vcov(reg1, dbF$Product) #cbind(, SD.BaseM$Product))
reg1C <- coeftest(reg1, vcov_R1)
reg1C
summary(reg1)

## with competition and variety

reg2 <- lm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar + as.factor(Product), data = dbF)
vcov_R2 <- cluster.vcov(reg2, dbF$Product) #cbind(, SD.BaseM$Product))
reg2C <- coeftest(reg2, vcov_R2)
reg2C
summary(reg2)

#sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaAll.txt", append = T)
sink("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
print("##########################################
      Regresion for Supermarkets in Montevideo #####")
print("#### salida base")
print(summary(reg1), include.rownames=F)
print("#### salida base: cluster")
print(reg1C, include.rownames=F)
sink()

#sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaAll.txt", append = T)
sink("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
print("##########################################
      Regresion for Supermarkets in Motnevideo #####")
print("#### Filtrada competencia y variedad")
print(summary(reg2), include.rownames=F)
print("#### salida base: cluster")
print(reg2C, include.rownames=F)
sink()


##########################################################
######## (3) Regressions for supermarkets in 2007 ########

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)

head(dbF)


#### Previous operations
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
#dbF$Time3 <- dbF$Time * dbF$Time2
#dbF$Time4 <- dbF$Time2 * dbF$Time2
dbF <- dbF[dbF$Time <90,]
dbF$SD.CompVar <- dbF$SD.CompVar * 100
 

#### Base regression 
#reg1 <- lm(SD.Base ~ Time + Time2 + PAve + as.factor(Product), data = dbF)
reg1b <- plm(SD.Base ~ Time + Time2 + PAve, dbF, index = ("Product"))
summary(reg1b)
# Test for autocorrelation
reg1C <- coeftest(reg1b,vcov = vcovHC(reg1b,method = "arellano")) #, vcovHC(reg1, type = "HAC"))
reg1C

#### With competition and variety
reg4b <- plm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar,dbF, index=("Product"))
# Test for autocorrelation
reg4C <- coeftest(reg4b,vcov = vcovHC(reg4b,method = "arellano")) #, vcovHC(reg1, type = "HAC"))
reg4C


#### Clustered standard errors

### Base regression 
reg1 <- lm(SD.Base ~ Time + Time2 + PAve + as.factor(Product), data = dbF)
vcov_R1 <- cluster.vcov(reg1, dbF$Product) #cbind(, SD.BaseM$Product))
reg1C <- coeftest(reg1, vcov_R1)
reg1C
summary(reg1)

### Filtered by competition and variety
reg2 <- lm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar + as.factor(Product), data = dbF)
vcov_R2 <- cluster.vcov(reg2, dbF$Product) #cbind(, SD.BaseM$Product))
reg2C <- coeftest(reg2, vcov_R2)
reg2C
summary(reg2)


### Competition and variety on the LHS
reg5 <- lm(SD.CompVar ~ Time + Time2 + Ave.CompVar + as.factor(Product), data = dbF)
vcov_R5 <- cluster.vcov(reg5, dbF$Product) #cbind(, SD.BaseM$Product))
reg5C <- coeftest(reg5, vcov_R5)
reg5C
summary(reg5)



#sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaAll.txt", append = T)
sink("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
print("##########################################
      Regresion for Supermarkets in 2007 #####")
print("#### salida base")
print(summary(reg1), include.rownames=F)
print("#### salida base: cluster")
print(reg1C, include.rownames=F)
sink()

#sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaAll.txt", append = T)
sink("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
print("##########################################
      Regresion for Supermarkets in 2007 #####")
print("#### Filtrada competencia y variedad")
print(summary(reg2), include.rownames=F)
print("#### salida base: cluster")
print(reg2C, include.rownames=F)
sink()


########################################################################
######## (4) Regressions for supermarkets in 2007 in Montevideo ########

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)

head(dbF)


##### Previous operations
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
#dbF$Time3 <- dbF$Time * dbF$Time2
#dbF$Time4 <- dbF$Time2 * dbF$Time2
dbF <- dbF[dbF$Time <90,]


#### Base regression 
#reg1 <- lm(SD.Base ~ Time + Time2 + PAve + as.factor(Product), data = dbF)
reg1b <- plm(SD.Base ~ Time + Time2 + PAve, dbF, index = ("Product"))
summary(reg1b)
# Test for autocorrelation
reg1C <- coeftest(reg1b,vcov = vcovHC(reg1b,method = "arellano")) #, vcovHC(reg1, type = "HAC"))
reg1C


#### Prices filtered by competition and variety
reg4b <- plm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar,dbF, index=("Product"))
# Test for autocorrelation
reg4C <- coeftest(reg4b,vcov = vcovHC(reg4b,method = "arellano")) #, vcovHC(reg1, type = "HAC"))
reg4C


#### Clustered standard errors

### Base regression 
reg1 <- lm(SD.Base ~ Time + Time2 + PAve + as.factor(Product), data = dbF)
vcov_R1 <- cluster.vcov(reg1, dbF$Product) #cbind(, SD.BaseM$Product))
reg1C <- coeftest(reg1, vcov_R1)
reg1C
summary(reg1)

### Filtered by competition and variety
reg2 <- lm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar + as.factor(Product), data = dbF)
vcov_R2 <- cluster.vcov(reg2, dbF$Product) #cbind(, SD.BaseM$Product))
reg2C <- coeftest(reg2, vcov_R2)
reg2C
summary(reg2)


### Competition and variety on the LHS
reg5 <- lm(SD.CompVar ~ Time + Time2 + Ave.CompVar + as.factor(Product), data = dbF)
vcov_R5 <- cluster.vcov(reg5, dbF$Product) #cbind(, SD.BaseM$Product))
reg5C <- coeftest(reg5, vcov_R5)
reg5C
summary(reg5)



#sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaAll.txt", append = T)
sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
#sink("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
print("#####################################################
      Regresion for Supermarkets in 2007 in Montevideo #####")
print("#### salida base")
print(summary(reg1), include.rownames=F)
print("#### salida base: cluster")
print(reg1C, include.rownames=F)
sink()

#sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaAll.txt", append = T)
sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
#sink("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaCluster.txt", append = T)
print("######################################################
       Regresion for Supermarkets in 2007 in Montevideo #####")
print("#### Filtrada competencia y variedad")
print(summary(reg2), include.rownames=F)
print("#### salida base: cluster")
print(reg2C, include.rownames=F)
sink()
