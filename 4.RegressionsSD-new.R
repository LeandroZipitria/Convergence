##########################################################
######## Description: regressions for SD database ########


## Load libraries used
library(stargazer)
library(lfe)
library(data.table)

#setwd("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Rutinas/")

#### --------------BASE REGRESIONS ----------------------------------

## Restricted supers and goods at the begining ----------------------

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)
dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)
head(dbF)
# Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
dbF <- dbF[dbF$Time <90,]
prodorig <- unique(dbF[dbF$Year == 2007,]$Product)
# Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"

## Clustered standard errors regressions
# Base regression
reg1 = felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg1)
# Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg2)
# Competition and same producer dispersion regression
reg3 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg3)

## Print regression outputs
sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaSD_2019_V2.txt", append = T)
print("##########################################
      Regresion for supermarkets and products in 2007 #####")
print("#### Main Regression ####")
print(summary(reg1), include.rownames=F)
print(reg1$N, include.rownames=F)
print("#### Filtered by competition and same producer ####")
print(summary(reg2), include.rownames=F)
print(reg2$N, include.rownames=F)
print("#### SD of competition and variety !!! ####")
print(summary(reg3), include.rownames=F)
print(reg3$N, include.rownames=F)
sink()


## Complete database ----------------------

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)
head(dbF)
# Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
dbF <- dbF[dbF$Time <90,]
# Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"

## Clustered standard errors regressions
# Base regression
reg1 = felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF)
summary(reg1)
# Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF)
summary(reg2)
# Competition and same producer dispersion regression
reg3 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF)
summary(reg3)

## Print regression outputs
sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaSD_2019_V2.txt", append = T)
print("##########################################
      Regresion for all supermarkets and products #####")
print("#### Main Regression ####")
print(summary(reg1), include.rownames=F)
print(reg1$N, include.rownames=F)
print("#### Filtered by competition and same producer ####")
print(summary(reg2), include.rownames=F)
print(reg2$N, include.rownames=F)
print("#### SD of competition and variety !!! ####")
print(summary(reg3), include.rownames=F)
print(reg3$N, include.rownames=F)
sink()


## Restricted supers and goods at the begining for Montevideo ----------------------

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)
head(dbF)
# Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
dbF <- dbF[dbF$Time <90,]
prodorig <- unique(dbF[dbF$Year == 2007,]$Product)
# Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"

## Clustered standard errors regressions
# Base regression
reg1 = felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg1)
# Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg2)
# Competition and same producer dispersion regression
reg3 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg3)

## Print regression outputs
sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaSD_2019_V2.txt", append = T)
print("##########################################
      Regresion for supermarkets and products in 2007 in MONTEVIDEO #####")
print("#### Main Regression ####")
print(summary(reg1), include.rownames=F)
print(reg1$N, include.rownames=F)
print("#### Filtered by competition and same producer ####")
print(summary(reg2), include.rownames=F)
print(reg2$N, include.rownames=F)
print("#### SD of competition and variety !!! ####")
print(summary(reg3), include.rownames=F)
print(reg3$N, include.rownames=F)
sink()


## All goods and supermarkets for Montevideo ----------------------

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedMontevideo.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)
head(dbF)
# Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
dbF <- dbF[dbF$Time <90,]
# Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"

## Clustered standard errors regressions
# Base regression
reg1 = felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF)
summary(reg1)
# Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF)
summary(reg2)
# Competition and same producer dispersion regression
reg3 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF)
summary(reg3)

## Print regression outputs
sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaSD_2019_V2.txt", append = T)
print("##########################################
      Regresion for ALL supermarkets and products in MONTEVIDEO #####")
print("#### Main Regression ####")
print(summary(reg1), include.rownames=F)
print(reg1$N, include.rownames=F)
print("#### Filtered by competition and same producer ####")
print(summary(reg2), include.rownames=F)
print(reg2$N, include.rownames=F)
print("#### SD of competition and variety !!! ####")
print(summary(reg3), include.rownames=F)
print(reg3$N, include.rownames=F)
sink()


#### --------------- Robustness ----------------



## End of script ------------