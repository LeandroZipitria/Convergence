##########################################################
######## Description: regressions for SD database ########


## Load libraries used
library(data.table)
library(plm)
library(multiwayvcov)
library(lmtest)
#library(boot)
library(sandwich)
library(stargazer)
library(lfe)

#setwd("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Rutinas/")



#########################################################################
######## (1) Regressions for whole database original supermarket ########

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)

head(dbF)


#### Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)

dbF <- dbF[dbF$Time <90,]


## Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"


#### Clustered standard errors regressions

### Base regression
reg1 <- felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF)
summary(reg1)
stargazer(reg1)

### Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF)
summary(reg2)
stargazer(reg2)

### Filtered by super
reg3 <- felm(SD.RSuper ~ Time + Time2 + PAve.RSuper | Product | 0 | Product, dbF)
summary(reg3)
stargazer(reg3)

### Filtered by super, chain, city
reg4 <- felm(SD.RSuperCC ~ Time + Time2 + PAve.RSuperCC | Product | 0 | Product, dbF)
summary(reg4)
stargazer(reg4)

### Filtered by competition, same producer, chain, and city
reg5 <- felm(SD.R_CCCV ~ Time + Time2 + PAve.R_CCCV | Product | 0 | Product, dbF)
summary(reg5)
stargazer(reg5)

### Filtered by super, competition, and same producer
reg6 <- felm(SD.RSuperCV ~ Time + Time2 + PAve.RSuperCV | Product | 0 | Product, dbF)
summary(reg6)
stargazer(reg6)

### Filtered by  Supermarket + chain + city + competition + same producer
reg7 <- felm(SD.RSuperTodo ~ Time + Time2 + PAve.RSuperTodo | Product | 0 | Product, dbF)
summary(reg7)
stargazer(reg7)

### Competition and same producer dispersion regression
reg8 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF)
summary(reg8)
stargazer(reg8)



### Print regression outputs
sink("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Salidas/salidaSD_2019.txt", append = T)
print("##########################################
      Regresion for original database #####")
print("#### Main Regression ####")
print(summary(reg1), include.rownames=F)

print("#### Filtered by competition and same producer ####")
print(summary(reg2), include.rownames=F)

print("#### Filtered by supermarket ####")
print(summary(reg3), include.rownames=F)

print("#### Filtered by supermarket, chain, and city ####")
print(summary(reg4), include.rownames=F)

print("#### Filtered by chain, city, competition, and same producer ####")
print(summary(reg5), include.rownames=F)

print("#### Filtered by supermarket, competition, and same producer ####")
print(summary(reg6), include.rownames=F)

print("#### Filtered by supermarket, chain, city, competition, and same producer ####")
print(summary(reg7), include.rownames=F)
sink()

print("#### Competition and same producer dispersion regression ####")
print(summary(reg8), include.rownames=F)
sink()




######## ------------------------------
### New regressions -------------------

# ----------------------
## Restricted supers and goods at the begining

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)

head(dbF)


#### Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
dbF <- dbF[dbF$Time <90,]
prodorig <- unique(dbF[dbF$Year == 2007,]$Product)


## Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"


#### Clustered standard errors regressions

### Base regression
reg1 = felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg1)

### Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg2)

### Competition and same producer dispersion regression
reg3 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg3)


### Print regression outputs
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


# ----------------------
## Complete database

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)

head(dbF)


#### Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)

dbF <- dbF[dbF$Time <90,]


## Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"


#### Clustered standard errors regressions

### Base regression
reg1 = felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF)
summary(reg1)

### Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF)
summary(reg2)

### Competition and same producer dispersion regression
reg3 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF)
summary(reg3)


### Print regression outputs
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


# ----------------------
## Restricted supers and goods at the begining for Montevideo

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)

head(dbF)


#### Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
dbF <- dbF[dbF$Time <90,]
prodorig <- unique(dbF[dbF$Year == 2007,]$Product)


## Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"


#### Clustered standard errors regressions

### Base regression
reg1 = felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg1)

### Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg2)

### Competition and same producer dispersion regression
reg3 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF[dbF$Product %in% prodorig,])
summary(reg3)


### Print regression outputs
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



# ----------------------
## All goods and supermarkets for Montevideo

# Load database
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedMontevideo.csv", data.table = F)
#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)

head(dbF)


#### Previous operations  
dbF$Product <- as.factor(dbF$Product)
dbF$Super <- dbF$moda <- dbF$chain.number <- dbF$city.number <- dbF$X_UTM <- dbF$Y_UTM <-  NULL
dbF <- na.omit(dbF)
dbF$SD.Base <- dbF$SD.Base * 100
dbF$SD.RCompVar <- dbF$SD.RCompVar *100 
dbF$P44 <- ifelse(dbF$Time > 43, 1, 0)
dbF <- dbF[dbF$Time <90,]


## Change name of variable
colnames(dbF)[colnames(dbF) == "SD.RSuperCCCV"] <- "SD.R_CCCV"
colnames(dbF)[colnames(dbF) == "PAve.RSuperCCCV"] <- "PAve.R_CCCV"


#### Clustered standard errors regressions

### Base regression
reg1 = felm(SD.Base ~ Time + Time2 + PAve | Product | 0 | Product, dbF)
summary(reg1)

### Filtered by competition and same producer
reg2 <- felm(SD.RCompVar ~ Time + Time2 + PAve.RCompVar | Product | 0 | Product, dbF)
summary(reg2)

### Competition and same producer dispersion regression
reg3 <- felm(SD.CompVar ~ Time + Time2 + Ave.CompVar | Product | 0 | Product, dbF)
summary(reg3)


### Print regression outputs
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

