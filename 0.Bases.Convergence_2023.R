############################################################################
### Description: This script create the filtered series to be used later ###
############################################################################

### This file is run on cluster

unlink(".RData") 

library("data.table")
library("dplyr")
library(fixest)


##### Create database for all data #####

## Load database
dbM <- fread("../../Bases/2023_dbase.csv", data.table = F)
head(dbM)

# If CPI is needed
# ipc <- fread("c:\\Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/IPC.csv", data.table = F) # FCS
# # ipc <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/IPC.csv", data.table = F) # casa
# ipc$IPC <- NULL
# ipc$index <- 100
# for(i in 2:177) {
#   ipc$index[i] <- ipc$index[i-1] * (1+ (ipc$Monthly.change[i]/100))
# }
# ipc$Time <- c(1:177)
# ipc$`Mes y año` <- NULL
# dbM <- merge(dbM, ipc, by = "Time")
# # dbM$PmodaOrig <- round(exp(dbM$moda /100), digits = 2)
# dbM$Mode_price_real <- log((dbM$PMode/(dbM$index /100)))


# Residual database: original ---------------------------------------------

dbM$moda <- dbM$moda/100 #(if previously multiplied by 100)

## 1) Filter: Time trend
dbM$RTime <- as.numeric(feols(moda ~ 1 | as.factor(Time), dbM)$residuals)
## 2) Filter: Varieties and time
dbM$RVariety <- as.numeric(feols(moda ~ 1 | as.factor(Variety) + as.factor(Time), dbM)$residuals)
## 3) Filter: Supermarket + city + time
# dbM$RStoreCC <- as.numeric(feols(moda ~ 1 | as.factor(Super) + as.factor(Time) +
#                                   as.factor(city), dbM)$residuals)
## 4) Filter: Category * Time
dbM$RCT <- as.numeric(feols(moda ~ 1 |  as.factor(Time)^as.factor(Category), dbM)$residuals)

## 5) Filter: Chain * Time
dbM$RChT <- as.numeric(feols(moda ~ 1 |  as.factor(Time)^as.factor(chain), dbM)$residuals)


## Save database
head(dbM)
fwrite(dbM, "../../Bases/2023BaseResiduals.csv",row.names = F)
rm(dbM)
gc()



## Next file: 1.CreateSDbase_2022.R

################## End of script ##################