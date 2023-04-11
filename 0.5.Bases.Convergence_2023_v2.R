############################################################################
### Description: This script create the filtered series to be used later ###
############################################################################

### This file is run on cluster

unlink(".RData") 
gc()

library("data.table")
library("dplyr")
library(fixest)


##### Create database for all data #####


## Load database
dbM <- fread("../../Bases/2023_dbase.csv", data.table = F)
head(dbM)

## logs of prices
sum(is.na(dbM$PMode))
dbM <- dbM[!is.na(dbM$PMode),]
dbM$lPmode <- log(dbM$PMode)
sum(is.na(dbM$lPmode))
dbase$PMode <- NULL


# If CPI is needed
ipc <- fread("../../Bases/IPC.csv", data.table = F)
ipc$IPC <- NULL
ipc$index <- 100
for(i in 2:189) {
  ipc$index[i] <- (ipc$index[i-1] * (1+ (ipc$Monthly.change[i]/100)))
}
ipc$Time <- c(1:189)
ipc$`Mes y año` <- ipc$Monthly.change <- NULL
dbM <- setDT(dbM)[setDT(ipc), on=c("Time" = "Time"), cpi:= i.index]
# dbM$PmodaOrig <- round(exp(dbM$moda /100), digits = 2)
# Deflacted prices
dbM$PMode_r <- log(exp(dbM$lPmode)/dbM$cpi)


# Residual database: original ---------------------------------------------

dbM$Time2 <- dbM$Time * dbM$Time

## 1) Filter: Time trend
regtrend <- feols(lPmode ~ 1 | as.factor(Time), dbM)
etable(regtrend)
dbM$RTime <- as.numeric(regtrend$residuals)


## Save database
head(dbM)
fwrite(dbM, "../../Bases/2023BaseResiduals.csv",row.names = F)
rm(dbM,regtrend,ipc)
gc()



## Next file: 1.CreateSDbase_2023_SD_v2.R

################## End of script ##################