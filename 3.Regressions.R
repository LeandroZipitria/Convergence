##########################################################
###### Description: Price regressions for the paper ######

## Libraries used

library("lfe")
library(stargazer)

## Load database
dbF <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.rds")
#dbF <- get(load("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))


### Create as factors ####
dbF$Super <- as.factor(dbF$Super)
dbF$Category <-as.factor(dbF$Category)
dbF <- dbF[dbF$Time < 90,]
dbF$Time <-as.factor(dbF$Time)
dbF$variety <- as.factor(dbF$variety)
dbF$competition <- as.factor(dbF$competition)



###########################################
############  Price Regression ############

# No cluster
areg1 <- felm(moda ~ variety + competition |Super:Time + Category:Time| 0, dbF) ## ok
areg11 <- summary(areg1)
areg11
stargazer(areg1, title="Results", align=TRUE)

# Cluster by Supermarket
areg1 <- felm(moda ~ variety + competition |Super:Time + Category:Time| 0 | Super, dbF) ## ok
areg11 <- summary(areg1)
areg11
stargazer(areg1, title="Results", align=TRUE)

# Cluster by Supermarket and Time
areg1 <- felm(moda ~ variety + competition |Super:Time + Category:Time| 0 | Super + Time, dbF) ## ok
areg11 <- summary(areg1)
areg11
stargazer(areg1, title="Results", align=TRUE)


############ End of script ############
#######################################


