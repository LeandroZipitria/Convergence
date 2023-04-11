## libraries

library(data.table)


# Figure X: dispersion all database ---------------------------------------

# Price dispersion database
db <- fread("../../Bases/2023.RestrictedAll.csv", data.table = F)
head(db)

# CPI datatase
# cpi <- fread("c:\\Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/IPC.csv", data.table = F) # FCS
# cpi <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/IPC.csv", data.table = F) # casa
# head(cpi)
# cpi$IPC <- NULL
# cpi$index <- 100
# for(i in 2:177) {
#   cpi$index[i] <- cpi$index[i-1] * (1+ (cpi$Monthly.change[i]/100))
# }
# cpi$Time <- c(1:177)
# colnames(cpi)[1] <- "year_month"
# cpi$year_month <- zoo::as.yearmon(lubridate::parse_date_time(cpi$year_month, orders = "my"), "%m - %Y")
# cpi$year_month2 <- NULL
# 
# write.csv(cpi, "~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/CPI.csv", row.names = F)

cpi <- fread("../../Bases/CPI.csv", data.table = F) # casa



# Descriptive statistics --------------------------------------------------


# Price database
db <- fread("../../Bases/2023BaseResiduals.csv", data.table = F)
head(db)


# Price dispersion database
dbDisp <- fread("../../Bases/2023.RestrictedAll.csv", data.table = F)
head(dbDisp)

# Price dispersion chains database
dbsup <- fread("../../Bases/2023.RestrictedAll.chains.csv", data.table = F)
head(dbDisp)


# Adjusted log price
mean(db$PMode_r)
sd(db$PMode_r)

# CV Price
mean(dbDisp$CV.PReal,na.rm = T)
sd(dbDisp$CV.PReal,na.rm = T)

# Entropy
mean(dbDisp$CatDisp)
sd(dbDisp$CatDisp)

# Competing stores
mean(dbDisp$num_strs_comp)
sd(dbDisp$num_strs_comp)

# Number of observations price database
nrow(db)

# Number of stores
length(unique(db$Super))

# Number of Chains
length(unique(db$chain))-1

# Number of markets
length(unique(db$city2))

# Number of Products
length(unique(db$Product))

# Number of Categories
length(unique(db$Category))

# Entropy for chains
mean(dbsup[dbsup$is.chain == "Chains",]$CatDisp)
sd(dbsup[dbsup$is.chain == "Chains",]$CatDisp)

# Entropy index for independent stores
mean(dbsup[dbsup$is.chain == "Indep.",]$CatDisp)
sd(dbsup[dbsup$is.chain == "Indep.",]$CatDisp)


##### Plot time effects

db$ProdTime <- as.factor(interaction(db$Product, db$Time))

## Coeff variation
rgCV <-  feols(CV.Trend ~ as.factor(Time) | Product + city, data = db)
summary(rgCV)
names(rgCV$coefficients) <- gsub("as.factor\\(Time\\)","Time ",names(rgCV$coefficients))
jtools::plot_coefs(rgCV, cluster = c("ProdTime"))

## Standard Deviation
rgSD <-  feols(SD.Base ~ as.factor(Time) | Product, data = db)
summary(rgSD)
names(rgSD$coefficients) <- gsub("as.factor\\(Time\\)","Time ",names(rgSD$coefficients))
jtools::plot_coefs(rgSD, cluster = c("ProdTime"))
# Note: standard deviation first decrease, then increase!

## Median Price
rgMean <-  feols(Mean.Base ~ as.factor(Time) | Product, data = db)
summary(rgMean)
names(rgMean$coefficients) <- gsub("as.factor\\(Time\\)","Time ",names(rgMean$coefficients))
jtools::plot_coefs(rgMean, cluster = c("ProdTime"))
# Average price consistently increase

