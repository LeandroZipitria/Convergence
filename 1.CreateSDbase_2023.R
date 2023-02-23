##########################################################################
###### Description; create SD, average price, and restrict database ###### 


### Libraries required
library("data.table")
library("dplyr")


# Function to create dispersion database ----------------------------------


sh_entropy <- function(x, base=exp(1)) {
  t <- table(x)
  freq <- t/sum(t)
  ifelse(freq==0, 0, -freq * log(freq, base))
}



fagg <- function(x){

## 1) Non.filtered data
  
#  x <- as.data.table(x)[, PAve := mean(moda,na.rm = T), by=.(Product, Time)]
#  x <- as.data.table(x)[, SD.Base := sd(moda), by=.(Product, Time)]
#  x <- as.data.table(x)[, IQR.Base := IQR(moda), by=.(Product, Time)]
  x <- as.data.table(x)[, CV.Base := sd(moda)/mean(moda,na.rm = T), by=.(Product, Time)]
  x <- as.data.table(x)[, Mean.Base := mean(moda,na.rm = T), by=.(Product, Time)]
  x <- as.data.table(x)[, SD.Base := sd(moda), by=.(Product, Time)]


## 2) Trend filtered data
  
  #  x <- as.data.table(x)[, PAve := mean(moda,na.rm = T), by=.(Product, Time)]
  #  x <- as.data.table(x)[, SD.Base := sd(moda), by=.(Product, Time)]
  #  x <- as.data.table(x)[, IQR.Base := IQR(moda), by=.(Product, Time)]
  x <- as.data.table(x)[, CV.Trend := sd(RTime)/mean(RTime,na.rm = T), by=.(Product, Time)]
  
  
## 2) Filtered by Variety
  
#  x <- as.data.table(x)[, PAve.RVariety := mean(RVariety,na.rm = T), by=.(Product, Time)]
#  x <- as.data.table(x)[, SD.RVariety := sd(RVariety), by=.(Product, Time)]
#  x <- as.data.table(x)[, IQR.RVariety := IQR(RVariety), by=.(Product, Time)]
  x <- as.data.table(x)[, CV.RVariety := sd(RVariety)/mean(RVariety,na.rm = T), by=.(Product, Time)]


## 3) Filtered by supermarket, chain, and city
  
#  x <- as.data.table(x)[, PAve.RStoreCC := mean(RStoreCC,na.rm = T), by=.(Product, Time)]
#  x <- as.data.table(x)[, SD.RStoreCC := sd(RStoreCC), by=.(Product, Time)]
#  x <- as.data.table(x)[, IQR.RStoreCC := IQR(RStoreCC), by=.(Product, Time)]
#  x <- as.data.table(x)[, CV.RStoreCC := sd(RStoreCC)/mean(RStoreCC,na.rm = T), by=.(Product, Time)]
  
  
## 4) Filtered by Category * Time
  
#  x <- as.data.table(x)[, PAve.RSTCT := mean(RSTCT,na.rm = T), by=.(Product, Time)]
#  x <- as.data.table(x)[, SD.RSTCT := sd(RSTCT), by=.(Product, Time)]
#  x <- as.data.table(x)[, IQR.RSTCT := IQR(RSTCT), by=.(Product, Time)]
  x <- as.data.table(x)[, CV.RCT := sd(RCT)/mean(RCT,na.rm = T), by=.(Product, Time)]

    
## 5) Filtered by Chain * Time
  
  x <- as.data.table(x)[, CV.RChT := sd(RChT)/mean(RChT,na.rm = T), by=.(Product, Time)]
  
  
## 6) Category dispersion
  
  x <- as.data.table(x)[, CatDisp := sum(sh_entropy(Variety)), by=.(Category, Time)]

  
## Restrict database
  x <- distinct(x, Product, Time, .keep_all = TRUE)
}



# Apply functions to database ---------------------------------------------


# Load database
db <- fread("../../Bases/2023BaseResiduals.csv")
head(db)
db$Time2 <- db$Time * db$Time



#### (1) Create volatility database: all data

dbrest <- fagg(db)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedAll.csv",row.names = F)


#### (2) Create restricted database: Montevideo

# Restrict to Montevideo city
dbM <- db[db$depto == "Montevideo" ,]

dbrest <- fagg(dbM)
dbrest <- as.data.frame(dbrest)
head(dbrest)


# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedMontevideo.csv",row.names = F)



#### (3) Create restricted database: Original supermarkets

# Restrict database to stores in 2007
OS <- unique(db[db$Year == 2007,]$Super)
dbO <- db[db$Super %in% OS,]


dbrest <- fagg(dbO)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedOriginal.csv",row.names = F)



#### (4) Create restricted database: Original in Montevideo

# Restrict to Montevideo city for stores in 2007
OS <- unique(dbM[dbM$Year == 2007,]$Super)
dbMr <- dbM[dbM$Super %in% OS,]

dbrest <- fagg(dbMr)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedOriginMdeo.csv",row.names = F)


### Erase aditional databases
rm(db,dbO,dbM,dbMr,dbrest,OS)




# Function to create dispersion database (by supermarket chain) ----------------------------------


sh_entropy <- function(x, base=exp(1)) {
  t <- table(x)
  freq <- t/sum(t)
  ifelse(freq==0, 0, -freq * log(freq, base))
}



faggchain <- function(x){
  
  ## 1) Non.filtered data
  
  #  x <- as.data.table(x)[, PAve := mean(moda,na.rm = T), by=.(Product, Time)]
  #  x <- as.data.table(x)[, SD.Base := sd(moda), by=.(Product, Time)]
  #  x <- as.data.table(x)[, IQR.Base := IQR(moda), by=.(Product, Time)]
  x <- as.data.table(x)[, CV.Base := sd(moda)/mean(moda,na.rm = T), by=.(Product, Time, chain)]
  x <- as.data.table(x)[, Mean.Base := mean(moda,na.rm = T), by=.(Product, Time, chain)]
  x <- as.data.table(x)[, SD.Base := sd(moda), by=.(Product, Time, chain)]
  
  
  ## 2) Trend filtered data
  
  #  x <- as.data.table(x)[, PAve := mean(moda,na.rm = T), by=.(Product, Time)]
  #  x <- as.data.table(x)[, SD.Base := sd(moda), by=.(Product, Time)]
  #  x <- as.data.table(x)[, IQR.Base := IQR(moda), by=.(Product, Time)]
  x <- as.data.table(x)[, CV.Trend := sd(RTime)/mean(RTime,na.rm = T), by=.(Product, Time, chain)]
  
  
  ## 2) Filtered by Variety
  
  #  x <- as.data.table(x)[, PAve.RVariety := mean(RVariety,na.rm = T), by=.(Product, Time)]
  #  x <- as.data.table(x)[, SD.RVariety := sd(RVariety), by=.(Product, Time)]
  #  x <- as.data.table(x)[, IQR.RVariety := IQR(RVariety), by=.(Product, Time)]
  x <- as.data.table(x)[, CV.RVariety := sd(RVariety)/mean(RVariety,na.rm = T), by=.(Product, Time, chain)]
  
  
  ## 3) Filtered by supermarket, chain, and city
  
  #  x <- as.data.table(x)[, PAve.RStoreCC := mean(RStoreCC,na.rm = T), by=.(Product, Time)]
  #  x <- as.data.table(x)[, SD.RStoreCC := sd(RStoreCC), by=.(Product, Time)]
  #  x <- as.data.table(x)[, IQR.RStoreCC := IQR(RStoreCC), by=.(Product, Time)]
  # x <- as.data.table(x)[, CV.RStoreCC := sd(RStoreCC)/mean(RStoreCC,na.rm = T), by=.(Product, Time, chain)]
  
  
  ## 4) Filtered by Category * Time
  
  #  x <- as.data.table(x)[, PAve.RSTCT := mean(RSTCT,na.rm = T), by=.(Product, Time)]
  #  x <- as.data.table(x)[, SD.RSTCT := sd(RSTCT), by=.(Product, Time)]
  #  x <- as.data.table(x)[, IQR.RSTCT := IQR(RSTCT), by=.(Product, Time)]
  x <- as.data.table(x)[, CV.RCT := sd(RCT)/mean(RCT,na.rm = T), by=.(Product, Time, chain)]
  
  
  ## 5) Filtered by Category * Time
  
  x <- as.data.table(x)[, CV.RChT := sd(RChT)/mean(RChT,na.rm = T), by=.(Product, Time, chain)]
  
  
  ## 6) Category dispersion
  
  x <- as.data.table(x)[, CatDisp := sum(sh_entropy(Variety)), by=.(Category, Time, chain)]
  
  
  ## Restrict database
  x <- distinct(x, Product, Time, chain, .keep_all = TRUE)
}



# Apply functions to database ---------------------------------------------


# Load database
db <- fread("../../Bases/2023BaseResiduals.csv")
head(db)
db$Time2 <- db$Time * db$Time



#### (1) Create volatility database: all data

# count number of stores by chain
db <- as.data.table(db)[, num_stores := length(unique(Super)), by=.(Time, chain)]

dbrest <- faggchain(db)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedAll.chains.csv",row.names = F)


#### (2) Create restricted database: Montevideo

# Restrict to Montevideo city
dbM <- db[db$depto == "Montevideo" ,]

# count number of stores by chain
dbM <- as.data.table(dbM)[, num_stores := length(unique(Super)), by=.(Time, chain)]

dbrest <- faggchain(dbM)
dbrest <- as.data.frame(dbrest)
head(dbrest)


# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedMontevideo.chains.csv",row.names = F)



#### (3) Create restricted database: Original supermarkets

# Restrict database to stores in 2007
OS <- unique(db[db$Year == 2007,]$Super)
dbO <- db[db$Super %in% OS,]

# count number of stores by chain
dbO <- as.data.table(dbO)[, num_stores := length(unique(Super)), by=.(Time, chain)]

dbrest <- faggchain(dbO)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedOriginal.chains.csv",row.names = F)



#### (4) Create restricted database: Original in Montevideo

# Restrict to Montevideo city for stores in 2007
OS <- unique(dbM[dbM$Year == 2007,]$Super)
dbMr <- dbM[dbM$Super %in% OS,]

# count number of stores by chain
dbMr <- as.data.table(dbMr)[, num_stores := length(unique(Super)), by=.(Time, chain)]

dbrest <- faggchain(dbMr)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedOriginMdeo.chains.csv",row.names = F)


### Erase aditional databases
rm(db,dbO,dbM,dbMr,dbrest,OS)



################## End of script ##################