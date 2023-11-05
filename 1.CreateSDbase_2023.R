##########################################################################
###### Description; create SD, average price, and restrict database ###### 


### Libraries required
library("data.table")
library("dplyr")
gc()



# Full database:  ---------------------------------------------------------



## Function to create dispersion database ----------------------------------


sh_entropy <- function(x, base=exp(1)) {
  t <- table(x)
  freq <- t/sum(t)
  ifelse(freq==0, 0, -freq * log(freq, base))
}



fagg <- function(x){

  
  
## 1) Average price
  
  x <- as.data.table(x)[, PAve := mean(lPMode_r, na.rm =T), by=.(Product, Time, city2)]
  
  
## 2) Standard deviation
  
   x <- as.data.table(x)[, SD.PReal := sd(lPMode_r), by=.(Product, Time, city2)]

  
## 3) Category dispersion
  
  x <- as.data.table(x)[, CatDisp := sum(sh_entropy(Variety)), by=.(Category, Time, city2)]

  
## 4) Number of competitors
  
  x <- as.data.table(x)[, num_strs_comp := length(unique(Super))-1, by=.(Time, city2)]


## 5) Number of products in store
  
  x <- as.data.table(x)[, SD.shProd := sd(shProd), by=.(Time, city2)]
  
  
## Restrict database
  
  x <- distinct(x, Product, Time, city2, .keep_all = TRUE)

}



## Apply functions to database: aggregate  ---------------------------------------------


# Load database
db <- fread("../../Bases/processed/2023_dbase.csv")
head(db)



#### (1) Create volatility database: all data

dbrest <- fagg(db)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/processed/2023.Aggregate.csv",row.names = F)


#### (2) Create restricted database: Original supermarkets

# Restrict database to stores in 2007
OS <- unique(db[db$Year == 2007,]$Super)
dbO <- db[db$Super %in% OS,]


dbrest <- fagg(dbO)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/processed/2023.Aggregate.Original.csv",row.names = F)


### Erase aditional databases
rm(db,dbO,dbrest,OS,fagg)
gc()



# Between chains:  ---------------------------------------------------------



## Function to create dispersion database ----------------------------------


sh_entropy <- function(x, base=exp(1)) {
  t <- table(x)
  freq <- t/sum(t)
  ifelse(freq==0, 0, -freq * log(freq, base))
}



fbetch <- function(x){
  
  
  
  ## 1) Average price
  
  x <- as.data.table(x)[, PAve := mean(lPMode_r, na.rm =T), by=.(Product, Time, city2)]
  
  
  ## 2) Standard deviation
  
  x <- as.data.table(x)[, SD.PReal := sd(lPMode_r), by=.(Product, Time, city2)]
  
  
  ## 3) Category dispersion
  
  x <- as.data.table(x)[, CatDisp := sum(sh_entropy(Variety)), by=.(Category, Time, city2)]
  
  
  ## 4) Number of competitors
  
  x <- as.data.table(x)[, num_strs_comp := length(unique(Super))-1, by=.(Time, city2)]
  
  
  ## 5) Number of products in store
  
  x <- as.data.table(x)[, SD.shProd := sd(shProd), by=.(Time, city2)]
  
  
  ## Restrict database
  
  x <- distinct(x, Product, Time, city2, .keep_all = TRUE)
  
}



## Apply functions to database: between chains  ---------------------------------------------


# Load database
db <- fread("../../Bases/processed/2023_dbase.csv")
head(db)


##### Operation of database

# count number of stores by chain
db <- as.data.table(db)[, num_stores_chain := length(unique(Super)), by=.(Time, city2, chain)]


# find median price, variety and share of products for each chain
db <- as.data.table(db)[, Pmedian := median(lPMode_r,na.rm = T), by=.(Product, Time, city2, chain)]
db <- as.data.table(db)[, Varmedian := median(Variety,na.rm = T), by=.(Product, Time, city2, chain)]
db <- as.data.table(db)[, shProdmedian := median(shProd,na.rm = T), by=.(Product, Time, city2, chain)]

# substitute variables
db$lPMode_r <- ifelse(db$chain != "sin cadena", db$Pmedian, db$lPMode_r)
db$Variety <- ifelse(db$chain != "sin cadena", db$Varmedian, db$Variety)
db$shProd <- ifelse(db$chain != "sin cadena", db$shProdmedian, db$shProd)
db$Pmedian <- db$Varmedian <- db$shProdmedian <- NULL

# erase duplicate observations
dbch <- db[db$chain != "sin cadena",]
dbnch <- db[db$chain == "sin cadena",]
dbch <- unique(as.data.table(dbch), by=c("Product", "Time", "city2","chain"))
db <- rbind(dbch,dbnch)
rm(dbch, dbnch)



#### (1) Create volatility database: between chains

dbrest <- fbetch(db)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/processed/2023.Between.chains.csv",row.names = F)


# #### (2) Create restricted database: Original supermarkets
# 
# # Restrict database to stores in 2007
# OS <- unique(db[db$Year == 2007,]$Super)
# dbO <- db[db$Super %in% OS,]
# 
# 
# dbrest <- fbetch(dbO)
# dbrest <- as.data.frame(dbrest)
# head(dbrest)
# 
# # Save database
# fwrite(dbrest,"../../Bases/2023.Between.chains.original.csv",row.names = F)


### Erase aditional databases
rm(db,dbO,dbrest,OS,fbetch)
gc()



# Within chains  ---------------------------------------------------------


## Function to create dispersion database (by supermarket chain) ----------------------------------


sh_entropy <- function(x, base=exp(1)) {
  t <- table(x)
  freq <- t/sum(t)
  ifelse(freq==0, 0, -freq * log(freq, base))
}



fwchain <- function(x){
  
  
  ## 1) Average price
  
  x <- as.data.table(x)[, PAve := mean(lPMode_r, na.rm =T), by=.(Product, Time, city2, chain)]
  

  ## 2) Standard deviation
  
  x <- as.data.table(x)[, SD.PReal := sd(lPMode_r), by=.(Product, Time, city2, chain)]
  
  
  ## 3) Category dispersion
  
  x <- as.data.table(x)[, CatDisp := sum(sh_entropy(Variety)), by=.(Category, Time, city2, chain)]
  
  
  ## 4) Number of total stores
  
  x <- as.data.table(x)[, num_stores_total := length(unique(Super))-1, by=.(Time, city2)]
  
  
  ## 5) Number of products in store
  
  x <- as.data.table(x)[, SD.shProd := sd(shProd), by=.(Time, city2, chain)]
  
  
  ## Restrict database
  
  x <- distinct(x, Product, Time, city2, chain, .keep_all = TRUE)
  
}



## Apply functions to database ---------------------------------------------


# Load database
db <- fread("../../Bases/processed/2023_dbase.csv")
head(db)
gc()


#### (1) Create volatility database: all data

# count number of stores by chain
db <- as.data.table(db)[, num_stores_chain := length(unique(Super)), by=.(Time, city2, chain)]

dbrest <- fwchain(db)
dbrest <- as.data.frame(dbrest)
head(dbrest)

dbrest$is.chain = ifelse(dbrest$chain == "sin cadena","Indep.","Chains")
table(dbrest$is.chain)
dbrest$num_strs_comp <- ifelse(dbrest$is.chain == "Chains",dbrest$num_stores_total - dbrest$num_stores_chain, 
                               dbrest$num_stores_total)
dbrest$num_stores_chain <- ifelse(dbrest$is.chain == "Chains",dbrest$num_stores_chain, 0)


# Save database
fwrite(dbrest,"../../Bases/processed/2023.Within.chains.csv",row.names = F)



# #### (2) Create restricted database: Original supermarkets
# 
# # Restrict database to stores in 2007
# OS <- unique(db[db$Year == 2007,]$Super)
# dbO <- db[db$Super %in% OS,]
# 
# # count number of stores by chain
# dbO <- as.data.table(dbO)[, num_stores := length(unique(Super)), by=.(Time, city2, chain)]
# 
# dbrest <- fwchain(dbO)
# dbrest <- as.data.frame(dbrest)
# head(dbrest)
# 
# dbrest$is.chain = ifelse(dbrest$chain == "Sin Cadena","Indep.","Chains")
# dbrest$num_strs_comp <- ifelse(dbrest$is.chain == "Chains",dbrest$num_stores_total - dbrest$num_stores_chain, 
#                                dbrest$num_stores_total)
# dbrest$num_stores_chain <- ifelse(dbrest$is.chain == "Chains",dbrest$num_stores_chain, 0)
# 
# # Save database
# fwrite(dbrest,"../../Bases/2023.Within.chains.original.csv",row.names = F)



### Erase additional databases
rm(db,dbO,dbrest,OS,fwchain,sh_entropy)
gc()


################## End of script ##################
