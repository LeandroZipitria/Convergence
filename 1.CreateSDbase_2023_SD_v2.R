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

  
  
## 1) Average price
  
  x <- as.data.table(x)[, PAve := mean(PMode_r, na.rm =T), by=.(Product, Time, city2)]
  
  
## 2) Standard deviation
  
   x <- as.data.table(x)[, SD.PReal := sd(PMode_r), by=.(Product, Time, city2)]

  
## 3) Category dispersion
  
  x <- as.data.table(x)[, CatDisp := sum(sh_entropy(Variety)), by=.(Category, Time, city2)]

  
## 4) Number of competitors
  
  x <- as.data.table(x)[, num_strs_comp := length(unique(Super))-1, by=.(Time, city2)]


## 5) Number of products in store
  
  
## Restrict database
  
  x <- distinct(x, Product, Time, city2, .keep_all = TRUE)

}



# Apply functions to database: aggregate  ---------------------------------------------


# Load database
db <- fread("../../Bases/2023BaseResiduals.csv")
head(db)



#### (1) Create volatility database: all data

dbrest <- fagg(db)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedAll.csv",row.names = F)


#### (2) Create restricted database: Original supermarkets

# Restrict database to stores in 2007
OS <- unique(db[db$Year == 2007,]$Super)
dbO <- db[db$Super %in% OS,]


dbrest <- fagg(dbO)
dbrest <- as.data.frame(dbrest)
head(dbrest)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedOriginal.csv",row.names = F)


### Erase aditional databases
rm(db,dbO,dbrest,OS,fagg)
gc()



# Function to create dispersion database (by supermarket chain) ----------------------------------


sh_entropy <- function(x, base=exp(1)) {
  t <- table(x)
  freq <- t/sum(t)
  ifelse(freq==0, 0, -freq * log(freq, base))
}



faggchain <- function(x){
  
  
  ## 1) Average price
  
  x <- as.data.table(x)[, PAve := mean(PMode_r, na.rm =T), by=.(Product, Time, city2, chain)]
  

  ## 2) Standard deviation
  
  x <- as.data.table(x)[, SD.PReal := sd(PMode_r), by=.(Product, Time, city2, chain)]
  
  
  ## 3) Category dispersion
  
  x <- as.data.table(x)[, CatDisp := sum(sh_entropy(Variety)), by=.(Category, Time, city2, chain)]
  
  
  ## 4) Number of total stores
  
  x <- as.data.table(x)[, num_stores_total := length(unique(Super))-1, by=.(Time, city2)]
  
  
  ## Restrict database
  
  x <- distinct(x, Product, Time, city2, chain, .keep_all = TRUE)
  
}



# Apply functions to database ---------------------------------------------


# Load database
db <- fread("../../Bases/2023BaseResiduals.csv")
head(db)


#### (1) Create volatility database: all data

# count number of stores by chain
db <- as.data.table(db)[, num_stores_chain := length(unique(Super)), by=.(Time, city2, chain)]

dbrest <- faggchain(db)
dbrest <- as.data.frame(dbrest)
head(dbrest)

dbrest$is.chain = ifelse(dbrest$chain == "Sin Cadena","Indep.","Chains")
dbrest$num_strs_comp <- ifelse(dbrest$is.chain == "Chains",dbrest$num_stores_total - dbrest$num_stores_chain, 
                               dbrest$num_stores_total)
dbrest$num_stores_chain <- ifelse(dbrest$is.chain == "Chains",dbrest$num_stores_chain, 0)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedAll.chains.csv",row.names = F)



#### (2) Create restricted database: Original supermarkets

# Restrict database to stores in 2007
OS <- unique(db[db$Year == 2007,]$Super)
dbO <- db[db$Super %in% OS,]

# count number of stores by chain
dbO <- as.data.table(dbO)[, num_stores := length(unique(Super)), by=.(Time, city2, chain)]

dbrest <- faggchain(dbO)
dbrest <- as.data.frame(dbrest)
head(dbrest)

dbrest$is.chain = ifelse(dbrest$chain == "Sin Cadena","Indep.","Chains")
dbrest$num_strs_comp <- ifelse(dbrest$is.chain == "Chains",dbrest$num_stores_total - dbrest$num_stores_chain, 
                               dbrest$num_stores_total)
dbrest$num_stores_chain <- ifelse(dbrest$is.chain == "Chains",dbrest$num_stores_chain, 0)

# Save database
fwrite(dbrest,"../../Bases/2023.RestrictedOriginal.chains.csv",row.names = F)



### Erase additional databases
rm(db,dbO,dbrest,OS,faggchain,sh_entropy)
gc()


################## End of script ##################
