##########################################################################
###### Description; create SD, average price, and restrict database ###### 

### Libraries required
library("data.table")
library("dplyr")


###### Define functions ######

functions <- function(){

## Create Time square
  db$Time2 <- db$Time * db$Time 

## 1) Non.filtered data
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve = ave(moda)) 
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.Base = sd(moda)) 

## 2) Filtered by competitor and same producer
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.RCompBrand = sd(RCompBrand)) 
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.RCompBrand = ave(RCompBrand))

## 3) Filtered by supermarket, chain, and city
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.RStoreCC = sd(RStoreCC)) 
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.RStoreCC = ave(RStoreCC))

## 4) Filtered by store*time + category*time
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.RSTCT = sd(RSTCT)) 
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.RSTCT = ave(RSTCT)) 

## 5) Calculate variable sum of competition and same producer (dispersion)
  db$CompVar <- db$competition + db$variety
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.CompVar = sd(CompVar)) 
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(Ave.CompVar = ave(CompVar))

## Restrict database
  db <- distinct(db, Product, Time, .keep_all = TRUE)
}

#-------- End ------------


################## Apply function to all databases ##################


#### (1) Create restricted database: all data

# Load database
db <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals-new.rds")
db <- readRDS("C://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals-new.rds")
#db <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.csv", data.table = F)
head(db)

db <- functions()
db <- as.data.frame(db)
head(db)

# Save database
write.csv(db,"C://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll-new.csv")
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", row.names = FALSE)



#### (2) Create restricted database: Montevideo

# Load database
db <- readRDS("C://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsMontevideo-new.rds")
db <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsMontevideo.rds")
head(db)

db <- functions()
db <- as.data.frame(db)
head(db)


# Save database
write.csv(db,"C://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedMontevideo-new.csv")
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedMontevideo-new.csv", row.names = FALSE)



#### (3) Create restricted database: Original supermarkets

# Load database
db <- readRDS("C://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuper-new.rds")
db <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuper.rds")
head(db)

db <- functions()
db <- as.data.frame(db)
head(db)

# Save database
write.csv(db,"C://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal-new.csv")
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal-new.csv", row.names = FALSE)



#### (4) Create restricted database: Original in Montevideo

# Load database
db <- readRDS("C://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuperMdeo-new.rds")
db <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuperMdeo-new.rds")
head(db)

db <- functions()
db <- as.data.frame(db)
head(db)

# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", row.names = FALSE)
write.csv(db, "C://Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo-new.rds", row.names = FALSE)
#write.csv(db, "C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", row.names = FALSE)



################## End of script ##################