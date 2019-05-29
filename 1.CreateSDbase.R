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

## 2) Filtered by supermarket
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.RSuper = sd(RSuper)) 
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.RSuper = ave(RSuper))

## 3) Filtered by supermarket, competition, and same producer
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.RSuperCV = sd(RSuperCV)) 
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.RSuperCV = ave(RSuperCV)) 

## 4) Filtered by competitor and same producer
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.RCompVar = sd(RCompVar)) 
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.RCompVar = ave(RCompVar))

## 5) Filtered by supermarket, chain, and city 
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.RSuperCC = sd(RSuperCC)) 
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.RSuperCC = ave(RSuperCC))

## 6) Filtered by chain, city, supermarket, competition, and same producer
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.RSuperTodo = sd(RSuperTodo)) 
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.RSuperTodo = ave(RSuperTodo))

## 7) Filtered by competition, same producer, chain, and city
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(SD.R_CCCV = sd(R_CCCV)) 
  
  db <- db %>%
    group_by(Product, Time) %>%
    mutate(PAve.R_CCCV = ave(R_CCCV)) 

## 8) Calculate variable sum of competition and same producer (dispersion)
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

########## End ##########


################## Apply function to all databases ##################


#### (1) Create restricted database: all data

# Load database
db <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.rds")
#db <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.csv", data.table = F)
head(db)

db <- functions()
head(db)

# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", row.names = FALSE)



#### (2) Create restricted database: Montevideo

# Load database
db <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsMontevideo.rds")
head(db)

db <- functions()
head(db)


# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedMontevideo.csv", row.names = FALSE)



#### (3) Create restricted database: Original supermarkets

# Load database
db <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuper.rds")
head(db)

db <- functions()
head(db)

# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", row.names = FALSE)



#### (4) Create restricted database: Original in Montevideo

# Load database
db <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuperMdeo.rds")
head(db)

db <- functions()
head(db)

# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", row.names = FALSE)
#write.csv(db, "C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", row.names = FALSE)



################## End of script ##################