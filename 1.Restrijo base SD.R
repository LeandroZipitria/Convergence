#library("data.table", lib.loc="~/R/lib/") # FING FING
library("data.table") # CASA CASA
library("dplyr")


setwd("/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Rutinas/")


#### (1) Create restricted database: all data ####

# Load database
db <- get(load("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.Rdata"))
#db <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.csv", data.table = F)
head(db)

# Create Time square
db$Time2 <- db$Time * db$Time


## 1) data sin filtrar

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve = ave(moda)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.Base = sd(moda)) 


## 2) filtrado efecto super

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuper = sd(RSuper)) 
db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuper = ave(RSuper)) 


## 3) filtrado efecto super + variedad + competencia

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCV = sd(RSuperCV)) 
db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCV = ave(RSuperCV)) 


## 4) filtrado competencia + variedad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RCompVar = sd(RCompVar)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RCompVar = ave(RCompVar)) 


## 5) Filtrado super + cadena + ciudad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCC = sd(RSuperCC)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCC = ave(RSuperCC)) 


## 6) Filtrado por todo

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperTodo = sd(RSuperTodo)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperTodo = ave(RSuperTodo)) 


## 7) Filtrado por competencia + variedad + chain + ciudad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCCCV = sd(RSuperCCCV)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCCCV = ave(RSuperCCCV)) 


### Restrict database

db <- distinct(db, Product, Time, .keep_all = TRUE) # pick one observation by month
colnames(db)

# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", row.names = FALSE)


#### (2) Create restricted database: Montevideo ####

# Load database
db <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsMontevideo.csv", data.table = F)
head(db)

# Create Time square
db$Time2 <- db$Time * db$Time


## 1) data sin filtrar

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve = ave(moda)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.Base = sd(moda)) 


## 2) filtrado efecto super

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuper = sd(RSuper)) 
db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuper = ave(RSuper)) 


## 3) filtrado efecto super + variedad + competencia

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCV = sd(RSuperCV)) 
db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCV = ave(RSuperCV)) 


## 4) filtrado competencia + variedad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RCompVar = sd(RCompVar)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RCompVar = ave(RCompVar)) 


## 5) Filtrado super + cadena + ciudad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCC = sd(RSuperCC)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCC = ave(RSuperCC)) 


## 6) Filtrado por todo

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperTodo = sd(RSuperTodo)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperTodo = ave(RSuperTodo)) 


## 7) Filtrado por competencia + variedad + chain + ciudad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCCCV = sd(RSuperCCCV)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCCCV = ave(RSuperCCCV)) 



### Restrict database

db <- distinct(db, Product, Time, .keep_all = TRUE) # pick one observation by month
colnames(db)

# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedMontevideo.csv", row.names = FALSE)


#### (3) Create restricted database: Original supermarkets ####

# Load database
db <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuper.csv", data.table = F)
head(db)

# Create Time square
db$Time2 <- db$Time * db$Time


## 1) data sin filtrar

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve = ave(moda)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.Base = sd(moda)) 


## 2) filtrado efecto super

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuper = sd(RSuper)) 
db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuper = ave(RSuper)) 


## 3) filtrado efecto super + variedad + competencia

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCV = sd(RSuperCV)) 
db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCV = ave(RSuperCV)) 


## 4) filtrado competencia + variedad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RCompVar = sd(RCompVar)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RCompVar = ave(RCompVar)) 


## 5) Filtrado super + cadena + ciudad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCC = sd(RSuperCC)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCC = ave(RSuperCC)) 


## 6) Filtrado por todo

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperTodo = sd(RSuperTodo)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperTodo = ave(RSuperTodo)) 


## 7) Filtrado por competencia + variedad + chain + ciudad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCCCV = sd(RSuperCCCV)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCCCV = ave(RSuperCCCV)) 



### Restrict database

db <- distinct(db, Product, Time, .keep_all = TRUE) # pick one observation by month
colnames(db)

# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", row.names = FALSE)



#### (4) Create restricted database: Original in Montevideo ####

# Load database
db <- get(load("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.Rdata"))
db <- get(load("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsOrigSuper.Rdata"))

#db <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.csv", data.table = F)
head(db)

# Restrict to Montevideo
db <- db[db$depto.number == 10,] # Number 10 is Montevideo

# Create Time square
db$Time2 <- db$Time * db$Time


## 1) data sin filtrar

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve = ave(moda)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.Base = sd(moda)) 


## 2) filtrado efecto super

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuper = sd(RSuper)) 
db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuper = ave(RSuper)) 


## 3) filtrado efecto super + variedad + competencia

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCV = sd(RSuperCV)) 
db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCV = ave(RSuperCV)) 


## 4) filtrado competencia + variedad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RCompVar = sd(RCompVar)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RCompVar = ave(RCompVar)) 


## 5) Filtrado super + cadena + ciudad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCC = sd(RSuperCC)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCC = ave(RSuperCC)) 


## 6) Filtrado por todo

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperTodo = sd(RSuperTodo)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperTodo = ave(RSuperTodo)) 


## 7) Filtrado por competencia + variedad + chain + ciudad

db <- db %>%
  group_by(Product, Time) %>%
  mutate(SD.RSuperCCCV = sd(RSuperCCCV)) 

db <- db %>%
  group_by(Product, Time) %>%
  mutate(PAve.RSuperCCCV = ave(RSuperCCCV)) 


### Restrict database

db <- distinct(db, Product, Time, .keep_all = TRUE) # pick one observation by month
colnames(db)

# Save database
write.csv(db, "/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", row.names = FALSE)
write.csv(db, "C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", row.names = FALSE)

