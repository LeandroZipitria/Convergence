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
db <- fread("../../Bases/processed/2023_dbase.csv", data.table = F)
head(db)

# Price dispersion database
dbDisp <- fread("../../Bases/processed/2023.Aggregate_test.csv", data.table = F)
head(dbDisp)

# ech database
ech <- fread("../../Bases/raw/ECH_2007a2021M6.csv",data.table = F)



#### Price database

# Adjusted log price
mean(db$lPMode_r)
sd(db$lPMode_r)

# SD Price
mean(dbDisp$SD.PReal,na.rm = T)
sd(dbDisp$SD.PReal,na.rm = T)

# Entropy
mean(dbDisp$CatDisp)
sd(dbDisp$CatDisp)

# Competing stores
mean(dbDisp$num_strs_comp)
sd(dbDisp$num_strs_comp)

# Dispersion of number of products
mean(dbDisp$SD.shProd, na.rm = T)
sd(dbDisp$SD.shProd, na.rm = T)


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


#### ECH

# Unemployment rate
ech$ur <- ech$desocupados / (ech$ocupados + ech$desocupados)
mean(ech$ur, na.rm = T)
sd(ech$ur, na.rm = T)

# log population
ech$pop <- (ech$ocupados + ech$desocupados)
ech$pop <- ifelse(ech$pop ==0,1,ech$pop)
mean(log(ech$pop), na.rm = T)
sd(log(ech$pop), na.rm = T)

# log income
mean(log(ech$ing_pc_cpi), na.rm = T)
sd(log(ech$ing_pc_cpi), na.rm = T)


# sd income 
mean(log(ech$stdev_ipc_pc_cpi), na.rm = T)
sd(log(ech$stdev_ipc_pc_cpi), na.rm = T)



# Descriptives: Chains ----------------------------------------------------


# Load database between chains
db_bch <- fread("../../Bases/processed/2023.Between.chains.csv", data.table = F)

# Load database within chains
db_wch <- fread("../../Bases/processed/2023.Within.chains.csv", data.table = F)
db_wch <- db_wch[db_wch$is.chain == "Chains",] # pick only chains, exclude independent stores


# SD Price: within chains
mean(db_wch$SD.PReal,na.rm = T)
sd(db_wch$SD.PReal,na.rm = T)


# SD Price: between chains
mean(db_bch$SD.PReal,na.rm = T)
sd(db_bch$SD.PReal,na.rm = T)

