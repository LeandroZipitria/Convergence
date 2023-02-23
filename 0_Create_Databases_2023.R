################################################
### SCRIPT TO CREATE PRICE DIFFERENCES #########
################################################


########### Common stuff  ################

##### PART 1: LOAD PRICE DATABASE

library("data.table") 
library("dplyr")
## load price database
dbase <- fread("../../Bases/Base2021MonthlyClean.csv", data.table = F)
dbase$Time <- (as.numeric(dbase$Year) -2007) * 12 + as.numeric(dbase$Month) - 3
dbase <- dbase[dbase$Time >0,]
fivenum(dbase$Time)
head(dbase)
table(dbase$Product)

# order the database
dbase <- dbase[order(dbase$Super, dbase$Year, dbase$Month),]

###

## logs of prices
dbase$moda <- log(dbase$PMode)
dbase$PMode <- NULL

## Add information of Category
library(readxl)
products <- read_excel("../../Bases/lista_productos_2014_web_MEF_empresas_lz.xls")
# products <- read_excel("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/lista_productos_2014_web_MEF_empresas_lz.xls")
colnames(products)
products$in.database <- ifelse(products$`Empresas distintas` == "NC",0,1)
categ <- distinct(products[, c(1,5,17)])
colnames(categ) <- c("Product", "Category","in.db")
dbase <- merge(dbase, categ, by= "Product")
dbase <- dbase[dbase$in.db ==1,]
dbase$in.db <- NULL
colnames(dbase)
rm(categ, products)



# Calculate number of varieties -------------------------------------------


dbase$Variety <- 0
dbase$Variety <- with(dbase, ave(Product, Category, Time, Super, FUN= length))

## Check information
table(dbase$Category, dbase$Variety)



# Add information of stores -----------------------------------------------


## Load database
supers <- fread("../../Bases/Establecimiento2021.csv",data.table = F)
head(supers)
colnames(supers)
supers <- supers[,c(1,7,8,11,12)]
table(supers$ciudad)
Encoding(supers$ciudad) <- "UTF-8"
supers$ciudad <- iconv(supers$ciudad, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
table(supers$depto)
Encoding(supers$depto) <- "UTF-8"
supers$depto <- iconv(supers$depto, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
table(supers$cadena)
supers[297,3] <- "Geant"
supers[391,3] <- "Geant"
farm <- c("San Roque","Pigalle","Farmashop","FarmaGlobal")
supers$farmacia <- ifelse(supers$cadena %in% farm,1,0)
rm(farm)

colnames(supers) <- c("Super", "cashiers", "chain", "city", "depto", "is.farmacy") ## the information needed to merge
supers <- supers[order(as.numeric(supers$Super)) ,] # order by supermarket


######## Supermarkets

## Check if all supermarkets are in price frame

supers.supers <- unique(supers$Super)
supers.dbase <- unique(dbase$Super)

not.in.supers <- setdiff(supers.dbase, supers.supers) # supermarkets in price, but not in supers
not.in.prices <- setdiff(supers.supers, supers.dbase)
in.both <- intersect(supers.supers, supers.dbase)
rm(supers.dbase, supers.supers)

# Supermarket 386 is repeated, delete it
supers <- supers[supers$Super != 386,]

## Merge databases
# Exclude those supermarkets not in both bases. If want to include only supermarkets
# in price base, include all.x = TRUE

dbase <- merge(dbase, supers, by = "Super") # Merge cola and supermarket bases
rm(supers, not.in.prices, not.in.supers, in.both)
dbase <- dbase[dbase$is.farmacy ==0,]
table(dbase$is.farmacy)
dbase$is.farmacy <- NULL


## Export databases
fwrite(dbase, "../../Bases/2023_dbase.csv", row.names = FALSE)
rm(dbase)


### Continue on file: 0.Bases.Convergence_2023.R

# ---------------- End of script ---------------