################################################
### SCRIPT TO CREATE PRICE DIFFERENCES #########
################################################



# Load price database -----------------------------------------------------


library("data.table") 
library("dplyr")
library(stringi)

## load price database
dbase <- fread("../../Bases/raw/Base2022MonthlyClean.csv", data.table = F)
dbase$Time <- (as.numeric(dbase$Year) -2007) * 12 + as.numeric(dbase$Month) - 3
dbase <- dbase[dbase$Time >0,]
fivenum(dbase$Time)
head(dbase)
table(dbase$Product)

# order the database
dbase <- dbase[order(dbase$Super, dbase$Year, dbase$Month),]

# Add time square
dbase$Time2 <- dbase$Time * dbase$Time



# Add information to price database ---------------------------------------


## * Of category ---------------------------------------------

library(readxl)
products <- read_excel("../../Bases/raw/lista_productos_2014_web_MEF_empresas_lz.xls")
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



## * Number of varieties and share of products ------------------------------------


dbase$Variety <- 0
dbase$Variety <- with(dbase, ave(Product, Category, Time, Super, FUN= length))

## Check information
table(dbase$Category, dbase$Variety)


## Share of products at the store
dbase$shProd <- 0
dbase <- as.data.table(dbase)[, totnumprod := length(unique(Product)), by=.(Time)]
dbase <- as.data.table(dbase)[, shProd := length(unique(Product)/totnumprod), by=.(Super, Time)]
dbase$totnumprod <- NULL



## * Stores -----------------------------------------------


## Load database
supers <- fread("../../Bases/raw/Establecimiento2023.csv",data.table = F, encoding = "Latin-1")
colnames(supers)


## Clean Spanish names
supers <- as.data.table(supers)[, "ciudad" := tolower(stri_trans_general(str = ciudad, 
                                   id = "Latin-ASCII"))]
supers <- as.data.table(supers)[, "barrio" := tolower(stri_trans_general(str = barrio, 
                                                                 id = "Latin-ASCII"))]
supers <- as.data.table(supers)[, "depto" := tolower(stri_trans_general(str = depto, 
                                                                 id = "Latin-ASCII"))]
supers <- as.data.table(supers)[, "cadena" := tolower(stri_trans_general(str = cadena, 
                                                                 id = "Latin-ASCII"))]
head(supers)
colnames(supers)

## Check cities
table(supers$ciudad)
sum(is.na(supers$ciudad))

## Check neighborhoods
table(supers$barrio)
sum(is.na(supers[supers$ciudad == "montevideo",]$barrio))

## Fix "Barrios"
# Montevideo
supers[supers$ciudad == "montevideo" & supers$barrio == "montevideo",]
# Parque batlle
supers$barrio <- gsub("parque batlle y villa dolores","parque batlle",supers$barrio)
# Colon
supers$barrio <- gsub("colon centro y noroeste","colon",supers$barrio)
# Jardines del hipódromo
supers$barrio <- gsub("jardines del hipodromo","maronas",supers$barrio)


## Select columns
supers <- supers[,c(1,6,7,8,11,12)] #id,barrio,cajas,cadena,ciudad,depto


## Erase farmacies
farm <- c("san roque","pigalle","farmashop","farmaglobal")
supers$farmacia <- ifelse(supers$cadena %in% farm,1,0)
table(supers$farmacia)
rm(farm)

## Rename columns
colnames(supers) <- c("Super","neighbhd", "cashiers", "chain", "city", "depto","is.farmacy") ## the information needed to merge
supers <- supers[order(as.numeric(supers$Super)) ,] # order by supermarket

## Create city Montevideo with neighborhood
supers$city2 <- ifelse(supers$depto == "montevideo",paste(supers$city,supers$neighbhd,sep = "_"),supers$city)


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

dbase <- setDT(dbase)[setDT(supers), on=c("Super"),]

rm(supers, not.in.prices, not.in.supers, in.both)
dbase <- dbase[dbase$is.farmacy ==0,]
table(dbase$is.farmacy)
dbase$is.farmacy <- NULL

# chech cities
table(dbase$city2)
head(dbase)



# * Income, employment and unemployment ---------------------------------------


## Check for same names
#neigh <- read.csv("../../Bases/marketsFer_sinrepetidos.csv")
#table(dbase$neighbhd)


## Previously: fix neighborhoods as ECH
# Capurro and Bella Vista
dbase$neighbhd <- gsub("bella vista","capurro",dbase$neighbhd)
# Carrasco norte and Portones
dbase$neighbhd <- gsub("portones","carrasco norte",dbase$neighbhd)
# Manga and Toledo chico
dbase$neighbhd <- gsub("toledo chico","manga",dbase$neighbhd)
# Paso molino and belvedere
dbase$neighbhd <- gsub("belvedere","paso molino",dbase$neighbhd)
# Peñarol and Lavalleja
dbase$neighbhd <- gsub("lavalleja","penarol",dbase$neighbhd)
# Punta Carretas and Trouville
dbase$neighbhd <- gsub("trouville","punta carretas",dbase$neighbhd)
sort(unique(dbase$neighbhd))

# Fix again
dbase$city2 <- ifelse(dbase$depto == "montevideo",paste(dbase$city,dbase$neighbhd,sep = "_"),dbase$city)
table(dbase$city2)


### Load ECH database (income, employment and unemployment)

inc <- fread("../../Bases/raw/ECH_2007a2021M6.csv", data.table = F)
head(inc)

inc$Time <- as.integer(inc$anio -2007) * 12 + inc$mes - 3
fivenum(inc$Time)

# unique(inc$nomdpto)
# inc$nomdpto <- tolower(inc$nomdpto)
table(inc$nomdpto)
#inc$NOMDPTO <- ifelse(inc$NOMDPTO == "","montevideo",inc$NOMDPTO)
table(dbase$neighbhd)
setdiff(unique(inc$Barrio_LZ),unique(dbase$neighbhd))

# Change names
inc$Barrio_LZ <- gsub("capurro bellavista","capurro",inc$Barrio_LZ)
inc$Barrio_LZ <- gsub("colon centro y noroeste","colon",inc$Barrio_LZ)
inc$Barrio_LZ <- gsub("belvedere paso molino","paso molino",inc$Barrio_LZ)
inc$Barrio_LZ <- gsub("manga toledo chico","manga",inc$Barrio_LZ)
inc$Barrio_LZ <- gsub("penarol lavalleja","penarol",inc$Barrio_LZ)
inc$Barrio_LZ <- gsub("parque batlle y villa dolores","parque batlle",inc$Barrio_LZ)
inc$Barrio_LZ <- ifelse(inc$nomdpto != "montevideo","interior",inc$Barrio_LZ)

# Change column names
colnames(inc)
colnames(inc) <- c("depto","neighbhd","num.ocup","num.unem","income","sd_income","Year","Month","Time")
inc <- inc[,c(1:6,9)]
inc <- as.data.frame(inc)

# Time frame
inc <- inc[inc$Time>0,]



# Merge databases
dbase <- setDT(dbase)[setDT(inc), on=c("Time","depto","neighbhd"), ":=" (num.ocup= i.num.ocup,
                                                                         num.unem= i.num.unem,
                                                                         income= i.income,
                                                                         sd_income=i.sd_income) ]
rm(inc)


# * CPI and prices --------------------------------------------------------


## logs of prices
sum(is.na(dbase$PMode))
dbase <- dbase[!is.na(dbase$PMode),]
# dbase$lPmode <- log(dbase$PMode)
sum(is.na(dbase$Pmode))


# If CPI is needed
ipc <- fread("../../Bases/raw/IPC.csv", data.table = F)
ipc$IPC <- NULL
ipc$index <- 100
for(i in 2:189) {
  ipc$index[i] <- (ipc$index[i-1] * (1+ (ipc$Monthly.change[i]/100)))
}
ipc$Time <- c(1:189)
ipc$`Mes y año` <- ipc$Monthly.change <- NULL
ipc$index <- ipc$index /100
dbase <- setDT(dbase)[setDT(ipc), on=c("Time" = "Time"), cpi:= i.index]
# dbM$PmodaOrig <- round(exp(dbM$moda /100), digits = 2)
# Deflacted prices
dbase$lPMode_r <- log(dbase$PMode/(dbase$cpi))
rm(ipc)




# Export database ---------------------------------------------------------


## Export databases
fwrite(dbase, "../../Bases/processed/2023_dbase.csv", row.names = FALSE)
rm(dbase)


### Next file: 1.CreateSDbase_2023_SD_v2.R

# ---------------- End of script ---------------