files <- list.files(path="E://BASES DE DATOS/DGC bases/Bases Limpias/Precios diarios/", pattern="*.csv", full.names=TRUE, recursive=FALSE)
dbase <- df[0,]

for (i in files) {
  df <- data.table::fread(i, data.table = F)
  dbase <- rbind(dbase, df)
}

# dbase <- data.table::fread("d:/Dropbox/BASES DE DATOS/DGC bases/Bases Limpias/Precios diarios/2022_D_Clean.csv", data.table = F)

## Add information of Category
library(readxl)
library(dplyr)
products <- read_excel("../../Bases/lista_productos_2014_web_MEF_empresas_lz.xls")
# products <- read_excel("c://Users/leandro/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/lista_productos_2014_web_MEF_empresas_lz.xls")
colnames(products)
products$in.database <- ifelse(products$`Empresas distintas` == "NC",0,1)
categ <- distinct(products[, c(1,5,17)])
colnames(categ) <- c("Product", "Category","in.db")
prod = categ[categ$in.db ==1,]$Product
dbase2 <- data.table::as.data.table(dbase)[Product %in% prod]
dbase <- merge(dbase, categ, by= "Product")
dbase <- dbase[dbase$in.db ==1,]
dbase$in.db <- NULL
colnames(dbase)
rm(categ, products)