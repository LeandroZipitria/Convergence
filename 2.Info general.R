## Load database

dbF <- readRDS("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.rds")
dbF <- readRDS("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.rds")

head(dbF)



#### 1) See the when products starts in the database ####

## Add information of Category
library(readxl)
products <- read_excel("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/lista_productos_2014_web_MEF_empresas_lz.xls")
categ <- distinct(products[, c(1,5)], id, Producto)
colnames(categ) <- c("Product", "Category")

# count the number of products by time! (Time = 44 es 2010/11)
a <- aggregate(dbF$Time, list(dbF$Product), min)
colnames(a) <- c("Product", "StartTime") 
a <- merge(a, categ, by= "Product")  
View(a)

write.csv(a, file="/home/lzipitria/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/productos.csv", row.names=FALSE)     ## salvo base

categories <- unique(categ$Category)



#### 2) Check information of supermarkets ####

## Load database
#db <- fread("/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/2018_dbase_with_super.csv", data.table = F)

library(zoo)

dbF$MonthYear <- zoo::as.Date(as.yearmon(with(dbF, paste(Year, Month,sep="-")), "%Y-%m"))
colnames(dbF)
MY <- unique(dbF[, c(5,25)])


# count the first observation for each supermarket
su <- aggregate(dbF$Time, list(dbF$Super), min)
colnames(su) <- c("Super", "Date")

h = hist(su$Date) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = NULL, ylim = c(0,80), xlim = c(0, 100),
     xlab = "Time", ylab = "Relative Frequency", col = "gray81")

h1 <- hist(su$StartTime)

# Time series
library(data.table)
prueba <- setDT(dbF)[, list(Super=uniqueN(Super)), by= Time] # count number of supermarket by time
prueba <- merge(prueba, MY, by = "Time")
prueba <- prueba[prueba$Time != "90",]
plot(prueba$MonthYear, prueba$Super, type = "l", ylim = c(150, 400), lwd = 2,
     xlab = "Time", ylab = "Number of Supermarkets")



#### 3) Dispersion in time ####
library(data.table)

# Load database of original supermarkets and restrict poducts 
dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)
dbFM <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginMdeo.csv", data.table = F)

#dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)

dbF <- na.omit(dbF)
dbF <- dbF[dbF$Time <90,]
prodorig <- unique(dbF[dbF$Year == 2007,]$Product)
dbF <- dbF[dbF$Product %in% prodorig, ]

dbFM <- na.omit(dbFM)
dbFM <- dbFM[dbFM$Time <90,]
prodorig <- unique(dbFM[dbFM$Year == 2007,]$Product)
dbFM <- dbFM[dbFM$Product %in% prodorig, ]

# dbF$filteredBase <- residuals(lm(SD.Base ~ as.factor(Product), data = dbF))
# dbF$filteredVarComp <- residuals(lm(SD.RCompVar ~ as.factor(Product), data = dbF))


# All series
tbd <- aggregate(dbF$SD.Base, by = list(dbF$Time), median)
lm_tbd <- lm(tbd$x ~ tbd$Group.1)

tbd2 <- aggregate(dbFM$SD.Base, by = list(dbFM$Time), median)
lm_tbd2 <- lm(tbd2$x ~ tbd2$Group.1)

#
plot(tbd,main = NULL, xlim = c(0, 100), ylim=c(4,8),
     xlab = "Time", ylab = "Median SD (in %)", col = "blue", cex.lab=1.2)
abline(coef(lm_tbd), lwd = 2, col = "blue")
points(tbd2, col = "gray21")
abline(coef(lm_tbd2), lwd = 2, col = "gray21")
color = c("blue", "gray21")
legend(60,8, c("Country", "Montevideo City"), bty = "n", #draw no box
       xpd = TRUE, # draw legend outside box
       cex = 1.2, pt.cex = 1.5,  y.intersp = 0.8, fill = color)


# Filtered series by product
tbd <- aggregate(dbF$filteredBase/100, by = list(dbF$Time), median)
lm_tbd <- lm(tbd$x ~ tbd$Group.1)

tbd2 <- aggregate(dbF$filteredVarComp/100, by = list(dbF$Time), median)
lm_tbd2 <- lm(tbd2$x ~ tbd2$Group.1)

#
plot(tbd,main = NULL, xlim = c(0, 100), ylim= c(-0.008, 0.008),
     xlab = "Time", ylab = "Median SD", col = "blue")
abline(coef(lm_tbd), lwd = 2, col = "blue")
points(tbd2, col = "gray21")
abline(coef(lm_tbd2), lwd = 2, col = "gray21")
color = c("blue", "gray21")
legend("topleft", c("Original", "Filtered by Competition and Variety"), bty = "n", #draw no box
       xpd = TRUE, # draw legend outside box
       cex = 1, pt.cex = 1.5,  y.intersp = 0.6, fill = color)


## (5) Variety and competition ####

dbF <- get(load("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))
dbF <- get(load("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))

table(dbF$variety)
aa <- table(dbF$variety, dbF$competition) #  el primero es fila, segundo columna
prop.table(aa)*100

