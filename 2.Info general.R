## Load database

dbF <- get(load("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResiduals.Rdata"))
#dbF <- get(load("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))

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
db <- fread("/home/lzipitria/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/2018.New/2018_dbase_with_super.csv", data.table = F)

# count the first observation for each supermarket
su <- aggregate(db$Time, list(db$Super), min)
colnames(su) <- c("Super", "StartTime")

h = hist(su$StartTime) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = NULL, ylim = c(0,80), xlim = c(0, 100),
     xlab = "Time", ylab = "Relative Frequency", col = "gray81")



#### 3) Dispersion in time ####

dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)
dbF <- na.omit(dbF)

dbF <- dbF[dbF$Time <90,]

dbF$filteredBase <- residuals(lm(SD.Base ~ as.factor(Product), data = dbF))
dbF$filteredVarComp <- residuals(lm(SD.RCompVar ~ as.factor(Product), data = dbF))


# All series
tbd <- aggregate(dbF$SD.Base/100, by = list(dbF$Time), median)
lm_tbd <- lm(tbd$x ~ tbd$Group.1)

tbd2 <- aggregate(dbF$SD.RCompVar/100, by = list(dbF$Time), median)
lm_tbd2 <- lm(tbd2$x ~ tbd2$Group.1)

#
plot(tbd,main = NULL, xlim = c(0, 100), ylim= c(0.05, 0.1),
     xlab = "Time", ylab = "Median SD", col = "blue")
abline(coef(lm_tbd), lwd = 2, col = "blue")
points(tbd2, col = "gray21")
abline(coef(lm_tbd2), lwd = 2, col = "gray21")
color = c("blue", "gray21")
legend("topleft", c("Original", "Filtered by Competition and Variety"), bty = "n", #draw no box
       xpd = TRUE, # draw legend outside box
       cex = 1, pt.cex = 1.5,  y.intersp = 0.6, fill = color)


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


## 4) Dispersion in time (original data) #####

dbF <- fread("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedAll.csv", data.table = F)
dbF <- fread("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018.RestrictedOriginal.csv", data.table = F)

dbF <- na.omit(dbF)

dbF <- dbF[dbF$Time <90,]

dbF$filteredBase <- residuals(lm(SD.Base ~ as.factor(Product), data = dbF))
dbF$filteredVarComp <- residuals(lm(SD.RCompVar ~ as.factor(Product), data = dbF))


# All series
tbd <- aggregate(dbF$SD.Base/100, by = list(dbF$Time), median)
lm_tbd <- lm(tbd$x ~ tbd$Group.1)

tbd2 <- aggregate(dbF$SD.RCompVar/100, by = list(dbF$Time), median)
lm_tbd2 <- lm(tbd2$x ~ tbd2$Group.1)

#
plot(tbd,main = NULL, xlim = c(0, 100), ylim= c(0.05, 0.1),
     xlab = "Time", ylab = "Median SD", col = "blue")
abline(coef(lm_tbd), lwd = 2, col = "blue")
points(tbd2, col = "gray21")
abline(coef(lm_tbd2), lwd = 2, col = "gray21")
color = c("blue", "gray21")
legend("topleft", c("Original", "Filtered by Competition and Variety"), bty = "n", #draw no box
       xpd = TRUE, # draw legend outside box
       cex = 1, pt.cex = 1.5,  y.intersp = 0.6, fill = color)


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


## (5) VAriety and competition ####

dbF <- get(load("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))
dbF <- get(load("C:/Users/leandro/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))

table(dbF$variety)
aa <- table(dbF$variety, dbF$competition) #  el primero es fila, segundo columna
prop.table(aa)*100

