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


##### Plot time effects

db$ProdTime <- as.factor(interaction(db$Product, db$Time))

## Coeff variation
rgCV <-  feols(CV.Base ~ as.factor(Time) | Product, data = db)
summary(rgCV)
names(rgCV$coefficients) <- gsub("as.factor\\(Time\\)","Time ",names(rgCV$coefficients))
jtools::plot_coefs(rgCV, cluster = c("ProdTime"))

## Standard Deviation
rgSD <-  feols(SD.Base ~ as.factor(Time) | Product, data = db)
summary(rgSD)
names(rgSD$coefficients) <- gsub("as.factor\\(Time\\)","Time ",names(rgSD$coefficients))
jtools::plot_coefs(rgSD, cluster = c("ProdTime"))
# Note: standard deviation first decrease, then increase!

## Median Price
rgMean <-  feols(Mean.Base ~ as.factor(Time) | Product, data = db)
summary(rgMean)
names(rgMean$coefficients) <- gsub("as.factor\\(Time\\)","Time ",names(rgMean$coefficients))
jtools::plot_coefs(rgMean, cluster = c("ProdTime"))
# Average price consistently increase

