##########################################################
######## Description: regressions for SD database ########


## Load libraries used
library(fixest)
library(data.table)



# --------------------- Base regressions ---------------------


# Load database
db <- fread("../../Bases/2023.RestrictedAll.csv", data.table = F)
# Previous operations  
db$Product <- as.factor(db$Product)
prodorig <- unique(db[db$Year == 2007,]$Product)

dbsup <- fread("../../Bases/2023.RestrictedAll.chains.csv", data.table = F)


## Standard Deviation
# rgSD <-  feols(SD.PReal ~ as.factor(Time) | Product + city2, data = db)
# summary(rgSD)
# names(rgSD$coefficients) <- gsub("as.factor\\(Time\\)","Time ",names(rgSD$coefficients))
# jtools::plot_coefs(rgSD, cluster = c("ProdTime"))




# Time fixed effect regression --------------------------------------------


### 1) Aggregate

### Base regression
reg1 = feols((SD.PReal*100) ~ PAve + Time, cluster = "Product^Time", data = db)
summary(reg1)

reg2 = feols((SD.PReal*100) ~ PAve + Time | Product, cluster = "Product^Time", data = db)
summary(reg2)

reg3 = feols((SD.PReal*100) ~ PAve + Time | city2, cluster = "Product^Time", data = db)
summary(reg3)

reg4 = feols((SD.PReal*100) ~ PAve + Time | Product + city2, cluster = "Product^Time", data = db)
summary(reg4)

reg5 = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2, cluster = "Product^Time", data = db)
summary(reg5)


setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market"))


etable(reg1,reg2,reg3,reg4,reg5,
       digits = 4)

etable(reg1,reg2,reg3,reg4,reg5,
       title = "Convergence Baseline Estimation.",
       label = "base-estimation",
       placement = "ht",
       digits = 4,
       tex = T)



### Explanations

# Linear
reg1cat = feols((SD.PReal*100) ~ PAve + Time + CatDisp| Product + city2, cluster = "Product^Time", data = db)
summary(reg1cat)

reg2comp = feols((SD.PReal*100) ~ PAve + Time + num_strs_comp| Product + city2, cluster = "Product^Time", data = db)
summary(reg2comp)

reg3catcomp = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp| Product + city2, cluster = "Product^Time", data = db)
summary(reg3catcomp)

# Interactions
reg1catint = feols((SD.PReal*100) ~ PAve + Time * CatDisp| Product + city2, cluster = "Product^Time", data = db)
summary(reg1catint)

reg2compint = feols((SD.PReal*100) ~ PAve + Time * num_strs_comp| Product + city2, cluster = "Product^Time", data = db)
summary(reg2compint)

reg3catcompint = feols((SD.PReal*100) ~ PAve + Time * (CatDisp + num_strs_comp)| Product + city2, cluster = "Product^Time", data = db)
summary(reg3catcompint)


setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "CatDisp" = "Cat. Entropy",
                 "num_strs_comp" = "Number Comp. Stores",
                 "Time:CatDisp" = "Time x Cat. Entropy",
                 "Time:num_strs_comp" = "Time x Number Comp. Stores"))


etable(reg1cat,reg2comp,reg3catcomp,reg1catint,reg2compint,reg3catcompint,
       digits = 4)

etable(reg1cat,reg2comp,reg3catcomp,reg1catint,reg2compint,reg3catcompint,
       title = "Source of Relative Convergence.",
       label = "source-estimation",
#       fontsize = "small",
       placement = "!htbp",
       digits = 4,
       tex = T)




### 2) By supermaket



### Base regression
reg1 = feols((SD.PReal*100) ~ PAve + Time, fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)
summary(reg1)

reg2 = feols((SD.PReal*100) ~ PAve + Time | chain, fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)
summary(reg2)

reg3 = feols((SD.PReal*100) ~ PAve + Time | Product + city2 + chain, fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)
summary(reg3)



setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "chain" = "Chain",
                 "is.chain" = "Stores"))


etable(reg1,reg2,reg3,
       digits = 4)


etable(reg2,reg3,
       title = "Convergence Estimation by Chains.",
       label = "chain-base-estimation",
       fontsize = "small",
       placement = "!h",
       digits = 4,
       tex = T)



### Explanations

# Linear
reg1chcat = feols((SD.PReal*100) ~ PAve + Time + CatDisp| Product + city2 + chain,
                  fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

reg2chcomp = feols((SD.PReal*100) ~ PAve + Time + num_strs_comp + num_stores_chain | Product + city2 + chain,
                   fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

reg3chcatcomp = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp + num_stores_chain | Product + city2 + chain,
                      fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

# Interactions
reg1chcatint = feols((SD.PReal*100) ~ PAve + Time * CatDisp| Product + city2 + chain,
                     fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

reg2chcompint = feols((SD.PReal*100) ~ PAve + Time * (num_strs_comp + num_stores_chain)| Product + city2 + chain,
                      fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

reg3chcatcompint = feols((SD.PReal*100) ~ PAve + Time * (CatDisp + num_strs_comp + num_stores_chain)| Product + city2 + chain,
                         fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

# All interactions (not run in the paper)
reg4chcatcompint = feols((SD.PReal*100) ~ PAve + Time * CatDisp * num_strs_comp * num_stores_chain| Product + city2 + chain,
                         fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

etable(reg4chcatcompint,
       digits = 4)

setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "CatDisp" = "Cat. Entropy",
                 "num_strs_comp" = "Number Comp. Stores",
                 "num_stores_chain" = "Same Chain Stores",
                 "Time:CatDisp" = "Time x Cat. Entropy",
                 "Time:num_strs_comp" = "Time x Number Comp. Stores"))


etable(reg1chcat,reg2chcomp,reg3chcatcomp,
       digits = 4)

etable(reg1chcatint,reg2chcompint,reg3chcatcompint,
       digits = 4)

etable(reg1chcat,reg2chcomp,reg3chcatcomp,
       title = "Source of Relative Convergence in Chains (Linear Trend).",
       label = "chain-source-linear",
       fontsize = "footnotesize",
       placement = "!htbp",
       digits = 4,
       tex = T)

etable(reg1chcatint,reg2chcompint,reg3chcatcompint,
       title = "Source of Relative Convergence in Chains (Interaction Trend).",
       label = "chain-source-interact",
       fontsize = "scriptsize",
       placement = "!htbp",
       digits = 4,
       tex = T)



# --------------------- Original stores regressions ---------------------


# Load database
db <- fread("../../Bases/2023.RestrictedOriginal.csv", data.table = F)
# Previous operations  
db$Product <- as.factor(db$Product)
prodorig <- unique(db[db$Year == 2007,]$Product)

dbsup <- fread("../../Bases/2023.RestrictedOriginal.chains.csv", data.table = F)


## Standard Deviation
# rgSD <-  feols(SD.PReal ~ as.factor(Time) | Product + city2, data = db)
# summary(rgSD)
# names(rgSD$coefficients) <- gsub("as.factor\\(Time\\)","Time ",names(rgSD$coefficients))
# jtools::plot_coefs(rgSD, cluster = c("ProdTime"))




# Time fixed effect regression --------------------------------------------


### 1) Aggregate

### Base regression
reg1 = feols((SD.PReal*100) ~ PAve + Time, cluster = "Product^Time", data = db)
summary(reg1)

reg2 = feols((SD.PReal*100) ~ PAve + Time | Product, cluster = "Product^Time", data = db)
summary(reg2)

reg3 = feols((SD.PReal*100) ~ PAve + Time | city2, cluster = "Product^Time", data = db)
summary(reg3)

reg4 = feols((SD.PReal*100) ~ PAve + Time | Product + city2, cluster = "Product^Time", data = db)
summary(reg4)

reg5 = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2, cluster = "Product^Time", data = db)
summary(reg5)


setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market"))


etable(reg1,reg2,reg3,reg4,reg5,
       digits = 4)

etable(reg1,reg2,reg3,reg4,reg5,
       title = "Convergence Baseline Estimation.",
       label = "base-estimation",
       placement = "ht",
       digits = 4,
       tex = T)



### Explanations

# Linear
reg1cat = feols((SD.PReal*100) ~ PAve + Time + CatDisp| Product + city2, cluster = "Product^Time", data = db)
summary(reg1cat)

reg2comp = feols((SD.PReal*100) ~ PAve + Time + num_strs_comp| Product + city2, cluster = "Product^Time", data = db)
summary(reg2comp)

reg3catcomp = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp| Product + city2, cluster = "Product^Time", data = db)
summary(reg3catcomp)

# Interactions
reg1catint = feols((SD.PReal*100) ~ PAve + Time * CatDisp| Product + city2, cluster = "Product^Time", data = db)
summary(reg1catint)

reg2compint = feols((SD.PReal*100) ~ PAve + Time * num_strs_comp| Product + city2, cluster = "Product^Time", data = db)
summary(reg2compint)

reg3catcompint = feols((SD.PReal*100) ~ PAve + Time * (CatDisp + num_strs_comp)| Product + city2, cluster = "Product^Time", data = db)
summary(reg3catcompint)


setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "CatDisp" = "Cat. Entropy",
                 "num_strs_comp" = "Number Comp. Stores",
                 "Time:CatDisp" = "Time x Cat. Entropy",
                 "Time:num_strs_comp" = "Time x Number Comp. Stores"))


etable(reg1cat,reg2comp,reg3catcomp,reg1catint,reg2compint,reg3catcompint,
       digits = 4)

etable(reg1cat,reg2comp,reg3catcomp,reg1catint,reg2compint,reg3catcompint,
       title = "Source of Relative Convergence.",
       label = "source-estimation",
       #       fontsize = "small",
       placement = "!htbp",
       digits = 4,
       tex = T)




### 2) By supermaket



### Base regression
reg1 = feols((SD.PReal*100) ~ PAve + Time, fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)
summary(reg1)

reg2 = feols((SD.PReal*100) ~ PAve + Time | chain, fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)
summary(reg2)

reg3 = feols((SD.PReal*100) ~ PAve + Time | Product + city2 + chain, fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)
summary(reg3)



setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "chain" = "Chain",
                 "is.chain" = "Stores"))


etable(reg1,reg2,reg3,
       digits = 4)


etable(reg2,reg3,
       title = "Convergence Estimation by Chains.",
       label = "chain-base-estimation",
       fontsize = "small",
       placement = "!h",
       digits = 4,
       tex = T)



### Explanations

# Linear
reg1chcat = feols((SD.PReal*100) ~ PAve + Time + CatDisp| Product + city2 + chain,
                  fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

reg2chcomp = feols((SD.PReal*100) ~ PAve + Time + num_strs_comp + num_stores_chain | Product + city2 + chain,
                   fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

reg3chcatcomp = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp + num_stores_chain | Product + city2 + chain,
                      fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

# Interactions
reg1chcatint = feols((SD.PReal*100) ~ PAve + Time * CatDisp| Product + city2 + chain,
                     fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

reg2chcompint = feols((SD.PReal*100) ~ PAve + Time * (num_strs_comp + num_stores_chain)| Product + city2 + chain,
                      fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

reg3chcatcompint = feols((SD.PReal*100) ~ PAve + Time * (CatDisp + num_strs_comp + num_stores_chain)| Product + city2 + chain,
                         fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

# All interactions (not run in the paper)
reg4chcatcompint = feols((SD.PReal*100) ~ PAve + Time * CatDisp * num_strs_comp * num_stores_chain| Product + city2 + chain,
                         fsplit = ~is.chain, weights = ~num_stores_total, cluster = "chain^Time", data = dbsup)

etable(reg4chcatcompint,
       digits = 4)

setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "CatDisp" = "Cat. Entropy",
                 "num_strs_comp" = "Number Comp. Stores",
                 "num_stores_chain" = "Same Chain Stores",
                 "Time:CatDisp" = "Time x Cat. Entropy",
                 "Time:num_strs_comp" = "Time x Number Comp. Stores"))


etable(reg1chcat,reg2chcomp,reg3chcatcomp,
       digits = 4)

etable(reg1chcatint,reg2chcompint,reg3chcatcompint,
       digits = 4)

etable(reg1chcat,reg2chcomp,reg3chcatcomp,
       title = "Source of Relative Convergence in Chains (Linear Trend).",
       label = "chain-source-linear",
       fontsize = "footnotesize",
       placement = "!htbp",
       digits = 4,
       tex = T)

etable(reg1chcatint,reg2chcompint,reg3chcatcompint,
       title = "Source of Relative Convergence in Chains (Interaction Trend).",
       label = "chain-source-interact",
       fontsize = "scriptsize",
       placement = "!htbp",
       digits = 4,
       tex = T)


## End of script ------------