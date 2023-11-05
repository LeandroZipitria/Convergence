##########################################################
######## Description: regressions for SD database ########


## Load libraries used
library(fixest)
library(data.table)
library(ggplot2)



# - Aggregate regressions ------------------------------------


# Load database aggregate data
db <- fread("../../Bases/processed/2023.Aggregate.csv", data.table = F)
db$pop <- db$num.ocup + db$num.unem
db$ur <- db$num.unem / db$pop



# * - Dispersion ----------------------------------------------------------


reg1 = feols((SD.PReal*100) ~ PAve + Time, weights = ~num_strs_comp+1, cluster = "Product^Time", data = db)
summary(reg1)

reg2 = feols((SD.PReal*100) ~ PAve + Time | Product + Month, weights = ~num_strs_comp+1, cluster = "Product^Time", data = db)
summary(reg2)

reg3 = feols((SD.PReal*100) ~ PAve + Time | city2 + Month, weights = ~num_strs_comp+1,cluster = "Product^Time", data = db)
summary(reg3)

reg4 = feols((SD.PReal*100) ~ PAve + Time | Product + city2 + Month, weights = ~num_strs_comp+1,  cluster = "Product^Time", data = db)
summary(reg4)

reg5 = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 + Month, weights = ~num_strs_comp+1, cluster = "Product^Time", data = db)
summary(reg5)


setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market"))


etable(reg1,reg2,reg3,reg4,reg5,
       digits = 4)

# For paper
etable(reg1,reg2,reg3,reg4,reg5,
       title = "Convergence Baseline Estimation.",
       label = "tab:congergence",
       placement = "ht",
       fontsize = "small",
       digits = 4,
       tex = T)
# write(paper1,file = "../../Lyx/regressions/reg1.tex")

# Presentation
pres1 <- etable(reg1,reg2,reg3,reg4,reg5,
       placement = "ht",
       fontsize = "tiny",
       digits = 4,
       coef.style = .("\\color{red} :coef_se:" = "Time"),
       tex = T)
#write(pres1,file = "../../Presentaciones/texoutput/pres1.tex")
rm(reg1,reg2,reg3,reg4,reg5)


# Above and below median time

db$time_median <- db$Time - median(db$Time)
db$time2_median <- db$time_median * db$time_median

#db$UntilMT <- ifelse(db$Time <median(db$Time),0,1)

reg4b = feols((SD.PReal*100) ~ PAve + Time | Product + city2 + Month, weights = ~num_strs_comp+1,
              cluster = "Product^Time", data = db[db$Time <= median(db$Time),])
summary(reg4b)

reg4a = feols((SD.PReal*100) ~ PAve + time_median | Product + city2 + Month, weights = ~num_strs_comp+1,
              cluster = "Product^Time", data = db[db$Time > median(db$Time),])
summary(reg4a)

reg5b = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 + Month, weights = ~num_strs_comp+1,
              cluster = "Product^Time", data = db[db$Time <= median(db$Time),])
summary(reg5b)

reg5a = feols((SD.PReal*100) ~ PAve + time_median + time2_median| Product + city2 + Month, weights = ~num_strs_comp+1,
              cluster = "Product^Time", data = db[db$Time > median(db$Time),])
summary(reg5a)

setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "time_median" = "Time",
                 "time2_median"= "Time2"))


etable(reg4b,reg5b,reg4a,reg5a,
       digits = 4)


# For paper
# Note: sustituir (1) & (2) 
# por \multicolumn{2}{c}{Until Median Time} 
# (3)                         & (4)
# \multicolumn{2}{c}{After Median Time} 

etable(reg4b,reg5b,reg4a,reg5a,
       title = "Convergence Baseline Estimation. Until and After Median Time",
       label = "tab:convergence-split-sample",
       placement = "ht",
#       group=list("Until Median Time"=c("1", "2")),
       digits = 4,
       tex = T)

# Presentation
etable(reg4b,reg5b,reg4a,reg5a,
       placement = "ht",
       fontsize = "tiny",
       digits = 4,
       group=list("Until Median Time"=c("1", "2")),
       coef.style = .("\\color{red} :coef_se:" = "Time"),
       tex = T)

rm(reg4b,reg5b,reg4a,reg5a)


# * - Plot coefficients ---------------------------------------------------


dbplot <- as.data.frame(matrix(0,nrow = 190,ncol = 2))
colnames(dbplot) <- c("time","time2")
dbplot$time <- c(1:190)
dbplot$time2 <- dbplot$time*dbplot$time

# Information for figures in the paper

# Columns (4) and (5) of Table 2
f1 <- function(x) 0.0172*x
f2 <- function(x) +.0027*x + 7.08*10^(-5)*x^2 


ggplot() + 
  xlim(0,190) + 
  ylim(0,4) +
  labs(x = "Time", y = "Dispersion (in %)") +
  geom_function(fun = f1, aes(color = "Linear Trend")) + 
  geom_function(fun = f2, aes(color = "Quadratic Trend")) +
  geom_hline(yintercept=0) + # add horizontal axes
  geom_vline(xintercept=0) + # add vertical axes
  labs(color="Trends") +# change name of box 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# Columns (1) and (2), and (3) and (4) of Table 3
f1 <- function(x) {
  (x <= 89) * (0.0095*x) + (x > 89) * (0.0165*(x-89))
}

f2 <- function(x) {
  (x <= 89) * (-.0063*x + 0.0001*x^2) + (x > 89) * (-.0493*(x-89) + 0.0008*(x-89)^2)
}

ggplot() + 
  xlim(0,200) + 
  ylim(-2,5) +
  labs(x = "Time", y = "Dispersion (in %)") +
  geom_function(fun = f1, aes(color = "Linear Trend")) + 
  geom_function(fun = f2, aes(color = "Quadratic Trend")) +
  geom_hline(yintercept=0) + # add horizontal axes
  geom_vline(xintercept=0) + # add vertical axes
  labs(color="Trends") +# change name of box 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))







# * - Dispersion: original stores -----------------------------------------


# Load database aggregate data
dbo <- fread("../../Bases/processed/2023.Aggregate.Original.csv", data.table = F)
dbo$pop <- dbo$num.ocup + dbo$num.unem
dbo$ur <- dbo$num.unem / dbo$pop



reg4o = feols((SD.PReal*100) ~ PAve + Time | Product + city2 + Month, 
              weights = ~num_strs_comp+1,  cluster = "Product^Time", data = dbo)
summary(reg4o)

reg5o = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 + Month, 
              weights = ~num_strs_comp+1, cluster = "Product^Time", data = dbo)
summary(reg5o)


# setFixest_dict(c("PAve" = "Av. Price",
#                  "SD.PReal*100" = "SD (in %)",
#                  "city2"="Market"))


etable(reg4o,reg5o,
       digits = 4)


# Above and below median time


dbo$time_median <- dbo$Time - median(dbo$Time)
dbo$time2_median <- dbo$time_median * dbo$time_median


reg4bo = feols((SD.PReal*100) ~ PAve + Time | Product + city2 + Month, weights = ~num_strs_comp+1,
              cluster = "Product^Time", data = dbo[dbo$Time <= median(dbo$Time),])
summary(reg4bo)

reg4ao = feols((SD.PReal*100) ~ PAve + time_median | Product + city2 + Month, weights = ~num_strs_comp+1,
              cluster = "Product^Time", data = dbo[dbo$Time > median(dbo$Time),])
summary(reg4ao)

reg5bo = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 + Month, weights = ~num_strs_comp+1,
              cluster = "Product^Time", data = dbo[dbo$Time <= median(dbo$Time),])
summary(reg5bo)

reg5ao = feols((SD.PReal*100) ~ PAve + time_median + time2_median| Product + city2 + Month, weights = ~num_strs_comp+1,
              cluster = "Product^Time", data = dbo[dbo$Time > median(dbo$Time),])
summary(reg5ao)


setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market"))


etable(reg4bo,reg5bo,reg4ao,reg5ao,
       digits = 4)


# For paper
# Note: sustituir (3) & (4) 
# por \multicolumn{2}{c}{Until June, 2015} 
# (5)                         & (5)
# \multicolumn{2}{c}{After June, 2015} 

etable(reg4o,reg5o,reg4bo,reg5bo,reg4ao,reg5ao,
       title = "Convergence Baseline Estimation. Stores in the year 2007.",
       label = "tab:original-stores",
       placement = "h!",
       fontsize = "small",
       #       group=list("Until Median Time"=c("1", "2")),
       digits = 4,
       tex = T)

# Presentation
etable(reg4b,reg5b,reg4a,reg5a,
       placement = "ht",
       fontsize = "tiny",
       digits = 4,
       group=list("Until Median Time"=c("1", "2")),
       coef.style = .("\\color{red} :coef_se:" = "Time"),
       tex = T)

rm(reg4o,reg5o,reg4bo,reg5bo,reg4ao,reg5ao)



# * - Plot coefficients ---------------------------------------------------


dbplot <- as.data.frame(matrix(0,nrow = 190,ncol = 2))
colnames(dbplot) <- c("time","time2")
dbplot$time <- c(1:190)
dbplot$time2 <- dbplot$time*dbplot$time



#### Information for figures in the paper


## Columns (1) and (2) of Table 2
f1 <- function(x) 0.012*x
f2 <- function(x) -.0101*x + .0001*x^2 


ggplot() + 
  xlim(0,190) + 
  ylim(-1,3) +
  labs(x = "Time", y = "Dispersion (in %)") +
  geom_function(fun = f1, aes(color = "Linear Trend")) + 
  geom_function(fun = f2, aes(color = "Quadratic Trend")) +
  geom_hline(yintercept=0) + # add horizontal axes
  geom_vline(xintercept=0) + # add vertical axes
  labs(color="Trends") +# change name of box 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# Columns (3) and (5), and (4) and (6) of Table 4
f1 <- function(x) {
  (x <= 89) * (0.0012*x) + (x > 89) * (0.0163*(x-89))
}

f2 <- function(x) {
  (x <= 89) * (-.0240*x + 0.0002*x^2) + (x > 89) * (-.0403*(x-89) + 0.0006*(x-89)^2)
}

ggplot() + 
  xlim(0,200) + 
  ylim(-1,3) +
  labs(x = "Time", y = "Dispersion (in %)") +
  geom_function(fun = f1, aes(color = "Linear Trend")) + 
  geom_function(fun = f2, aes(color = "Quadratic Trend")) +
  geom_hline(yintercept=0) + # add horizontal axes
  geom_vline(xintercept=0) + # add vertical axes
  labs(color="Trends") +# change name of box 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))






# * - Explanations --------------------------------------------------------



### Short Run

regExpSR = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp + SD.shProd + 
                          log(pop) + ur + log(income) + sd_income| 
                          Product + city2 + Month, weights = ~num_strs_comp+1,
                        cluster = "Product^Time", data = db)
summary(regExpSR)

regExpSR_bm = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp + SD.shProd + 
                   log(pop) + ur + log(income) + sd_income| 
                   Product + city2 + Month, weights = ~num_strs_comp+1,
                 cluster = "Product^Time", data = db[db$Time <= median(db$Time),])
summary(regExpSR_bm)

regExpSR_am = feols((SD.PReal*100) ~ PAve + time_median + CatDisp + num_strs_comp + SD.shProd + 
                      log(pop) + ur + log(income) + sd_income| 
                      Product + city2 + Month, weights = ~num_strs_comp+1,
                    cluster = "Product^Time", data = db[db$Time > median(db$Time),])
summary(regExpSR_am)



setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "CatDisp" = "Cat. Entropy",
                 "num_strs_comp" = "Num. Comp. Stores",
                 "SD.shProd" = "SD Sh. Prod.",
                 "log(pop)" = "Log Pop.",
                 "ur" = "Unemp. Rate",
                 "log(income)" = "Log Income",
                 "sd_income" = "SD Income",
                 "time_median" = "Time"))


etable(regExpSR,regExpSR_bm,regExpSR_am,
       digits = 4)

# Paper: short run
etable(regExpSR,regExpSR_bm,regExpSR_am,
       title = "Sources of Price Convergence: Short-run.",
       label = "tab:source-sr",
       #       fontsize = "small",
       placement = "!h",
       digits = 4,
       tex = T)


# # Presentation: short run
# pres2 <- etable(reg4catcompprod,
#                 placement = "ht",
#                 fontsize = "tiny",
#                 digits = 4,
#                 #       coef.style = .("\\color{red} :coef_se:" = "Time"),
#                 tex = T)
# write(pres2,file = "../../Presentaciones/texoutput/pres2.tex")

rm(regExpSR,regExpSR_bm,regExpSR_am)


#### Long Run


regExpLR = feols((SD.PReal*100) ~ PAve + Time * (CatDisp + num_strs_comp + SD.shProd + 
                               log(pop) + ur + log(income) + sd_income)| Product + city2 + Month, 
                 weights = ~num_strs_comp+1, cluster = "Product^Time", data = db)
summary(regExpLR)


# Paper: long run (only trends)
etable(regExpLR,
       title = "Sources of Price Convergence: Long-run effects.",
       label = "tab:source-lr",
       placement = "ht",
       fontsize = "small",
       digits = 4,
       drop = "!Time",
       #       highlight = .("rowcol" = ("Time:CatDisp")),#"Time x Number Comp. Stores","Time x SD Sh. Prod.")),
       tex = T)

# Presentation: long run
# pres31 <- etable(reg3catcompshprodint,
#                 placement = "ht",
#                 fontsize = "tiny",
#                 digits = 4,
#                 drop = "Time",
#                 #       highlight = .("rowcol" = ("Time:CatDisp")),#"Time x Number Comp. Stores","Time x SD Sh. Prod.")),
#                 tex = T)
# write(pres31,file = "../../Presentaciones/texoutput/pres31.tex")
# 
# pres32 <- etable(reg3catcompshprodint,
#                  placement = "ht",
#                  fontsize = "tiny",
#                  digits = 4,
#                  drop = "!Time",
#                  #       highlight = .("rowcol" = ("Time:CatDisp")),#"Time x Number Comp. Stores","Time x SD Sh. Prod.")),
#                  tex = T)
# write(pres32,file = "../../Presentaciones/texoutput/pres32.tex")

rm(regExpLR)
rm(dbo)



# ---------------- Chains ------------------------------------------


# Load database between chains
db_bch <- fread("../../Bases/processed/2023.Between.chains.csv", data.table = F)
db_bch$pop <- db_bch$num.ocup + db_bch$num.unem
db_bch$ur <- db_bch$num.unem / db_bch$pop
db_bch$time_median <- db_bch$Time - median(db_bch$Time)
db_bch$time2_median <- db_bch$time_median * db_bch$time_median


# Load database within chains
db_wch <- fread("../../Bases/processed/2023.Within.chains.csv", data.table = F)
db_wch$pop <- db_wch$num.ocup + db_wch$num.unem
db_wch$ur <- db_wch$num.unem / db_wch$pop
db_wch <- db_wch[db_wch$is.chain == "Chains",] # pick only chains, exclude independent stores
db_wch$time_median <- db_wch$Time - median(db_wch$Time)
db_wch$time2_median <- db_wch$time_median * db_wch$time_median




# * - Dispersion: within ----------------------------------------------------------


reg1w = feols((SD.PReal*100) ~ PAve + Time | Product + city2 ^ chain + Month, 
               weights = ~num_stores_chain,  cluster = "Product^Time", data = db_wch)
summary(reg1w)

reg2w = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 ^ chain + Month,
              weights = ~num_stores_chain, cluster = "Product^Time", data = db_wch)
summary(reg2w)


# setFixest_dict(c("PAve" = "Av. Price",
#                  "SD.PReal*100" = "SD (in %)",
#                  "city2"="Market"))


etable(reg1w,reg2w,
       digits = 4)


# Above and below median time


reg3wb = feols((SD.PReal*100) ~ PAve + Time | Product + city2 ^ chain + Month, weights = ~num_stores_chain,
               cluster = "Product^Time", data = db_wch[db_wch$Time <= median(db_wch$Time),])
summary(reg3wb)

reg3wa = feols((SD.PReal*100) ~ PAve + time_median | Product + city2 ^ chain + Month, weights = ~num_stores_chain,
               cluster = "Product^Time", data = db_wch[db_wch$Time > median(db_wch$Time),])
summary(reg3wa)

reg4wb = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 ^ chain + Month, weights = ~num_stores_chain,
               cluster = "Product^Time", data = db_wch[db_wch$Time <= median(db_wch$Time),])
summary(reg4wb)

reg4wa = feols((SD.PReal*100) ~ PAve + time_median + time2_median| Product + city2 ^ chain + Month, weights = ~num_stores_chain,
               cluster = "Product^Time", data = db_wch[db_wch$Time > median(db_wch$Time),])
summary(reg4wa)

# setFixest_dict(c("PAve" = "Av. Price",
#                  "SD.PReal*100" = "SD (in %)",
#                  "city2"="Market"))


etable(reg3wb,reg4wb,reg3wa,reg4wa,
       digits = 4)



# * - Dispersion: between ----------------------------------------------------------
  
  
reg1b = feols((SD.PReal*100) ~ PAve + Time | Product + city2 + Month, 
                weights = ~num_strs_comp+1,  cluster = "Product^Time", data = db_bch)
summary(reg1b)

reg2b = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 + Month,
              weights = ~num_strs_comp+1, cluster = "Product^Time", data = db_bch)
summary(reg2b)


# setFixest_dict(c("PAve" = "Av. Price",
#                  "SD.PReal*100" = "SD (in %)",
#                  "city2"="Market"))


etable(reg1b,reg2b,
       digits = 4)


# Above and below median time


reg3bb = feols((SD.PReal*100) ~ PAve + Time | Product + city2 + Month, weights = ~num_strs_comp+1,
               cluster = "Product^Time", data = db_bch[db_bch$Time <= median(db_bch$Time),])
summary(reg3bb)

reg3ba = feols((SD.PReal*100) ~ PAve + time_median | Product + city2 + Month, weights = ~num_strs_comp+1,
               cluster = "Product^Time", data = db_bch[db_bch$Time > median(db_bch$Time),])
summary(reg3ba)

reg4bb = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 + Month, weights = ~num_strs_comp+1,
               cluster = "Product^Time", data = db_bch[db_bch$Time < median(db_bch$Time),])
summary(reg4bb)

reg4ba = feols((SD.PReal*100) ~ PAve + Time + Time2| Product + city2 + Month, weights = ~num_strs_comp+1,
               cluster = "Product^Time", data = db_bch[db_bch$Time > median(db_bch$Time),])
summary(reg4ba)

# setFixest_dict(c("PAve" = "Av. Price",
#                  "SD.PReal*100" = "SD (in %)",
#                  "city2"="Market"))


etable(reg3bb,reg4bb,reg3ba,reg4ba,
       digits = 4)



########## Results tables


setFixest_dict(c("PAve" = "Av. Price",
                 "SD.PReal*100" = "SD (in %)",
                 "city2"="Market",
                 "CatDisp" = "Cat. Entropy",
                 "num_strs_comp" = "Num. Comp. Stores",
                 "num_stores_chain" = "Num. Chain Own Stores",
                 "SD.shProd" = "SD Sh. Prod.",
                 "log(pop)" = "Log Pop.",
                 "ur" = "Unemp. Rate",
                 "log(income)" = "Log Income",
                 "sd_income" = "SD Income"))


# For paper: table in paper
etable(reg1w,reg2w,reg1b,reg2b,
       title = "Price Convergence: Within and Between Chains.",
       label = "tab:convergence-chains",
       fontsize = "small",
       placement = "!h",
       digits = 4,
       tex = T)

# For paper: appendix table
etable(reg3wb,reg4wb,reg3wa,reg4wa,reg3bb,reg4bb,reg3ba,reg4ba,
       title = "Price Convergence: Within and Between Chains, Until and After Median Period.",
       label = "tab:appendix-convergence-chains",
       fontsize = "footnotesize",
       placement = "!htbp",
       digits = 4,
       tex = T)



# For presentation
# pres4 <- etable(reg1,reg2,reg3,reg4,#reg5,
# #       title = "Convergence Estimation by Chains.",
# #       label = "chain-base-estimation",
#        fontsize = "tiny",
#        placement = "!h",
#        coef.style = .("\\color{red} :coef_se:" = "Time"),
#        digits = 4,
#        tex = T)
# write(pres4,file = "../../Presentaciones/texoutput/pres4.tex")
rm(reg1w,reg2w,reg1b,reg2b,reg3wb,reg4wb,reg3wa,reg4wa,reg3bb,reg4bb,reg3ba,reg4ba)




# * - Explanations: WCh --------------------------------------------------------



### Short Run

regExpWSR = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp + num_stores_chain 
                 + SD.shProd + log(pop) + ur + log(income) + sd_income| 
                   Product + city2 ^ chain + Month, weights = ~num_stores_chain,
                 cluster = "Product^Time", data = db_wch)
summary(regExpWSR)

regExpWSR_bm = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp + num_stores_chain 
                     + SD.shProd + log(pop) + ur + log(income) + sd_income| 
                       Product + city2 ^ chain + Month, weights = ~num_stores_chain,
                    cluster = "Product^Time", data = db_wch[db_wch$Time <= median(db_wch$Time),])
summary(regExpWSR_bm)

regExpWSR_am = feols((SD.PReal*100) ~ PAve + time_median + CatDisp + num_strs_comp + num_stores_chain 
                     + SD.shProd + log(pop) + ur + log(income) + sd_income| 
                       Product + city2 ^ chain + Month, weights = ~num_stores_chain,
                    cluster = "Product^Time", data = db_wch[db_wch$Time > median(db_wch$Time),])
summary(regExpWSR_am)





etable(regExpWSR,regExpWSR_bm,regExpWSR_am,
       digits = 4)



### Long Run

regExpWLR = feols((SD.PReal*100) ~ PAve + Time * (CatDisp + num_strs_comp + num_stores_chain 
                  + SD.shProd + log(pop) + ur + log(income) + sd_income)| 
                    Product + city2 ^ chain + Month, weights = ~num_stores_chain,
                  cluster = "Product^Time", data = db_wch)
summary(regExpWLR)




# * - Explanations: BCh --------------------------------------------------------



### Short Run

regExpBSR = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp  
                  + SD.shProd + log(pop) + ur + log(income) + sd_income| 
                    Product + city2 + Month, weights = ~num_strs_comp+1,
                  cluster = "Product^Time", data = db_bch)
summary(regExpBSR)

regExpBSR_bm = feols((SD.PReal*100) ~ PAve + Time + CatDisp + num_strs_comp  
                     + SD.shProd + log(pop) + ur + log(income) + sd_income| 
                       Product + city2 + Month, weights = ~num_strs_comp+1,
                     cluster = "Product^Time", data = db_bch[db_bch$Time <= median(db_bch$Time),])
summary(regExpBSR_bm)

regExpBSR_am = feols((SD.PReal*100) ~ PAve + time_median + CatDisp + num_strs_comp  
                     + SD.shProd + log(pop) + ur + log(income) + sd_income| 
                       Product + city2 + Month, weights = ~num_strs_comp+1,
                     cluster = "Product^Time", data = db_bch[db_bch$Time > median(db_bch$Time),])
summary(regExpBSR_am)


etable(regExpBSR,regExpBSR_bm,regExpBSR_am,
       digits = 4)



### Long Run

regExpBLR = feols((SD.PReal*100) ~ PAve + Time * (CatDisp + num_strs_comp 
                    + SD.shProd + log(pop) + ur + log(income) + sd_income)| 
                    Product + city2 + Month, weights = ~num_strs_comp+1,
                  cluster = "Product^Time", data = db_bch)
summary(regExpBLR)





# ** - Tables --------------------------------------------------------


########## Short run


# For paper: table in paper
etable(regExpWSR,regExpBSR,
       title = "Sources of Price Convergence: Short-run. Within and Between Chains.",
       label = "tab:sources-chains-sr",
       fontsize = "small",
       placement = "!h",
       digits = 4,
       tex = T)

# For paper: appendix table
etable(regExpWSR_bm,regExpWSR_am,regExpBSR_bm,regExpBSR_am,
       title = "Sources of Price Convergence: Short-run. Within and Between Chains, Until and After Median Period.",
       label = "tab:appendix-sources-chains-sr",
       fontsize = "small",
       placement = "!htbp",
       digits = 4,
       tex = T)



########## Long run


# For paper: table in paper
etable(regExpWLR,regExpBLR,
       title = "Sources of Price Convergence: Long-run. Within and Between Chains.",
       label = "tab:sources-chains-lr",
       fontsize = "small",
       placement = "!h",
       drop = "!Time",
       digits = 4,
       tex = T)


rm(egExpWSR_bm,regExpWSR_am,regExpBSR_bm,regExpBSR_am,regExpWLR,regExpBLR)
rm(regExpBSR,regExpBSR_bm,regExpBSR_am)



## End of script ------------