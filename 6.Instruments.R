

dbF <- get(load("~/Dropbox/Docs/Investigacion/2018.Price convergence/Bases/2018BaseResidualsRev.Rdata"))


####################################
######## Create instruments ########

colnames(dbF)
# Add CCZ information
supers <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/Establecimientos.csv", header = T, sep = ',')
colnames(supers)
supers <- supers[,c(1,8)]
dbF <- merge(dbF, supers, by = "Super")
rm(supers)

dbF$Inst.Var <- dbF$Inst.Comp <- 0

for (t in unique(dbF$Time)) {
  for (p in unique(dbF$Product)) {
    for (i in unique(dbF$Super)) {
      if (dbF[dbF$Super == i,]$city.number != 30) {
        x <- unique(get("dbF")[dbF$Super == i,]$city.number)
        Sub <- dbF[dbF$Product == p & dbF$city.number == x & dbF$Time == t,]
        if (nrow(Sub[Sub$Super == i,]) < 1) {next}
        dbF[which(dbF$Super == i & dbF$Time == t & dbF$Product == p),]$Inst.Comp <- 
          mean(Sub[Sub$Super != i,]$competition)
        dbF[which(dbF$Super == i & dbF$Time == t & dbF$Product == p),]$Inst.Var <-
          mean(Sub[Sub$Super != i,]$variety)
      }
      else {
        x <- unique(get("dbF")[dbF$Super == i,]$ccz)
        Sub <- dbF[dbF$Product == p & dbF$ccz == x & dbF$Time == t,]
        if (nrow(Sub[Sub$Super == i,]) < 1) {next}
        dbF[which(dbF$Super == i & dbF$Time == t & dbF$Product == p),]$Inst.Comp <- 
          mean(Sub[Sub$Super != i,]$competition)
        dbF[which(dbF$Super == i & dbF$Time == t & dbF$Product == p),]$Inst.Var <-
          mean(Sub[Sub$Super != i,]$variety)
      }
    }
  }
}