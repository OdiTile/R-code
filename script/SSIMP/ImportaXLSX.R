# importazione xlsx
# file  "c:\dati\lav\corsi\ssimp\ElInf1516\Dispense\R\ImportaXLSX.R"
# dataset disponibili data() 
# data(package = .packages(all.available = TRUE))
# installa pacchetti readxl (R 3.2.4), xlsx (richiede java), gdata (read.xls richiede Perl>=5.10)
# seleziona la cartella di lavoro
setwd(choose.dir())
getwd()
# alternativa
# setwd("C:/dati/lav/corsi/ssimp/ElInf1516/Dispense/R")
# setwd("C:/Users/mario.bonamin/Desktop/Dati")
dir()
library(readxl)
# read_excel(path, sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
bmi<-read_excel("bmichild.xlsx")
bmi
str(bmi)
plot(bmi$height,bmi$weight)
# salvataggio file csv
write.table(bmi, file="bmi.txt")
dir()
m<-table(bmi$GENDER,bmi$zep)
chisq.test(m)
chi2<-chisq.test(m)
chi2$observed
chi2$expected
#
wb<-read_excel("weight_birth.xls")
wb
#
library(xlsx)
dat <- read.xlsx("bmichild.xlsx", sheetName="Exo1")
dat
