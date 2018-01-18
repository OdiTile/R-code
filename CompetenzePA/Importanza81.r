# Importanza8.r
# Classificazione binaria
# Utilizzo della funzione recode() (car)
rm(list=objects())
objects()
# library(epicalc)
# library(ca)
library(car)
library(xlsReadWrite)
dati<-read.xls( "c:\\dati\\lav\\Bolzan\\CompetenzePA\\DatiCompetenzePA.xls", 
          colNames = TRUE, 
          sheet = "OsservateAttese", 
          type = "data.frame" )
# dati[1:5,]
# names(dati)
# sostituisco i dati mancanti con la mediana
for(i in 2:64){
dati[,i][which(is.na(dati[,i]))]<-round(median(dati[,i],na.rm=TRUE))}
dati[1:5,]
which(is.na(dati))
datir<-round(dati); datir[1:5,]
for(i in 2:53){
datir[,i]<-recode(datir[,i],"1:5=1;c(6,7,8)=2;c(9,10)=3",as.factor.result=TRUE)}
head(datir)
# datir[,54:56]
# quali sono le risposte convinte sul ruolo del comunne
# tra erogatore-54 mediatore-55 e promotore-56?
# which(datir[,54]>50)
# which(datir[,55]>50)
# which(datir[,56]>50)
# which(datir[,54:56]>50)


# mission attese - osservate

v65<-dati[,57]-dati[,54]
v65<-recode(v65,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v65
v66<-dati[,58]-dati[,55]
v66<-recode(v66,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v66
v67<-dati[,59]-dati[,56]
v67<-recode(v67,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v67
# comuni riclassificati 1=1,2,3 2=4,5
v68<-recode(dati$v60,"1:3=1; 4:5=2 ",as.factor.result=FALSE); v68
datir<-cbind(datir, v65, v66,v67)
head(datir)

# fine ricodifica preliminare =========================================
# =====================================================================
# usare dati
# dati2 contiene fattori per variabili categoriali
dati2<-dati
dati2$v61<-as.factor(dati2$v61)
dati2$v62<-as.factor(dati2$v62)
dati2$v64<-as.factor(dati2$v64)
str(dati2)

# segmentazione binaria
library(rpart)

# segmentazione binaria su erogatore 0 - 100
mod<-rpart(v57~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Erogatore 0 - 100%")
text(mod,digits=3, all=TRUE,use.n=F,fancy=F)
mod
# segmentazione binaria su erogatore 0 - 100
# solo variabili oggettive
mod<-rpart(v57~v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Erogatore 0 - 100% per var oggettive")
text(mod,digits=3, all=TRUE,use.n=F,fancy=F)
mod

# segmentazione binaria su mediatore 0 - 100
mod<-rpart(v58~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Mediatore 0 - 100%")
text(mod,digits=3, all=TRUE,use.n=F,fancy=F)
mod
# segmentazione binaria su mediatore 0 - 100
# solo variabili oggettive
mod<-rpart(v58~v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Mediatore 0 - 100% per var oggettive")
text(mod,digits=3, all=TRUE,use.n=F,fancy=F)
mod

# segmentazione binaria su promotore 0 - 100
mod<-rpart(v59~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Promotore 0 - 100%")
text(mod,digits=3, all=TRUE,use.n=F,fancy=F)
mod
# segmentazione binaria su promotore 0 - 100
# solo variabili oggettive
mod<-rpart(v59~v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Promotore 0 - 100% per var oggettive")
text(mod,digits=3, all=TRUE,use.n=F,fancy=F)
mod




# segmentazione binaria su erogatore atteso - osservato
mod<-rpart((v57-v54)~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Erogatore atteso - osservato")
text(mod,digits=3, all=TRUE)
mod

# segmentazione binaria su mediatore atteso - osservato
mod<-rpart((v58-v55)~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Mediatore atteso - osservato")
text(mod,digits=3, all=TRUE)
mod
# segmentazione binaria su promotore atteso - osservato
mod<-rpart((v59-v56)~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v62
	         +v63+v64, data=dati2)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, uniform=TRUE, main="Promotore atteso - osservato")
text(mod,digits=3, all=TRUE)
mod




