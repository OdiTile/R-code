
# Utilizzo della funzione recode() (car)
rm(list=objects())
objects()
#library(epicalc)
library(ca)
library(car)
library(xlsReadWrite)
dati<-read.xls( "c:\\dati\\lav\\Bolzan\\CompetenzePA\\DatiCompetenzePA.xls", 
          colNames = TRUE, 
          sheet = "OsservateAttese", 
          type = "data.frame" )
dati[1:5,]
names(dati)
# sostituisco i dati mancanti con la mediana
for(i in 2:64){
dati[,i][which(is.na(dati[,i]))]<-round(median(dati[,i],na.rm=TRUE))}
dati[1:5,]
which(is.na(dati))
datir<-round(dati); datir[1:5,]
for(i in 2:53){
datir[,i]<-recode(datir[,i],"1:5=1;c(6,7,8)=2;c(9,10)=3",as.factor.result=TRUE)}
head(datir)
datir[,54:56]
# quali sono le risposte convinte sul ruolo del comunne
# tra erogatore-54 mediatore-55 e promotore-56?
which(datir[,54]>50)
which(datir[,55]>50)
which(datir[,56]>50)
which(datir[,54:56]>50)


# mission attese - osservate

v65<-dati[,57]-dati[,54]
v65<-recode(v65,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v65
v66<-dati[,58]-dati[,55]
v66<-recode(v66,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v66
v67<-dati[,59]-dati[,56]
v67<-recode(v67,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v67
# comuni riclassificati 1=1,2,3 2=4,5
v68<-recode(dati$v60,"1:3=1; 4:5=2 ",as.factor.result=FALSE); v68



# regressione logistica
# riclassificazione importanza elevata verso tutto il resto

round(cumsum(table(dati$v57))/length(dati$v57),2)
round(cumsum(table(dati$v58))/length(dati$v58),2)
round(cumsum(table(dati$v59))/length(dati$v59),2)
# alto = ultimo 33%
y57<-recode(datir[,57],"0:50=0; 51:100=1")
y58<-recode(datir[,58],"0:25=0; 26:100=1")
y59<-recode(datir[,59],"0:25=0; 26:100=1")

# dummy -> fattori
v621<-(datir$v62==1)*1; # v621
v622<-(datir$v62==2)*1; # v622
v623<-(datir$v62==3)*1; # v623

v642<-(datir$v64==2)*1; # v642



mod57<-glm(y57~datir$v60+datir$v61+datir$v63+v621+v622+v623+v642+v65+v66+v67, 
family=binomial())
summary(mod57)

mod58<-glm(y58~datir$v60+datir$v61+datir$v63+v621+v622+v623+v642+v65+v66+v67, 
family=binomial())
summary(mod58)

mod59<-glm(y59~datir$v60+datir$v61+datir$v63+v621+v622+v623+v642+v65+v66+v67, 
family=binomial())
summary(mod59)



library(epicalc)

logistic.display(mod57)
logistic.display(mod58)
logistic.display(mod59)

# tabelle di contingenza
tabpct(y57,datir$v60)
# stima del rischio attraverso l'odds ratio
cc(y57,datir$v61, design="case-control")
















