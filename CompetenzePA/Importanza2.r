
# Utilizzo della funzione recode() (car)
rm(list=objects())
objects()
library(ca)
library(car)
library(xlsReadWrite)
dati<-read.xls( "c:\\dati\\lav\\Bolzan\\CompetenzePA\\DatiCompetenzePA.xls", 
          colNames = TRUE, 
          sheet = "OsservateAttese", 
          type = "data.frame" )
dati[1:5,]
names(dati)
str(dati)
dati[18,]
# controllo dati mancanti
# dati$v6
# median(dati$v6,na.rm=TRUE)
# which(is.na(dati$v6))
# dati$v6[is.na(dati$v6)]
# dati$v6[which(is.na(dati$v6))]<-median(dati$v6,na.rm=TRUE)

# sostituisco i dati mancanti con la mediana
for(i in 2:64){
dati[,i][which(is.na(dati[,i]))]<-round(median(dati[,i],na.rm=TRUE))
}
dati[1:5,]
which(is.na(dati))

datir<-round(dati); datir[1:5,]
for(i in 2:53){
datir[,i]<-recode(datir[,i],"1:5=1;c(6,7,8)=2;c(9,10)=3",as.factor.result=TRUE)}

# v54 - v59 vengono distribuite per frequenze omogenee

table(dati$v54)
round(cumsum(table(dati$v54))/length(dati$v54),2)
round(cumsum(table(dati$v55))/length(dati$v55),2)
round(cumsum(table(dati$v56))/length(dati$v56),2)
round(cumsum(table(dati$v57))/length(dati$v57),2)
round(cumsum(table(dati$v58))/length(dati$v58),2)
round(cumsum(table(dati$v59))/length(dati$v59),2)
datir[,54]<-recode(datir[,54],"0:35=1; 36:45=2; 46:100=3",as.factor.result=TRUE)
datir[,55]<-recode(datir[,55],"0:15=1; 16:25=2; 26:100=3",as.factor.result=TRUE)
datir[,56]<-recode(datir[,56],"0:25=1; 26:35=2; 36:100=3",as.factor.result=TRUE)
datir[,57]<-recode(datir[,57],"0:40=1; 41:50=2; 51:100=3",as.factor.result=TRUE)
datir[,58]<-recode(datir[,58],"0:15=1; 16:25=2; 26:100=3",as.factor.result=TRUE)
datir[,59]<-recode(datir[,59],"0:20=1; 21:25=2; 26:100=3",as.factor.result=TRUE)


# for(i in 54:59){
# datir[,i]<-recode(datir[,i],"0:35=1; 36:45=2; 46:100=3",as.factor.result=TRUE)}
round(cumsum(table(datir[,54]))/length(datir[,54]),2)
round(cumsum(table(datir[,55]))/length(datir[,55]),2)
round(cumsum(table(datir[,56]))/length(datir[,56]),2)
round(cumsum(table(datir[,57]))/length(datir[,57]),2)
round(cumsum(table(datir[,58]))/length(datir[,58]),2)
round(cumsum(table(datir[,59]))/length(datir[,59]),2)

# for(i in 54:59){
# datir[,i]<-recode(datir[,i],"0:33=1; 34:66=2; 67:100=3",as.factor.result=TRUE)}
# dati[,54:59]
# datir[,54:59]
# which(datir[,54:59]==3)
# table(dati$v2,dati$v3)

# dataset variabili v2 - v27 osservate -> datir1
datir1<-datir[,2:27]; datir1[1:10,]

# dataset variabili v2-v27; v54-v56 osservate -> datir2
datir2<-cbind(datir[,2:27],datir[,54:56]); datir2[1:10,]

# dataset variabili v2-v27; v54-v56; v60-64 osservate -> datir3
datir3<-cbind(datir[,2:27],datir[,54:56],datir[,60:64]); datir3[1:10,]

# dataset variabili v60 - v64 osservate -> datir4
datir4<-datir[,60:64]; datir4[1:10,]

# corrispondenze multiple per datir1
mjca(datir1)
summary(mjca(datir1))
plot(mjca(datir1))
# sort(mjca(datir1)$rowdist)
# sort(mjca(datir1)$rowinertia)
# mjca(datir1)$rowcoord[,1:2]
# sort(mjca(datir1)$coldist)
# mjca(datir1)$colcoord[,1:2]

# corrispondenze multiple per datir3 completo
datir3[1:3,]
mjca(datir3)
summary(mjca(datir3))
plot(mjca(datir3))
righe<-c(1:10,130:134); righe
sort(mjca(datir3)$rowdist, decreasing = TRUE)[righe]
col<-c(1:10,91:95); col
sort(mjca(datir3)$coldist, decreasing = TRUE)[col]

# corrispondenze multiple per datir3 
# v54-v56 proiettate
# names(datir3)
mjca(datir3,supcol=c(27:29))
summary(mjca(datir3,supcol=c(27:29)))
plot(mjca(datir3,supcol=c(27:29)))
righe<-c(1:10,130:134); righe
sort(mjca(datir3)$rowdist, decreasing = TRUE)[righe]
col<-c(1:10,91:95); col
sort(mjca(datir3)$coldist, decreasing = TRUE)[col]


# corrispondenze multiple per datir3 
# v60-v64 proiettate
names(datir3)
mjca(datir3,supcol=c(30:34))
summary(mjca(datir3,supcol=c(30:34)))
plot(mjca(datir3,supcol=c(30:34)))
righe<-c(1:10,130:134); righe
sort(mjca(datir3)$rowdist, decreasing = TRUE)[righe]
col<-c(1:10,91:95); col
sort(mjca(datir3)$coldist, decreasing = TRUE)[col]

# mission attese - osservate

v65<-dati[,57]-dati[,54]
v65<-recode(v65,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=TRUE); v65
v66<-dati[,58]-dati[,55]
v66<-recode(v66,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=TRUE); v66
v67<-dati[,59]-dati[,56]
v67<-recode(v67,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=TRUE); v67
# comuni riclassificati 1=1,2,3 2=4,5
v68<-recode(dati$v60,"1:3=1; 4:5=2 ",as.factor.result=TRUE); v68

# dataset variabili v2-v27; v54-v56; v60-64 osservate; v65-67 diff mission -> datir5
# 1-26=v2-v27; 27-29=v54-v56; 30-34=v60-v64; 35-37=v65-v66-v67
datir5<-cbind(datir[,2:27],datir[,54:56],datir[,60:64],v65,v66,v67,v68); datir5[1:10,]

# corrispondenze multiple per datir5
# v65-v67 proiettate
names(datir5)
mjca(datir5,supcol=c(35:37))
summary(mjca(datir5,supcol=c(35:37)))
plot(mjca(datir5,supcol=c(35:37)),what=c("none","all"))
righe<-c(1:10,130:134); righe
sort(mjca(datir5)$rowdist, decreasing = TRUE)[righe]
col<-c(1:10,91:95); col
sort(mjca(datir5)$coldist, decreasing = TRUE)[col]

# prova distribuzione frequenze
cumsum(table(as.integer(as.character(v65))))

# corrispondenze multiple per datir5
# tutte
names(datir5)
mjca(datir5)
summary(mjca(datir5))
plot(mjca(datir5[-(27:34)]))
righe<-c(1:10,130:134); righe
sort(mjca(datir5)$rowdist, decreasing = TRUE)[righe]
col<-c(1:10,91:95); col
sort(mjca(datir5)$coldist, decreasing = TRUE)[col]

#prove mjca()
# 1-26=v2-v27; 27-29=v54-v56; 30-34=v60-v64; 35-37=v65-v66-v67; 38=v68
plot(mjca(datir5[,1:38]),what=c("none","all"))
title(xlab="v1-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(2:10,35:38)],supcol=c(10:12)),what=c("none","all"))
title(xlab="v3-v11, v68, v65-v67 proiettate", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(2:5,35:38)],supcol=c(5:7)),what=c("none","all"))
title(xlab="v3-v6, v68, v65-v67 proiettate", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(12:15,35:38)],supcol=c(5:7)),what=c("none","all"))
title(xlab="v13-v16, v68, v65-v67 proiettate", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(2:4,35:38)]),what=c("none","all"))
title(xlab="v3-v5, v65-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(1:2,35:38)]),what=c("none","all"))
title(xlab="v2-v3, v65-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(1:10,35:38)]),what=c("none","all"))
title(xlab="v2-v11, v65-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(1:15,35:38)]),what=c("none","all"))
title(xlab="v2-v16, v65-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(12:15,38)]),what=c("none","all"))
title(xlab="v13-v15, v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(15:20,35:38)]),what=c("none","all"))
title(xlab="v16-v21, v65-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(21:23,35:38)]),what=c("none","all"))
title(xlab="v22-v24, v65-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(3,35:38)]),what=c("none","all"))
title(xlab="v4, v65-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(3,31:34,38)]),what=c("none","all"))
title(xlab="v4, v61-v64, v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(2:5,38)]),what=c("none","all"))
title(xlab="v3-v6, v65-v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(12,31:34,38)]),what=c("none","all"))
title(xlab="v13, v61-v64, v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(13,31:34,38)]),what=c("none","all"))
title(xlab="v14, v61-v64, v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(13,35:37,38)]),what=c("none","all"))
title(xlab="v14, v65-v67, v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(14,35:37,38)]),what=c("none","all"))
title(xlab="v15, v65-v67, v68", main="Analisi delle corrispondenze multiple")

plot(mjca(datir5[,c(15,35:37,38)]),what=c("none","all"))
title(xlab="v16, v65-v67, v68", main="Analisi delle corrispondenze multiple")





# codice Greenacre
library(MASS)
library(ca)
data(farms)
farms
mjca(farms)
pchlist()
data(wg93)
wg93
mjca(wg93[,1:4])
summary(mjca(wg93[,1:4], lambda="Burt"))
plot(mjca(wg93[,1:4]))
data(smoke)
smoke
plot(ca(smoke, supcol=1))
plot(ca(smoke), mass=TRUE, contrib="absolute",
     map="rowgreen", arrows=c(FALSE,TRUE))
plot3d(ca(smoke,nd=3))



