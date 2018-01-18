
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

# v54 - v59 vengono distribuite per frequenze omogenee
# table(dati$v54)
# round(cumsum(table(dati$v54))/length(dati$v54),2)
# round(cumsum(table(dati$v55))/length(dati$v55),2)
# round(cumsum(table(dati$v56))/length(dati$v56),2)
# round(cumsum(table(dati$v57))/length(dati$v57),2)
# round(cumsum(table(dati$v58))/length(dati$v58),2)
# round(cumsum(table(dati$v59))/length(dati$v59),2)
# datir[,54]<-recode(datir[,54],"0:35=1; 36:45=2; 46:100=3",as.factor.result=TRUE)
# datir[,55]<-recode(datir[,55],"0:15=1; 16:25=2; 26:100=3",as.factor.result=TRUE)
# datir[,56]<-recode(datir[,56],"0:25=1; 26:35=2; 36:100=3",as.factor.result=TRUE)
# datir[,57]<-recode(datir[,57],"0:40=1; 41:50=2; 51:100=3",as.factor.result=TRUE)
# datir[,58]<-recode(datir[,58],"0:15=1; 16:25=2; 26:100=3",as.factor.result=TRUE)
# datir[,59]<-recode(datir[,59],"0:20=1; 21:25=2; 26:100=3",as.factor.result=TRUE)

# round(cumsum(table(datir[,54]))/length(datir[,54]),2)
# round(cumsum(table(datir[,55]))/length(datir[,55]),2)
# round(cumsum(table(datir[,56]))/length(datir[,56]),2)
# round(cumsum(table(datir[,57]))/length(datir[,57]),2)
# round(cumsum(table(datir[,58]))/length(datir[,58]),2)
# round(cumsum(table(datir[,59]))/length(datir[,59]),2)

########################################################################################
#
# Tutte le competenze, le variabili, assieme
# Ruolo > 50 (anche > 49)
#
plot(mjca(datir[,2:53]),what=c("none","all"))
title(xlab="Tutte le competenze", main="Analisi delle corrispondenze multiple")
summary(mjca(datir[,2:53]),what=c("none","all"))

plot(mjca(datir[,2:53]),what=c("all","all"))
title(xlab="Variabili e rispondenti", main="Analisi delle corrispondenze multiple")

# competenze osservate
plot(mjca(datir[,2:27]),what=c("none","all"))
title(xlab="Competenze osservate", main="Analisi delle corrispondenze multiple")
summary(mjca(datir[,2:27]),what=c("none","all"))

# competenze attese
plot(mjca(datir[,28:53]),what=c("none","all"))
title(xlab="Competenze attese", main="Analisi delle corrispondenze multiple")
summary(mjca(datir[,2:27]),what=c("none","all"))


# mappa dei rispondenti 
# x(-2.5 +1) y(-0.5 +2.5)
plot(mjca(datir[,2:53]),what=c("all","none"))
title(xlab="Tutte le competenze", main="Analisi delle corrispondenze multiple")
out<-mjca(datir[,2:53])
# head(out)
punti<-summary(out, scree=F, rows = T)$rows
head(punti)

# plot di alcune variabili interessanti
puntic<-summary(out, scree=F, rows = T)$columns
head(puntic)
puntic
puntico<-puntic[,c(5,8)]
head(puntico)
x<-as.numeric(puntico[,1])/1000 
# x
y<-as.numeric(puntico[,2])/1000 
# y
# tutte le variabili
plot(x,y,xlim=c(-1.5,0.7),ylim=c(-0.9,0.7),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Tutte le variabili")
abline(h=0, v=0, lty=3)
text(x,y, puntic$name,cex=1)

# alcune variabili 1
plot(x,y,xlim=c(-1.5,0.2),ylim=c(-0.2,0.7),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Alcune variabili")
abline(h=0, v=0, lty=3)
interv<-c(43:51,58:60)
text(x[interv],y[interv], puntic$name[interv],cex=1)

# alcune variabili 2
plot(x,y,xlim=c(-1,0.5),ylim=c(-0.8,0.4),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Alcune variabili")
abline(h=0, v=0, lty=3)
interv<-c(64:66,88:90,103:105,112:114,136:138)
text(x[interv],y[interv], puntic$name[interv],cex=1)






# comuni erogatori
puntier<-punti[which(datir[,54]>50),c(5,8)]
puntier
x<-as.numeric(puntier[,1])/1000; x
y<-as.numeric(puntier[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore vs tutte le competenze")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-0.7,0.6),ylim=c(-0.6,0.5),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore vs tutte le competenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntier),cex=1)

# comuni mediatori
puntime<-punti[which(datir[,55]>49),c(5,8)]
puntime
x<-as.numeric(puntime[,1])/1000; x
y<-as.numeric(puntime[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Mediatore vs tutte le competenze")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-0.7,0.6),ylim=c(-0.5,1.5),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Mediatore vs tutte le competenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntime),cex=1)

# comuni promotori
puntipr<-punti[which(datir[,56]>50),c(5,8)]
puntipr
x<-as.numeric(puntipr[,1])/1000; x
y<-as.numeric(puntipr[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Promotore vs tutte le competenze")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-0.5,0.1),ylim=c(-0.2,0.5),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Promotore vs tutte le competenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipr),cex=1)

# comuni altro (non erogatori, non mediatori, non promotori)
altro<-rep(0,134); altro
for(i in 1:134){altro[i]<-(datir[i,54]<51 && datir[i,55]<51 && datir[i,56]<51)
}
altro

puntial<-punti[which(altro[]==1),c(5,8)]
puntial
x<-as.numeric(puntial[,1])/1000; x
y<-as.numeric(puntial[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Altro vs tutte le competenze")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-1.0,0.6),ylim=c(-0.6,0.6),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Altro vs tutte le competenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntial),cex=1)



##############################################################
#
# rispondenti Er Me Pr su competenze attese
#
plot(mjca(datir[,28:53]),what=c("none","all"))
title(xlab="Competenze Attese", main="Analisi delle corrispondenze multiple")
out<-mjca(datir[,28:53])
# head(out)
punti<-summary(out, scree=F, rows = T)$rows
head(punti)
# plot di alcune variabili interessanti
puntic<-summary(out, scree=F, rows = T)$columns
head(puntic)
puntic
puntico<-puntic[,c(5,8)]
head(puntico)
x<-as.numeric(puntico[,1])/1000 
# x
y<-as.numeric(puntico[,2])/1000 
# y
# tutte le variabili
plot(x,y,xlim=c(-0.9,1.7),ylim=c(-0.7,0.9),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Competenze attese")
abline(h=0, v=0, lty=3)
text(x,y, puntic$name,cex=1)

# comuni erogatori
puntier<-punti[which(datir[,54]>50),c(5,8)]
puntier
x<-as.numeric(puntier[,1])/1000; x
y<-as.numeric(puntier[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore vs competenze attese")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-0.7,0.6),ylim=c(-0.7,0.5),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore vs competenze attese")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntier),cex=1)

# comuni mediatori
puntime<-punti[which(datir[,55]>49),c(5,8)]
puntime
x<-as.numeric(puntime[,1])/1000; x
y<-as.numeric(puntime[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Mediatore vs competenze attese")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-0.7,0.6),ylim=c(-0.5,1.5),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Mediatore vs competenze attese")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntime),cex=1)

# comuni promotori
puntipr<-punti[which(datir[,56]>50),c(5,8)]
puntipr
x<-as.numeric(puntipr[,1])/1000; x
y<-as.numeric(puntipr[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Promotore vs competenze attese")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-0.2,0.8),ylim=c(-0.7,0.3),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Promotore vs competenze attese")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipr),cex=1)

# comuni altro (non erogatori, non mediatori, non promotori)
altro<-rep(0,134); altro
for(i in 1:134){altro[i]<-(datir[i,54]<51 && datir[i,55]<51 && datir[i,56]<51)
}
altro

puntial<-punti[which(altro[]==1),c(5,8)]
puntial
x<-as.numeric(puntial[,1])/1000; x
y<-as.numeric(puntial[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Altro vs competenze attese")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-0.8,1.2),ylim=c(-0.7,0.6),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Altro vs competenze attese")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntial),cex=1)

#################################################################
# 5-4-09
#  caratteristiche dei rispondenti su competenze attese
#
#
# Ricodifica delle variabili
head(datir)
# datir[,60]
datir[,60]<-recode(datir[,60],"1:2=1; 3:4=2; 5=3",as.factor.result=TRUE)
datir[,60]
datir[,63]<-recode(datir[,63],"1:2=1; 3=2",as.factor.result=TRUE)
datir[,63]
datir[,64]<-recode(datir[,64],"2=1; c(1,3)=2",as.factor.result=TRUE)
datir[,64]

# competenze attese
plot(mjca(datir[,28:53]),what=c("none","all"))
title(xlab="Competenze attese", main="Analisi delle corrispondenze multiple")
summary(mjca(datir[,2:27]),what=c("none","all"))
# competenze attese + caratteristiche
# plot(mjca(datir[,c(28:53,60:64)]),what=c("none","all"))
# title(xlab="Competenze attese", main="Analisi delle corrispondenze multiple")
# summary(mjca(datir[,2:27]),what=c("none","all"))
##############################################################
#
# rispondenti su competenze attese
#
plot(mjca(datir[,c(28:53,60:64)]),what=c("none","all"))
title(xlab="Competenze Attese", main="Analisi delle corrispondenze multiple")
out<-mjca(datir[,c(28:53,60:64)])
# head(out)
punti<-summary(out, scree=F, rows = T)$rows
head(punti)
# plot di alcune variabili interessanti
puntic<-summary(out, scree=F, rows = T)$columns
head(puntic)
puntic
puntico<-puntic[79:91,c(5,8)]
head(puntico)
x<-as.numeric(puntico[,1])/1000 
y<-as.numeric(puntico[,2])/1000 
# tutte le variabili
plot(x,y,xlim=c(-0.7,1.6),ylim=c(-1,1),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Competenze attese")
abline(h=0, v=0, lty=3)
text(x,y, puntic$name,cex=1)

# dimensione del comune v60 1<5000, 5000<2<15000, 3>15000
puntico<-puntic[79:81,c(5,8)]
head(puntico)
x<-as.numeric(puntico[,1])/1000 
y<-as.numeric(puntico[,2])/1000 
plot(x,y,xlim=c(-0.7,1.6),ylim=c(-1,1),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Competenze attese")
abline(h=0, v=0, lty=3)
# text(x,y, row.names(puntico),cex=1)
text(x,y, puntic$name[79:81],cex=1)
# text(x,y, c(1,2,3),cex=1)
text(0.8,0.9,"Dimensioni del comune")
text(0.8,0.75,"1<5000, 5000<2<15000, 3>15000")

# M - F v61 1=m 2=f
puntico<-puntic[82:83,c(5,8)]
head(puntico)
x<-as.numeric(puntico[,1])/1000 
y<-as.numeric(puntico[,2])/1000 
plot(x,y,xlim=c(-0.7,1.6),ylim=c(-1,1),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Competenze attese")
abline(h=0, v=0, lty=3)
# text(x,y, row.names(puntico),cex=1)
text(x,y, puntic$name[82:83],cex=1)
text(0.8,0.9,"Genere")
text(0.8,0.75,"1=M, 2=F")

# Ruolo 1 dg 2 dir 3 sc 4 po
puntico<-puntic[84:87,c(5,8)]
head(puntico)
x<-as.numeric(puntico[,1])/1000 
y<-as.numeric(puntico[,2])/1000 
plot(x,y,xlim=c(-0.7,1.6),ylim=c(-1,1),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Competenze attese")
abline(h=0, v=0, lty=3)
# text(x,y, row.names(puntico),cex=1)
text(x,y, puntic$name[84:87],cex=1)
text(0.8,0.9,"Ruolo nel comune")
text(0.8,0.75,"1=dg, 2=dir, 3=sc, 4=po")

# Anzianita' 1 <=8 2 >8
puntico<-puntic[88:89,c(5,8)]
head(puntico)
x<-as.numeric(puntico[,1])/1000 
y<-as.numeric(puntico[,2])/1000 
plot(x,y,xlim=c(-0.7,1.6),ylim=c(-1,1),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Competenze attese")
abline(h=0, v=0, lty=3)
# text(x,y, row.names(puntico),cex=1)
text(x,y, puntic$name[88:89],cex=1)
text(0.8,0.9,"Anzianità di servizio")
text(0.8,0.75,"1<= 8 anni, 2>8 anni")

# titolo di studio 1=laurea 2 =altro
puntico<-puntic[90:91,c(5,8)]
head(puntico)
x<-as.numeric(puntico[,1])/1000 
y<-as.numeric(puntico[,2])/1000 
plot(x,y,xlim=c(-0.7,1.6),ylim=c(-1,1),cex=1,type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Competenze attese")
abline(h=0, v=0, lty=3)
# text(x,y, row.names(puntico),cex=1)
text(x,y, puntic$name[90:91],cex=1)
text(0.8,0.9,"Titolo di studio")
text(0.8,0.75,"1=laureati, 2=altro")























# comuni erogatori
puntier<-punti[which(datir[,54]>50),c(5,8)]
puntier
x<-as.numeric(puntier[,1])/1000; x
y<-as.numeric(puntier[,2])/1000; y
plot(x,y,xlim=c(-2.5,1),ylim=c(-0.5,2.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore vs competenze attese")
abline(h=0, v=0, lty=3)
# Ingrandimento grafico
plot(x,y,xlim=c(-0.7,0.6),ylim=c(-0.7,0.5),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore vs competenze attese")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntier),cex=1)
























# Suddivisione delle variabili v2-v27 per competenze
#
#
# Saper essere
datise <-datir[,c(6,14,15,16,17,18,19,20,21,22,26,54,55,56,60)]; datise
# Corrispondenze Saper essere
plot(mjca(datise,supcol=c(12:14)),what=c("none","all"))
title(xlab="Competenze saper essere", main="Analisi delle corrispondenze multiple")
summary(mjca(datise,supcol=c(12:14)),what=c("none","all"))
# mappa dei rispondenti
plot(mjca(datise,supcol=c(12:14)),what=c("all","none"))
title(xlab="Competenze saper essere", main="Analisi delle corrispondenze multiple")

# Saper fare
datisf <-datir[,c(7,8,10,11,12,13,23,24,25,27,54,55,56,60)]; datisf
# Corrispondenze Saper fare
plot(mjca(datisf,supcol=c(11:13)),what=c("none","all"))
title(xlab="Competenze saper fare", main="Analisi delle corrispondenze multiple")
summary(mjca(datisf,supcol=c(11:13)),what=c("none","all"))
# mappa dei rispondenti
plot(mjca(datisf,supcol=c(11:13)),what=c("all","none"))
title(xlab="Competenze saper fare", main="Analisi delle corrispondenze multiple")


# Conoscenze
datic <-datir[,c(2,3,4,5,9,54,55,56,60)]; datic
# Corrispondenze Conoscenza
plot(mjca(datic,supcol=c(6:8)),what=c("none","all"))
title(xlab="Competenze conoscenze", main="Analisi delle corrispondenze multiple")
summary(mjca(datic,supcol=c(6:8)),what=c("none","all"))
# mappa dei rispondenti
plot(mjca(datic,supcol=c(6:8)),what=c("all","none"))
title(xlab="Competenze conoscenze", main="Analisi delle corrispondenze multiple")



