
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
# str(dati)
# dati[18,]
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


round(cumsum(table(datir[,54]))/length(datir[,54]),2)
round(cumsum(table(datir[,55]))/length(datir[,55]),2)
round(cumsum(table(datir[,56]))/length(datir[,56]),2)
round(cumsum(table(datir[,57]))/length(datir[,57]),2)
round(cumsum(table(datir[,58]))/length(datir[,58]),2)
round(cumsum(table(datir[,59]))/length(datir[,59]),2)


# mission attese - osservate
# v65<-dati[,57]-dati[,54]
# v65<-recode(v65,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=TRUE); v65
# v66<-dati[,58]-dati[,55]
# v66<-recode(v66,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=TRUE); v66
# v67<-dati[,59]-dati[,56]
# v67<-recode(v67,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=TRUE); v67
# comuni riclassificati 1=1,2,3 2=4,5
# v68<-recode(dati$v60,"1:3=1; 4:5=2 ",as.factor.result=TRUE); v68
# dataset variabili v2-v27; v54-v56; v60-64 osservate; v65-67 diff mission -> datir5
# 1-26=v2-v27; 27-29=v54-v56; 30-34=v60-v64; 35-37=v65-v66-v67
# datir<-cbind(datir[,2:64],v65,v66,v67,v68); datir[1:10,]
# names(datir)

########################################################################################


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


# Saper essere togliendo variabili poco  esplicative
# datise <-datir[,c(6,14,15,16,17,18,19,20,21,22,26,54,55,56)]; datise
# Corrispondenze Saper essere
# plot(mjca(datise,supcol=c(8:10)),what=c("none","all"))
# title(xlab="Competenze saper essere (ridotte)", main="Analisi delle corrispondenze multiple")
# summary(mjca(datise,supcol=c(8:10)),what=c("none","all"))


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


# Mappature Rispondenti: Erogatore su Saper Essere
#
#
# Saper essere
# datise <-datir[,c(6,14,15,16,17,18,19,20,21,22,26,54,55,56)]; datise
# Corrispondenze Saper essere
# plot(mjca(datise,supcol=c(12:14)),what=c("none","all"))
# title(xlab="Competenze saper essere", main="Analisi delle corrispondenze multiple")
# summary(mjca(datise,supcol=c(12:14)),what=c("none","all"))
# mappa dei rispondenti

# plot( mjca(datise)$rowcoord[datise[,12]==3,1:2],xlim=c(-0.4,1.4),ylim=c(-1.2,0.3))
# punti<-mjca(datise)$rowcoord[datise[,12]==3,1:2]; punti
# plot(punti,)
# summary(mjca(datise,supcol=c(12:14)),what=c("all","all"),rows = T)
# mjca(datise)$colcoord[1:2,]
# out<-mjca(datise,supcol=c(12:14)); out
# summary(out, scree=F, rows = T)
# names(summary(out, scree=F, rows = T))
# out3<-summary(out, scree=F, rows = T)$rows ; out3
# out4<-out3[ c(2,4,5,11,19,20,21,22,28,29,30,31,33,34,46,48,49,50,51,52,53,54,56,58,63,64,
# 66,69,70,78,80,83,84,89,90,91,93,95,99,105,111,114,115,116,117,119,121,122,124,128),c(5,8)]
# xy<-c(0,0)
# plot(xy,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3) )
# plot(out3, add=T)
# str(out4)
# names(out)
# out$coldist
# out2<-plot(mjca(datise,supcol=c(12:14)),what=c("all","none"))
# names(out2)
# summary(out2)
# names(out)
# out$rownames
# out$rowmass
# out$rowdist
# print(mjca(datise,supcol=c(12:14)),what=c("all","none"), rows=TRUE)
# summary(out, rows = TRUE)
# plot(out, map="rowgreen")
# plot(out, map="colprincipal")
# datise[,12]==3
# erogatore<-c(2,4,5,11,19,20,21,22,28,29,30,31,33,34,46,48,49,50,51,52,53,54,56,58,63,64,
# 66,69,70,78,80,83,84,89,90,91,93,95,99,105,111,114,115,116,117,119,121,122,124,128)
# datiseer<-datise[erogatore,]; datiseer
# names(datiseer)
# datiseer2<-cbind(datiseer[,1:14]); datiseer2
# plot(mjca(datise, supcol=c(12:14)),what=c("all","none"))
# plot(mjca(datise[erogatore,],supcol=c(12:14)),what=c("all","none"))
# title(xlab="Competenze saper essere", main="Analisi delle corrispondenze multiple")
#
#Greenacre pp 244-..
#
# datiseb<-mjca(datise)$Burt
# subset<-c(2,4,5,11,19,20,21,22,28,29,30,31,33,34,46,48,49,50,51,52,53,54,56,58,63,64,
# 66,69,70,78,80,83,84,89,90,91,93,95,99,105,111,114,115,116,117,119,121,122,124,128)
# names(mjca(datise))
# summary(mjca(datise))
# str(mjca(datise))



###################################################
# Classifica Erogatore Mediatore Promotore
dati[1:10,54:56]
ClassErMePr<-max.col(dati[,54:56])
ClassErMePr
# trascrizione dei valori trovati per bloccare le scelte casuali
# 1 = Erogatore 2 = Mediatore 3 = Promotore
ClassErMePr<-c( 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 3, 3,
 3, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3,
 1, 1, 3, 3, 3, 1, 3, 3, 1, 3, 1, 1, 3, 1, 1, 1, 1, 3, 1, 3, 1,
 3, 3, 3, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 3, 2, 1, 1, 2, 3, 1, 1,
 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 3, 1, 3, 1, 1, 3, 1, 1, 3,
 3, 3, 3, 3, 1, 1, 3, 3, 3, 3, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 1,
 1, 3, 1, 1, 3, 3, 1, 3, 1, 2, 3, 1, 1 )
which(ClassErMePr==2)
# Mappature Rispondenti: Erogatore su Saper Essere
#
# datise[,12]==3
plot(mjca(datise,supcol=c(12:14)),what=c("all","none"))
title(xlab="Competenze saper essere", main="Analisi delle corrispondenze multiple")
out<-mjca(datise,supcol=c(12:14)); head(out)
punti<-summary(out, scree=F, rows = T)$rows ; head(punti)
# puntier<-punti[ c(2,4,5,11,19,20,21,22,28,29,30,31,33,34,46,48,49,50,51,52,53,54,56,58,63,64,
# 66,69,70,78,80,83,84,89,90,91,93,95,99,105,111,114,115,116,117,119,121,122,124,128),c(5,8)]
# puntier
puntier<-punti[(ClassErMePr==1),c(5,8)]
puntier

x<-as.numeric(puntier[,1])/1000; x
y<-as.numeric(puntier[,2])/1000; y
plot(x,y,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3),cex=0.5,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore su Saper Essere")
abline(h=0, v=0, lty=3)
# text((x+0.01), (y+0.01), row.names(puntier), cex=0.6, pos=4, col="red") 
# text(x,y, row.names(puntier), cex=0.5, pos=4) 
# mappa numeri con area ridotta
plot(x,y,xlim=c(-0.4,0.8),ylim=c(-0.5,0.3),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore su Saper Essere")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntier),cex=1)

# Mappature Rispondenti: Mediatore su Saper Essere
#
datise[,13]==3
# puntime<-punti[ c(1,2,3,6,10,12,13,25,27,32,35,38,41,42,45,47,56,57,61,63,65,72,73,74,
# 76,79,82,85,86,90,96,98,100,104,106,108,109,110,122,123,124,126,131,132),c(5,8)]
# puntime
puntime<-punti[(ClassErMePr==2),c(5,8)]
puntime
x<-as.numeric(puntime[,1])/1000; x
y<-as.numeric(puntime[,2])/1000; y
plot(x,y,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="Mediatore su Saper Essere")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntime),cex=1)

# Mappature Rispondenti: Promotore su Saper Essere
#
datise[,14]==3
# puntipr<-punti[ c(3,7,8,9,14,15,16,17,24,26,34,35,36,37,39,40,41,42,43,44,45,46,47,48,50,
# 55,57,59,60,61,67,71,72,75,77,81,83,87,88,92,93,94,95,97,99,101,102,103,107,109,112,113,
# 118,126,127,129,130,132,133),c(5,8)]
# puntipr
puntipr<-punti[(ClassErMePr==3),c(5,8)]
puntipr
x<-as.numeric(puntipr[,1])/1000; x
y<-as.numeric(puntipr[,2])/1000; y
plot(x,y,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="Promotore su Saper Essere")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipr),cex=1)


# Mappature Rispondenti: Comuni > 15.000 ab. su Saper Essere
#
datise[datise[,15]>4,]
puntigc<-punti[ c(10,11,13,19,32,35,42,52,58,59,61,76,79,82,84,88,92,103,113,133),c(5,8)]
puntigc
x<-as.numeric(puntigc[,1])/1000; x
y<-as.numeric(puntigc[,2])/1000; y
plot(x,y,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3),xlab="Asse principale",ylab="Asse secondario")
title(main="> 15.000 ab. su Saper Essere")
abline(h=0, v=0, lty=3)
plot(x,y,xlim=c(-0.4,0.5),ylim=c(-0.5,0.3),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="> 15.000 ab. su Saper Essere")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntigc),cex=1)


# Mappature Rispondenti: Comuni < 5.000 ab. su Saper Essere
#
datise[datise[,15]<3,]
puntipc<-punti[ c(1,4,6,17,22,23,24,25,28,33,38,43,45,46,48,51,53,54,55,66,68,69,72,74,75,77,
78,80,81,85,86,90,98,99,100,101,102,104,105,106,108,110,111,112,116,118,119,121,126,128,
130,131,134),c(5,8)]
puntipc
x<-as.numeric(puntipc[,1])/1000; x
y<-as.numeric(puntipc[,2])/1000; y
plot(x,y,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3),xlab="Asse principale",ylab="Asse secondario")
title(main="< 5.000 ab. su Saper Essere")
abline(h=0, v=0, lty=3)
plot(x,y,xlim=c(-0.4,0.8),ylim=c(-0.5,0.3),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="< 5.000 ab. su Saper Essere")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipc),cex=1)

################################################################################################
# Saper fare
datisf <-datir[,c(7,8,10,11,12,13,23,24,25,27,54,55,56,60)]; datisf
# Corrispondenze Saper fare
plot(mjca(datisf,supcol=c(11:13)),what=c("none","all"))
title(xlab="Competenze saper fare", main="Analisi delle corrispondenze multiple")
summary(mjca(datisf,supcol=c(11:13)),what=c("none","all"))
# mappa dei rispondenti
plot(mjca(datisf,supcol=c(11:13)),what=c("all","none"))
title(xlab="Competenze saper fare", main="Analisi delle corrispondenze multiple")
# Classifica Erogatore Mediatore Promotore
# trascrizione dei valori trovati per bloccare le scelte casuali
# 1 = Erogatore 2 = Mediatore 3 = Promotore
ClassErMePr<-c( 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 3, 3,
 3, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3,
 1, 1, 3, 3, 3, 1, 3, 3, 1, 3, 1, 1, 3, 1, 1, 1, 1, 3, 1, 3, 1,
 3, 3, 3, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 3, 2, 1, 1, 2, 3, 1, 1,
 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 3, 1, 3, 1, 1, 3, 1, 1, 3,
 3, 3, 3, 3, 1, 1, 3, 3, 3, 3, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 1,
 1, 3, 1, 1, 3, 3, 1, 3, 1, 2, 3, 1, 1 )
# Mappature Rispondenti: Erogatore su Saper Fare
out<-mjca(datisf,supcol=c(12:14)); head(out)
punti<-summary(out, scree=F, rows = T)$rows ; head(punti)
puntier<-punti[(ClassErMePr==1),c(5,8)]
puntier
x<-as.numeric(puntier[,1])/1000; x
y<-as.numeric(puntier[,2])/1000; y

plot(x,y,xlim=c(-0.6,2),ylim=c(-1.2,1.5),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore su Saper Fare")
abline(h=0, v=0, lty=3)

plot(x,y,xlim=c(-0.4,2),ylim=c(-0.5,1.5),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore su Saper Fare")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntier),cex=1)

# Mappature Rispondenti: Mediatore su Saper Fare
puntime<-punti[(ClassErMePr==2),c(5,8)]
puntime
x<-as.numeric(puntime[,1])/1000; x
y<-as.numeric(puntime[,2])/1000; y
plot(x,y,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="Mediatore su Saper Fare")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntime),cex=1)

# Mappature Rispondenti: Promotore su Saper Fare
puntipr<-punti[(ClassErMePr==3),c(5,8)]
puntipr
x<-as.numeric(puntipr[,1])/1000; x
y<-as.numeric(puntipr[,2])/1000; y
plot(x,y,xlim=c(-0.4,2),ylim=c(-0.5,0.8),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="Promotore su Saper Fare")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipr),cex=1)


# Mappature Rispondenti: Comuni > 15.000 ab. su Saper Fare
#
puntigc<-punti[ c(10,11,13,19,32,35,42,52,58,59,61,76,79,82,84,88,92,103,113,133),c(5,8)]
puntigc
x<-as.numeric(puntigc[,1])/1000; x
y<-as.numeric(puntigc[,2])/1000; y
plot(x,y,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3),xlab="Asse principale",ylab="Asse secondario")
title(main="> 15.000 ab. su Saper Fare")
abline(h=0, v=0, lty=3)
plot(x,y,xlim=c(-0.4,2),ylim=c(-0.5,1.5),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="> 15.000 ab. su Saper Fare")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntigc),cex=1)


# Mappature Rispondenti: Comuni < 5.000 ab. su Saper Fare
#
puntipc<-punti[ c(1,4,6,17,22,23,24,25,28,33,38,43,45,46,48,51,53,54,55,66,68,69,72,74,75,77,
78,80,81,85,86,90,98,99,100,101,102,104,105,106,108,110,111,112,116,118,119,121,126,128,
130,131,134),c(5,8)]
puntipc
x<-as.numeric(puntipc[,1])/1000; x
y<-as.numeric(puntipc[,2])/1000; y
plot(x,y,xlim=c(-0.4,0.5),ylim=c(-0.5,0.3),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="< 5.000 ab. su Saper Fare")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipc),cex=1)

################################################################################################
# Conoscenze
datic <-datir[,c(2,3,4,5,9,54,55,56,60)]; datic
# Corrispondenze Conoscenza
plot(mjca(datic,supcol=c(6:8)),what=c("none","all"))
title(xlab="Competenze conoscenze", main="Analisi delle corrispondenze multiple")
summary(mjca(datic,supcol=c(6:8)),what=c("none","all"))
# mappa dei rispondenti
plot(mjca(datic,supcol=c(6:8)),what=c("all","none"))
title(xlab="Competenze conoscenze", main="Analisi delle corrispondenze multiple")

# Classifica Erogatore Mediatore Promotore
# trascrizione dei valori trovati per bloccare le scelte casuali
# 1 = Erogatore 2 = Mediatore 3 = Promotore
ClassErMePr<-c( 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 3, 3,
 3, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3,
 1, 1, 3, 3, 3, 1, 3, 3, 1, 3, 1, 1, 3, 1, 1, 1, 1, 3, 1, 3, 1,
 3, 3, 3, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 3, 2, 1, 1, 2, 3, 1, 1,
 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 3, 1, 3, 1, 1, 3, 1, 1, 3,
 3, 3, 3, 3, 1, 1, 3, 3, 3, 3, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 1,
 1, 3, 1, 1, 3, 3, 1, 3, 1, 2, 3, 1, 1 )
# Mappature Rispondenti: Erogatore su Conoscenze
out<-mjca(datic,supcol=c(6:8)); head(out)
punti<-summary(out, scree=F, rows = T)$rows ; head(punti)
puntier<-punti[(ClassErMePr==1),c(5,8)]
puntier
x<-as.numeric(puntier[,1])/1000; x
y<-as.numeric(puntier[,2])/1000; y

plot(x,y,xlim=c(-1,1),ylim=c(-1,1),cex=1,type="p",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore su Conoscenze")
abline(h=0, v=0, lty=3)

# Ingrandimento grafico
plot(x,y,xlim=c(-0.7,0.5),ylim=c(-0.2,0.4),type="n",
xlab="Asse principale",ylab="Asse secondario")
title(main="Erogatore su Conoscenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntier),cex=1)

# Mappature Rispondenti: Mediatore su Conoscenze
puntime<-punti[(ClassErMePr==2),c(5,8)]
puntime
x<-as.numeric(puntime[,1])/1000; x
y<-as.numeric(puntime[,2])/1000; y
plot(x,y,xlim=c(-0.3,0.3),ylim=c(-0.12,0.05),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="Mediatore su Conoscenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntime),cex=1)

# Mappature Rispondenti: Promotore su Conoscenze
puntipr<-punti[(ClassErMePr==3),c(5,8)]
puntipr
x<-as.numeric(puntipr[,1])/1000; x
y<-as.numeric(puntipr[,2])/1000; y

plot(x,y,xlim=c(-1,1),ylim=c(-1,1),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="Promotore su Conoscenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipr),cex=1)

plot(x,y,xlim=c(-0.8,0.5),ylim=c(-0.2,0.5),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="Promotore su Conoscenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipr),cex=1)


# Mappature Rispondenti: Comuni > 15.000 ab. su Conoscenze
#
puntigc<-punti[ c(10,11,13,19,32,35,42,52,58,59,61,76,79,82,84,88,92,103,113,133),c(5,8)]
puntigc
x<-as.numeric(puntigc[,1])/1000; x
y<-as.numeric(puntigc[,2])/1000; y
plot(x,y,xlim=c(-0.4,1.4),ylim=c(-1.2,0.3),xlab="Asse principale",ylab="Asse secondario")
title(main="> 15.000 ab. su Conoscenze")
abline(h=0, v=0, lty=3)
plot(x,y,xlim=c(-0.7,0.3),ylim=c(-0.2,0.4),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="> 15.000 ab. su Conoscenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntigc),cex=1)


# Mappature Rispondenti: Comuni < 5.000 ab. su Conoscenze
#
puntipc<-punti[ c(1,4,6,17,22,23,24,25,28,33,38,43,45,46,48,51,53,54,55,66,68,69,72,74,75,77,
78,80,81,85,86,90,98,99,100,101,102,104,105,106,108,110,111,112,116,118,119,121,126,128,
130,131,134),c(5,8)]
puntipc
x<-as.numeric(puntipc[,1])/1000; x
y<-as.numeric(puntipc[,2])/1000; y
plot(x,y,xlim=c(-0.5,0.5),ylim=c(-0.2,0.2),type="n",xlab="Asse principale",ylab="Asse secondario")
title(main="< 5.000 ab. su Conoscenze")
abline(h=0, v=0, lty=3)
text(x,y, row.names(puntipc),cex=1)










# plot( mjca(datise)$rowcoord[datise[,12]==3,1:2],xlim=c(-0.4,1.4),ylim=c(-1.2,0.3))
# plot(mjca(datise,supcol=c(12:14)),what=c("all","none"))
# title(xlab="Competenze saper essere", main="Analisi delle corrispondenze multiple")

# prove clustering
# crawley p. 742
# competenze: saper essere
hccomp<-hclust(dist(datir[,c(6,14,15,16,17,18,19,20,21,22,26)]))
plot(hccomp,main="Competenze: Saper Essere", xlab="Dirigenti PA")
# rect.hclust(hccomp, k=3, border="red")

# competenze: saper fare
hccomp<-hclust(dist(datir[,c(7,8,10,11,12,13,23,24,25,27)]))
plot(hccomp,main="Competenze: Saper Fare", xlab="Dirigenti PA")
# rect.hclust(hccomp, k=3, border="red")

# competenze: conoscenze
hccomp<-hclust(dist(datir[,c(2,3,4,5,9)]))
plot(hccomp,main="Competenze: Conoscenze", xlab="Dirigenti PA")
# rect.hclust(hccomp, k=3, border="red")

# cluster per mission e dimensione del comune
# riclassificazione comune
# 1,2,3=1   4,5=2
datir[,60]<-recode(dati$v60,"1:3=1; 4:5=2 ",as.factor.result=TRUE)
# datir[,60]
hccomp<-hclust(dist(datir[,c(54,55,56,68)]))
plot(hccomp,main="Mission e dimensione del comune", xlab="Dirigenti PA")
rect.hclust(hccomp, k=3, border="red")

# datir[,c(54,55,56,60)]
# head(datir)
# names(datir)
# datir[(datir[,54]==3),c(54,55)]

# cluster per comune erogatore
hccomp<-hclust(dist(datir[(datir[,54]==3),]))
plot(hccomp,main="Comune erogatore", xlab="Dirigenti PA")
rect.hclust(hccomp, k=3, border="red")

# cluster per comune mediatore
hccomp<-hclust(dist(datir[(datir[,55]==3),]))
plot(hccomp,main="Comune mediatore", xlab="Dirigenti PA")
rect.hclust(hccomp, k=3, border="red")

# cluster per comune promotore
hccomp<-hclust(dist(datir[(datir[,56]==3),]))
plot(hccomp,main="Comune promotore", xlab="Dirigenti PA")
rect.hclust(hccomp, k=3, border="red")





#
# Vecchie elaborazioni
#
#
#
#
#
#
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



