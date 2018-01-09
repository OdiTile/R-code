# file Importa.R
# importazioni vari tipi di dati
# setwd(choose.dir())
# dir()
# getwd()
setwd("C:/Users/mario.bonamin/Desktop/Dati")
# file a formato fisso
ls()
rm(list=ls())
ls()
dir()
dati<-read.fwf(file="babyboom.txt",width=c(8,8,8,8))
ls()
dati
names(dati)
names(dati)<-c("time","sex","weight","minute")
names(dati)
head(dati)
# vettori
x<-c("red","blu", "yellow") ; x
str(x)
x<-c(1:10); str(x)
str(dati)
tapply(dati$weight,dati$sex,mean)
tapply(dati$weight,dati$sex,length)
# ora di nascita
dati$minute
# floor()  trunc()
ora<-floor(dati$minute/60); ora
table(ora)
dati<-cbind(dati,ora); head(dati)
str(dati)
tab<-table(dati$sex,dati$ora); tab
mr<-apply(tab,1,sum); mr
mc<-apply(tab,2,sum); mc
str(mr)
vmr<-as.vector(mr); vmr; str(vmr)
vnc<-as.vector(mc); vnc
# grafico a barre
barplot(table(dati$ora))
barplot(table(dati$ora), col=c(2,3,4,5,6))
barplot(table(dati$ora), col=c(1,2))
barplot(table(dati$ora), col=heat.colors(15))
barplot(table(dati$ora), col=terrain.colors(20))
title(main="Distribuzione dell'ora di nascita", 
  xlab="ora di nascita", ylab="Numero di nati")
# riprendiamo da qui
# istogramma
hist(dati$weight)
hist(dati$weight, breaks=10, col=6)
hist(dati$weight, breaks=10, col=terrain.colors(10))
#
boxplot(dati$weight)
boxplot(dati$weight~dati$sex, col=2)
# lettura di dati csv
ls()
rm(list=ls())
ls()
dir()
#res<-read.table("PopResPianiga2015.csv",header=TRUE, 
#     sep=",",skip=2)
res<-read.table("PopResPianiga2015.csv",header=F, 
     sep=",",skip=3); head(res)
var<-c("eta","m","f","tot"); var
dimnames(res)
dimnames(res)[[2]]<-var
dimnames(res)[2]
head(res)
length(res$eta)
res[100:102 , ]
str(res)
# res$m[100]<-2
# res$f[100]<-5
# res$tot[100]<-7
# res[100:102 , ]
res<-res[-102,] ;res
res<-cbind(0:100, res$m, res$f, res$tot); res
str(res)
dimnames(res)[[2]]<-var; head(res)
res<-as.data.frame(res); head(res)
str(res)
write.table(res, file="respianiga.txt")
dir()
rm(list=ls())
ls()
res<-read.table("respianiga.txt"); head(res)
# rapporto di genere
tm<-sum(res$m); tm
tf<-sum(res$f); tf
rg<-round((tm/tf)*100)
rg
# indice di vecchiaia
iv<-(sum(res$tot[66:101]))/sum(res$tot[1:15])*100 ;iv
# rapporto di composizione per eta' 65+
rce<-((sum(res$tot[66:101]))/sum(res$tot))*100; rce
# età media
em<-(res$eta+0.5)*res$tot; em
em<-sum(em)/sum(res$tot); em
# dati di mortalita' Femmine VE 2014
dir()
tm<-read.table("Tavole_mortalita_-_Provincia__Venezia_-_Femmine_-_Anno_2014.csv",
    header=T, sep=",", skip=1); head(tm)
# importazione da clipboard
ls()
rm(list=ls()); ls()
tm<-read.delim2(file="clipboard", header=T); head(tm)
library()
library(readxl)
rm(list=ls()); ls()
tm<-read_excel("Esercizi1516.xlsx", sheet="TavMortVEF2014"); head(tm)
# nomi variabili
var<-c("x", "lx","dx","qx","Lx","px","ex"); var
dimnames(tm)[[2]]<-var; head(tm)
# ricostruzione tavola
q2x<-tm$qx[1:101]; q2x
q2x[101]<-1000; q2x
l2x<-tm$lx[1:101]
for (i in 2:(length(q2x)))
     {l2x[i]<-(l2x[i-1])-((l2x[i-1])*q2x[i-1])/1000} ; l2x
d2x<-round(l2x*q2x/1000);d2x
tm$lx
# anni di vita vissuti
L2x<-tm$Lx[1:101]
for (i in 1:(length(L2x)-1)) {
     L2x[i]<-(l2x[i]+l2x[i+1])/2}
L2x[101]<-l2x[101]/2 ; L2x
# retrocumulata anni di vita vissuti
T2x<-0.0
T2x[101]<-L2x[101]; T2x
for( i in (length(L2x)-1):1)
    {T2x[i]<-L2x[i]+T2x[i+1]}; T2x
# speranza d vita attesa
e2x<-T2x/l2x; e2x
tm$ex

tm$qx
q2x
cbind(l2x, round(l2x*q2x/1000))
# ====================================================
# associazione e dipendenza lineare
#
rm(list=ls()); ls()
setwd("C:/Users/mario.bonamin/Desktop/Dati")
dir()
# inserimento diretto dei dati
# altezza x
x<-c(168,166,162,151,168,
     170,167,172,175,165,
     158,173,163,185,165,
     170,175,158,160,163); x ; length(x)
# peso y
y<-c(64,54,65,42,58,
     50,58,65,78,52,
     48,66,58,103,63,
     70,65,53,62,58); y; length(y)
# costruzione della matrice di dati ( data frame)
dati1<-cbind(x,y); dati1
(dati<-data.frame(x,y))
str(dati1)
str(dati)
write.table(dati, file="pesoalt.txt")
#
ls()
rm(list=ls()); ls()
setwd("C:/Users/mario.bonamin/Desktop/Dati")
dir()
# importazione
dati<-read.table("pesoalt.txt"); dati
ls()
attach(dati)  # rende disponibili le variabili in dati
ls(); x; y
# suddivisione del peso e dell'altezza in classi
#
# classi altezza 1 (x<=164); 2 (164<x<=170); 3 (x>170)
xcl<-0 # inizializzazione vettore risultato
# which(x<=164)
# (x<=164)*1
xcl[x<=164]<-1 ; xcl
xcl[(x>164) & (x <= 170)] <-2; xcl
xcl[x>170]<-3; xcl
#
# suddivisione del peso con cut()
# classi 1(y<=57); 2(57<Y<=64) 3(y>64)
summary(y)
ycl<-cut(y,c(40,57,64,110),labels=F); ycl
# ycl2<-cut(y,c(57,64),labels=T); ycl2
# tabella di contingenza
fxy<-table(xcl,ycl); fxy
chisq.test(fxy)
str(chisq.test(fxy))
fa<-chisq.test(fxy)$expected; fa
# chi quadro
chi2<-sum((fxy-fa)^2/fa);chi2
V<- sqrt(chi2/(sum(fxy)*2)); V
#
# Dipendenza lineare
# regressione lineare bivariata
plot(x,y)
plot(x,y, main="Diagramma altezza-peso",
     xlab="altezza in cm.", ylab="peso in kg.", col=2)
# y=a+b*x
# b=sxy/s2x   a=my-b*mx
# y=a+b*x+e
mod<-lm(y~x); mod
str(mod)
abline(mod)
points(185,95, cex=2)
text(155,100,"y = a + b * x + e")
text(155,95, "-171 + 1.39 * x + e")
points(mean(x), mean(y), cex=4, pch=3)  
detach()
#
# analisi di dati epidemiologici
#
rm(list=ls()); ls()
setwd("C:/Users/mario.bonamin/Desktop/Dati")
dir()
library(readxl)
dati<-read_excel("DatasetScreening.xls",
                 sheet="DatasetCompleto"); head(dati)
# B00 caso=1 controllo=0
dati$B00
# B01 eta' in anni compiuti
dati$B01
summary(dati$B01)
library(epiDisplay)
B00<-dati$B00
B01<-(dati$B01<58)*1; B01
table(B01,B00)
tabpct(B01,B00)
# fattore di rischio eta' > 57 anni
cc(B00,B01, design="case-control")
tab<-table(B01,B00); tab
a<-tab[1,1]; a
b<-tab[1,2]; b
c<-tab[2,1]; c
d<-tab[2,2]; d
# indicatori epidemiologici

RT= (a+b)/(a+b+c+d); RT
RE= a/(a+b); RE
RNE= c/(c+d); RNE
RR=RE/RNE; RR
RA=RE-RNE ; RA
NNT=1/RA; NNT


# controllo
OR=(a*d)/(b*c); OR


ls()
dati$B02
B02
B01
attach(dati)
B01
B02
detach(dati)
B02



































































