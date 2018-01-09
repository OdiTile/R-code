# file elaborazioni.r
#
#
# getwd()
# backslash \\\\\   in R uso / oppure \\
# setwd("C:/Users/mario.bonamin/Desktop/DatiSSIMP")
# getwd()
# dir()
# choose.files()
# partenza con sistema pulito
rm(list=ls())
ls()
setwd("C:/Users/mario.bonamin/Desktop/DatiSSIMP")
dir()
# lettura file a formato fisso da rete
# utilizzare protocollo http
# 
dati1<-read.fwf(file="http://sites.google.com/site/paginadati/dati/babyboom.txt", width=c(8,8,8,8))
dati1
#
dati<-read.fwf(file="babyboom.txt",width=c(8,8,8,8))
dati
names(dati)<-c("time", "sex","weight","minute"); names(dati)
dati$minute
tapply(dati$weight, dati$sex, mean)
dati[,4]
tapply(dati[,3],dati[,2], mean)
# ora di nascita
dati[,4]/60
ora<-floor(dati[,4]/60); ora
head(dati)
dati<-cbind(dati,ora)
head(dati)
tab<-table(dati$sex,dati$ora); tab
mc<-apply(tab,2,sum); mc
mr<-apply(tab,1,sum); mr
# grafico a barre 
barplot(table(dati$ora))
#uso del colore
barplot(table(dati$ora),col=c(1,2,3,4,5,6,7,8))
barplot(table(dati$ora), col=heat.colors(24))
barplot(table(dati$ora), col=topo.colors(24))
hist(dati$weight)
hist(dati$weight, breaks=15, col=heat.colors(10))
boxplot(dati$weight, col=2)
boxplot(dati$weight~dati$sex, col=c(2,4))ù
#
# nuova sessione
# lettura di file csv
rm(list=ls())
ls()
# res<-read.table("ResCapizzi2016.csv", header=TRUE, sep=",", skip=2)
res<-read.table("ResCapizzi2016.csv", header=F, sep=",", skip=3)
head(res)
var<-c("eta","m","f","tot"); var  # vettore con i nomi delle variabili
str(res)
dimnames(res)
dimnames(res)[[2]]<-var
dimnames(res)[[2]]
res
res[100:102,]
res<-res[-102,]; tail(res)
res<-cbind(0:100,res$m,res$f,res$tot); head(res)
tail(res)
dimnames(res)[[2]]<-var
dimnames(res)[[2]]
str(res)
res<-as.data.frame(res)
str(res)
# calcolo del rapporto di genere
tm<-sum(res$m); tm
tf<-sum(res$f); tf
rg<-tm/tf; rg   # 0.9497 ogni 100 femmine ci sono ~95 maschi
# indice di vecchiaia
sum(res$tot[66:101])
iv<-(sum(res$tot[66:101])/sum(res$tot[1:15]))*100; iv
iv<-round(iv,2); iv
# proporzione di anziani sulla popolazione
iv1<-(sum(res$tot[66:101])/sum(res$tot))*100; iv1
# rapporto di genere per età
plot(res$eta, res$m/res$f)
# abline(b,a)
abline(h=1)
#
# nuova sessione
#
rm(list=ls())
ls()
setwd("C:/Users/mario.bonamin/Desktop/DatiSSIMP")
dir()
res<-read.table("ResCapizzi2016.csv", header=F, sep=",", skip=3)
head(res)
var<-c("eta","m","f","tot"); var  # vettore con i nomi delle variabili

res<-res[-102,]; tail(res)
dimnames(res)[[2]]<-var
res<-cbind(0:100,res$m,res$f,res$tot)
head(res)
dimnames(res)[[2]]<-var
head(res)
res<-as.data.frame(res)
head(res)
# età media della poplazione
# ((x+0.5)*Px)/P
res$eta
# res[,1]
etam<-sum(((res$eta+0.5)*res$tot))/sum(res$tot)
etam
#
# Lettura dati excel
# analisi dati peso - altezza
#
library(readxl)
pa<-read_excel("PesoAlt.xlsx", sheet="Dati")
pa
summary(pa$Altezza)
x<-cut(pa$Altezza, c(150, 167, 175, 190), labels=F)
x
summary(pa$Peso)
y<-cut(pa$Peso, c(40,58,66,105), labels=F)
y
# tabella di contingenza o tabella bivariata
fxy<-table(x,y)
fxy
# marginali come vettori
tr<-as.numeric(margin.table(fxy,1)); tr
tc<-as.numeric(margin.table(fxy,2)); tc
tot<-sum(tr); tot
(tr)%*%t(tc)
fa<-((tr)%*%t(tc))/tot
fa
# calcolo del valore chi quadro
# somma(((fxy-fa)^2)/fa)
chi2<-sum(((fxy-fa)^2)/fa)
chi2
# V cramer V=radq(chi2/(n*min(i-1;j-1))
V<-sqrt(chi2/(tot*2))
V
# funzione di R
chisq.test(fxy)
#
# indipendenza lineare
#
pa
x<-pa$Altezza; x
y<-pa$Peso; y
# varianza della popolazione
varpx<-(sum((x-mean(x))^2))/length(x)
varpx
var(x)
(sum((x-mean(x))^2))/(length(x)-1)
varpy<-(sum((y-mean(y))^2))/length(y)
varpy
covxy<-sum((x-mean(x))*(y-mean(y)))/length(x)
covxy
# calcolo della correlazione di Pearson
corxy= covxy/sqrt(varpx*varpy)
corxy
# funzione correlazione di R
cor(x,y)
#
# regressione lineare
# grafico a dispersione
plot(x,y)
plot(x,y, main="Diagramma altezza - peso", xlab="Altezza in cm", ylab="Peso in kg")
# stima dei parametri
# pendenza
b<-covxy/varpx
b
# intercetta
a<-mean(y)-b*mean(x)
a
# funzione di R - funzione lm()
mod<-lm(y~x)
str(mod)
mod$coefficients[[2]]
mod$coefficients[[1]]
plot(x,y, main="Diagramma altezza - peso", xlab="Altezza in cm", ylab="Peso in kg")
abline(mod, col=2, lty=2)
text(155,95,"y = -171 + 1.39 * x")
# analisi del rischio
# tabella tetracorica
#
ds<-read_excel("DatasetScreening.xls", sheet="DatasetCompleto")
head(ds)
B00<-ds$B00;B00

summary(ds$B01)
ds$B01>58
(ds$B01>58)*1
B01<-(ds$B01>58)*1
B01
tab<-table(B01,B00)
tab
a<-tab[1,2]; a
b<-tab[1,1]; b
c<-tab[2,2]; c
d<-tab[2,1]; d
RT<-(a+c)/(a+b+c+d); RT
RE<-a/(a+b); RE
RNE<-c/(c+d); RNE
RR<-RE/RNE; RR
OR<-(a*d)/(c*b); OR
RA<-(RE-RNE); RA
# inversione di una variabile
head(B01)
head(1-B01)
# prove 
# codifica di B03
ds$B03
table(ds$B03)
B03<-(ds$B03>1)*1
B03



















