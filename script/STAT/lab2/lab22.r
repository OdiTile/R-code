# dati quantitativi continui
setwd(choose.dir())
dir()
dati<-read.table("dati1.txt"); dati
# barplot(dati$w)  # errore !
summary(dati$w)
# intervalli chiusi a destra    ( w ]
faw<-(hist(dati$w, c(40,50,58,70,95),plot=FALSE)); faw
plot(faw)
# funzione di ripartizione
hist(dati$w, breaks="Scott", Main="Scott")
hist(dati$w, breaks=11, Main="11 intervalli")

cl<-c(30,40,50,58,70,95,100); cl
# funzione di ripartizione come somma di
#  frequenze relative
fr<-cumsum(table(cut(dati$w,cl)))/length(dati$w);fr
fr<-c(0,fr); fr
plot(cl, fr, type="b", axes=FALSE)
title(main="Funzione di ripartizione")
axis(2, fr)
axis(1, cl)
box()
# Indici di posizione e di scala
# Entropia di Gini e Shanno per x
frx<-table(dati$x)/length(dati$x); frx
attach(dati)
x
k<-length(frx); k
Gx<-1-sum(frx^2); Gx
Gstarx<-(k/(k-1))*Gx; Gstarx
# Shannon
log(frx)
frx*log(frx)
Hx<-(-sum(frx*log(frx))); Hx
Hstarx<-Hx/log(k); Hstarx
# media e varianza per variabili
# quantitative
n<-length(w); n
mw<-sum(w)/n; mw
vw<-sum((w-mw)^2)/n; vw
cvw<-sqrt(vw)/mw; cvw
# funzioni di R
mean(w)
var(w)
((n-1)/n)*var(w)
detach()
# nuova analisi dati
ls()
rm(list=ls());  ls()
setwd(choose.dir())
dir()
dati<-read.table("pesoalt.txt"); dati
attach(dati)
x
# suddivisione in classi utilizzando
# il ciclo for e if
# xcl 1=pesa poco 2=peso medio 3=pesa molto
xcl<-0
for(i in 1:length(x)){
if(x[i]<=57) xcl[i]<-1
if(x[i]>57 & x[i]<=64) xcl[i]<-2
if(x[i]>64) xcl[i]<-3 }
xcl
# suddivisione in classi
# con vettorializzazione
ycl<-0 ; ycl
ycl[y<=164]<-1 ; ycl
ycl[(y>164) & (y<=170)] <- 2; ycl
ycl[y>170] <-3 ; ycl

# tabella di contingenza
fxy<-table(xcl,ycl); fxy
# marginali come vettori
tr<-as.numeric(margin.table(fxy, 1)); tr
tc<-as.numeric(margin.table(fxy, 2)); tc
# frequenze attese
(tr) %*% t(tc)
fa<-((tr) %*% t(tc))/(sum(tc)); fa
# funzione in R
chisq.test(fxy)$expected
# chi quadro
chi2<-sum((fxy-fa)^2/fa); chi2
chisq.test(fxy)
# V di Cramer
min(length(tr)-1, length(tc)-1)
V<-sqrt(chi2/(sum(tc)*min(length(tr)-1, length(tc)-1))); V














































detach()

