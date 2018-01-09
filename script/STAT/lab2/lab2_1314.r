# lezione 2
# file lab2_1314.3
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
hist(dati$w, breaks="Scott", main="Scott")
hist(dati$w, breaks=11, main="11 intervalli")

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
# =================================================
# nuova analisi dati
ls()
rm(list=ls());  ls()
setwd(choose.dir())
dir()
#setwd("C:/dati/lav/corsi/stat/Statistica1/Stat1B1213/lab2")
#setwd("H:/Statistica1/Stat1B1213/lab2")
# peso x
x<-c(64,54,65,42,58,50,58,65,78,52,48,66,58,103,63,70,65,53,62,58)
x; length(x)
# altezza y
y<-c(168,166,162,151,168,170,167,172,175,165,158,173,
     163,185,165,170,175,158,160,163); y; length(y)
dati<-data.frame(x,y); dati
write.table(dati, file="pesoalt.txt")
#
# inizio elaborazioni
#
rm(list=ls()); ls()
dati<-read.table("pesoalt.txt"); dati
names(dati)
names(dati)<-c("peso","altezza"); dati
names(dati)<-c("x","y"); dati
attach(dati)       # rende direttamente disponibili le variabili
# suddivisione in classi
# fax<-hist(x,c(40,57,64,105),plot=FALSE);fax
# alternativamente
# summary(x)
# fax<-table(cut(x, breaks=c(40,57,64,105))); fax
# summary(y)
# fay<-table(cut(y, breaks=c(150,164,170,190))); fay
# fxy<-table(fay,fax); fxy   # non funziona
#
# suddivisione in classi con ciclo if  ###########################
xcl<-0                             # inizializza il vettore risultato
for(i in 1:length(x)){             # parentesi graffa con Alt - 126
if(x[i]<=57) xcl[i]<-1
if(x[i]>57 & x[i]<=64) xcl[i]<-2
if(x[i]>64) xcl[i]<-3}
xcl  #  2 1 3 1 2 1 2 3 3 1 1 3 2 3 2 3 3 1 2 2
#
ycl<-0
for(i in 1:length(y)){
if(y[i]<=164) ycl[i]<-1
if(y[i]>164 & y[i]<=170) ycl[i]<-2
if(y[i]>170) ycl[i]<-3}
ycl  #  2 2 1 1 2 2 2 3 3 2 1 3 1 3 2 2 3 1 1 1
# tabella di contingenza
fxy<-table(xcl,ycl); fxy

# suddivisione in classi con vettorializzazione  ##################
xcl<-0                     # vettore risultato di lunghezza 1
xcl[x <= 57] <- 1  ; xcl
xcl[(x > 57) & (x<=64)] <- 2  ; xcl
xcl[x > 64] <- 3  ; xcl
# xcl
#
ycl<-0; ycl
ycl[y <= 164] <- 1 ;ycl
ycl[(y > 164) & (y<=170)] <- 2; ycl
ycl[y > 170] <- 3; ycl
# ycl
#
# suddivisione in classi con cut()  ##############################
summary(x)
xcl<-cut(x,c(40,57,64,105), labels=F); xcl
summary(y)
ycl<-cut(y,c(150,164,170,190), labels=F); ycl
# tabella di contingenza
fxy<-table(xcl,ycl); fxy  # 3 3 0 / 3 4 0 / 1 1 5

# dataframe completo
dati1<-data.frame(x,xcl,y,ycl); dati1
detach(dati)             # termina la disponibilita' diretta
write.table(dati1, file="pesoalt1.txt")
#
# inizio elaborazioni
#
rm(list=ls()); ls()
dati1<-read.table("pesoalt1.txt"); dati1
attach(dati1)
#detach(dati1)
#
# tabella di contingenza
fxy<-table(xcl,ycl); fxy                 # frequenze osservate
# marginali come vettori
tr<-as.numeric(margin.table(fxy,1)); tr  #  6 7 7 totale di riga
tc<-as.numeric(margin.table(fxy,2)); tc  #  7 8 5 totale di colonna
# calcolo delle frequenze attese
(tr)%*%t(tc)                             # prodotto vettoriale
# 42 48 30 / 49 56 35 / 49 56 35 
fa<-((tr)%*%t(tc))/(sum(tc)); fa         # frequenze attese
# 2.10 2.4 1.50 / 2.45 2.8 1.75 / 2.45 2.8 1.75
# library(stats)
chisq.test(fxy)$expected                 # ancora frequenze attese
# calcolo del chi quadro
chi2<-sum((fxy-fa)^2/fa) ; chi2          # chi quadro = 12.47449
chisq.test(fxy)
#
# V di cramer
min(length(tr)-1,length(tc)-1)
sum(tc)
V<- sqrt(chi2/(sum(tc)*min(length(tr)-1,length(tc)-1))); V # 0.5584463
#
#  da verificare  =======================================================
#
# analisi della varianza
# media dei pesi per altezza 1,2,3
detach(dati1)
#
# mg<-tapply(dati1$x, dati1$ycl,mean); mg
b<-(dati1$x[dati1$ycl==1]);b  # 65 42 48 58 53 62 58 peso dei bassi
m<-(dati1$x[dati1$ycl==2]);m  # 64 54 58 50 58 52 63 70 peso dei medi
a<-(dati1$x[dati1$ycl==3]);a  # 65 78 66 103 65 peso degli alti
nb<-length(b); nb             # 7 numero dei bassi
nm<-length(m); nm             # 8 numero dei medi
na<-length(a); na             # 5 numero degli alti
n<-length(dati1$x); n         # 20 numero totale
mb<-sum(b)/nb; mb             # 55.14286 media dei bassi
mm<-sum(m)/nm; mm             # 58.625 media dei medi
ma<-sum(a)/na; ma             # 75.4 media degli alti
mt<-sum(dati1$x)/n; mt        # 61.6 media totale
# varianze  
vt<-sum((dati1$x-mt)^2)/n; vt # 154.74 varianza totale
# varianza tra gruppi
vb<-(((mb-mt)^2)*nb+((mm-mt)^2)*nm+((ma-mt)^2)*na)/n; vb #65.74339
# varianza interna ai gruppi o residua
vw<-(sum((b-mb)^2)+sum((m-mm)^2)+sum((a-ma)^2))/n; vw  # 88.99661
vt; vb+vw     # 154.74 = 65.74339 + 88.99661 scomposizione della varianza
# eta quadro
eta2<-vb/vt; eta2
boxplot(b,m,a)
title(main="Analisi della varianza",xlab=("Gruppi"), ylab="Peso in kg")
#devianze
dt<-vt*n; dt                 # 3094.8 devianza totale
db<-vb*n; db                 # 1314.868 devianza between
dw<-vw*n; dw                 # 1779.932 devianza within
dt; db+dw   # 3094.8 = 1314.868 + 1779.932 scomposizione della devianza
# funzione di R
mod<-aov(dati1$x~factor(dati1$ycl))
summary(mod)
anova(mod)
# Ieva - Paganoni - Vitelli
dati1
n<-table(dati1$ycl); n      # osservazione ogni gruppo
N<-sum(n); N                # dimensione totale
mg<-mean(dati1$x); mg       # media generale
mog<-tapply(dati1$x,dati1$ycl,mean); mog  # media ogni gruppo
devb<-sum(n*(mog-mg)^2); devb   # devianza between
devw<-sum((n-1)*tapply(dati1$x,dati1$ycl,var)); devw  # devianza within
devt<-(N-1)*var(dati1$x); devt; devb+ devw
summary(aov(dati1$x~factor(dati1$ycl)))

