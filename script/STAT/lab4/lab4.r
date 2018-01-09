# lezione 4
# file lab4.r
# dim car 12
# covarianza e correlazione     test sulla correlazione
# modello diregressione lineare
# analisi della varianza per il modello di regressione
#
# seleziona cartella di lavoro
rm(list=ls()); ls()
setwd(choose.dir())
getwd()
dir()
#setwd("C:/dati/lav/corsi/stat/Statistica1/Stat1B1213/lab2")
#setwd("H:/Statistica1/Stat1B1213/lab2")
#
dati<-read.table("pesoaltgen.txt"); dati
names(dati)
names(dati)<-c("peso","altezza", "genere"); dati
names(dati)<-c("x","y","g"); dati
#
#
# covarianza e correlazione x=peso y=altezza
n<-length(dati$x); n
mx<-sum(dati$x)/n; mx
my<-sum(dati$y)/n; my
vx<-sum((dati$x-mx)^2)/n; vx
vy<-sum((dati$y-my)^2)/n; vy
cxy<-sum((dati$x-mx)*(dati$y-my))/n; cxy
cov(dati$x,dati$y)*(n-1)/n  # covarianza campionaria vs cov popolazione
corrxy<-cxy/sqrt(vx*vy); corrxy
cor(dati$x,dati$y)
# test H0:corrxy=0 vs H1:corrxy <> 0
toss<-corrxy/sqrt((1-corrxy^2)/(n-2));toss
tcr<-qt(0.975,18); tcr
# toss=6.01 > tcr=2.10  -> posso falsificare H0
pv<-2*(1-pt(toss,18)); pv
# pvalue=0.00001 < 0.05  ->  posso falsificare l'ipotesi nulla
# verifica con funzione cor.test()
cor.test(dati$x,dati$y)
# detach(dati)
#
# regressione lineare
# variabile indipendente altezza
# variabile dipendente peso
# devo cambiare il nome alle variabili
names(dati)<-c("y","x","g"); dati
# attach(dati)
plot(dati$x,dati$y, main="Grafico a dispersione",
     xlab="Peso in kg.", ylab="Altezza in cm.")
n<-length(dati$x); n
mx<-sum(dati$x)/n; mx
my<-sum(dati$y)/n; my
vx<-sum((dati$x-mx)^2)/n; vx
vy<-sum((dati$y-my)^2)/n; vy
cxy<-sum((dati$x-mx)*(dati$y-my))/n; cxy
# stima del modello y=a+b*x
b<-cxy/vx; b
a<-my-b*mx; a
# disegno la retta di regressione sul grafico
abline(a,b, lty="dashed", col=2)             # lty=2 col="red"
# calcolo dei valori stimati per il peso yst
yst<-a+b*dati$x; yst
# calcolo dei residui res
res <- dati$y-yst; res
# i residui hanno media nulla
sum(res); round(sum(res),12)
# normalita' dei residui
plot(res, col=4)
abline(h=0, lty=2, col=2)
qqnorm(res)
qqline(res, lty=2, col=2)
#




#
# coefficiente di determinazione
R2<-corrxy^2; R2
# devianza totale, di regressione e dei residui
SST=sum((y-my)^2); SST
SSR=sum((yst-my)^2); SSR
SSE=sum(res^2); SSE
# calcolo nuovamente il coeff di determinazione
R2<-SSR/SST; R2
# analisi della devianza per la regressione   SST=SSR+SSE
SST; SSR+SSE
# 3094.8 = 2065.498+1029.302
# uso delle funzioni di R
mod<-lm(y~x)
summary(mod)
aov(mod)
#
detach(dati)
#
# il p-value per il test sulla correlazione, sul parametro b
# e sul coefficiente di determinazione coincidono
#
plot(mod)
# 
differenza tra maschi e femmine
ym<-dati$y[dati$g==1]; ym
yf<-dati$y[dati$g==2]; yf
xm<-dati$x[dati$g==1]; xm
xf<-dati$x[dati$g==2]; xf
# modello per i maschi
modm<-lm(ym~xm); summary(modm)
# modello per le femmine
modf<-lm(yf~xf); summary(modf)
#
# grafico maschi femmina
plot(dati$x,dati$y, type="n")
points(xm,ym,col=4)
points(xf,yf,col=2)
abline(modm,lty=2, col=4)
abline(modf,lty=2, col=2)
#
modm$coefficients
modf$coefficients
modm$coefficients[1]
am<-round(modm$coefficients[[1]],2); am
bm<-round(modm$coefficients[[2]],2); bm
text(180,95,paste("ym=",am," + ",bm,"*xm"), col=4)
#
af<-round(modf$coefficients[[1]],2); af
bf<-round(modf$coefficients[[2]],2); bf
text(180,55,paste("yf=",af," + ",bf,"*xf"), col=4)

par(mfrow=c(2,1))
qqnorm(modm$residuals)
qqline(modm$residuals)
text(-1, 8,"Maschi")
#
qqnorm(modf$residuals)
qqline(modf$residuals)
text(-1, 4,"Femmine")
par(mfrow=c(1,1))








