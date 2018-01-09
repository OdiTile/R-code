# lezione 4 Indipendenza Lineare
# file lab4_1314.r
# dim car 20
# covarianza e correlazione
# modello diregressione lineare
# analisi della varianza per il modello di regressione
#
# seleziona cartella di lavoro
rm(list=ls()); ls()
setwd(choose.dir())
getwd()
dir()
#setwd("C:/dati/lav/corsi/stat/Statistica1/Stat1A1314/lab4")
#setwd("H:/Statistica1/Stat1A1314/lab4")
#
# preparazione del dataset altezza peso genere
# altezza x
x<-c(168,166,162,151,168,170,167,172,175,165,158,173,
     163,185,165,170,175,158,160,163); x; length(x)
# peso y
y<-c(64,54,65,42,58,50,58,65,78,52,48,66,58,103,63,70,65,53,62,58)
y; length(y)
# genere g 1 = maschi 2 = femmine
g<-c(2,2,1,2,1,2,2,1,1,1,2,2,2,1,2,1,1,2,1,2); g
dati<-data.frame(x,y,g); dati
write.table(dati, file="pesoaltgen.txt")
#
#
rm(list=ls()); ls()
dati<-read.table("pesoaltgen.txt"); dati
names(dati)
# names(dati)<-c("altezza","peso", "genere"); dati
# names(dati)<-c("x","y","g"); dati
#
# covarianza e correlazione x=altezza y=peso
attach(dati)
n<-length(x); n
mx<-sum(x)/n; mx
my<-sum(y)/n; my
vx<-sum((x-mx)^2)/n; vx
vy<-sum((y-my)^2)/n; vy
#
# diagramma a dispersione
plot(x,y, main="Diagramma altezza - peso", 
     xlab="altezza in cm", ylab="peso in kg")
abline(h=my, v=mx)
# scostamenti positivi e negativi
cbind(x,mx,(x-mx),y,my,(y-my),(x-mx)*(y-my))
#
covxy<-sum((x-mx)*(y-my))/n; covxy
cov(x,y)*(n-1)/n  # covarianza campionaria vs cov popolazione
corrxy<-covxy/sqrt(vx*vy); corrxy
cor(x,y)
#
# test H0:corrxy=0 vs H1:corrxy <> 0
# toss<-corrxy/sqrt((1-corrxy^2)/(n-2));toss
# tcr<-qt(0.975,18); tcr
# toss=6.01 > tcr=2.10  -> posso falsificare H0
# pv<-2*(1-pt(toss,18)); pv
# pvalue=0.00001 < 0.05  ->  posso falsificare l'ipotesi nulla
# verifica con funzione cor.test()
# cor.test(dati$x,dati$y)
# detach(dati)
#
# regressione lineare
# variabile indipendente altezza
# variabile dipendente peso
plot(dati$x,dati$y, main="Grafico a dispersione",
     xlab="Peso in kg.", ylab="Altezza in cm.")
# ipotetica retta di regressione
lines(c(155,180),c(45,85), col=4, lty=2)

# stima del modello y=a+b*x

b<-covxy/vx; b
a<-my-b*mx; a
# disegno la retta di regressione sul grafico
abline(a,b, lty="dashed", col=2)             # lty=2 col="red"
# calcolo dei valori stimati per il peso yst
yst<-a+b*dati$x 
cbind(y,yst)
round(cbind(y,yst),2)
# calcolo dei residui res
res <- dati$y-yst
res
round(res,2)
# i residui hanno media nulla
sum(res)
round(sum(res),12)
# normalita' dei residui
plot(res, col=4)
abline(h=0, lty=2, col=2)
qqnorm(res)
qqline(res, lty=2, col=2)
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
# differenza tra maschi e femmine
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
text(180,55,paste("yf=",af," + ",bf,"*xf"), col=2)

par(mfrow=c(2,1))
qqnorm(modm$residuals)
qqline(modm$residuals)
text(-1, 8,"Maschi")
#
qqnorm(modf$residuals)
qqline(modf$residuals)
text(-1, 4,"Femmine")
par(mfrow=c(1,1))






