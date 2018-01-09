# lezione 3
# file lab3.r
# imposta cartella di lavoro
setwd(choose.dir())
dir()
getwd()
# leggiamo i dati
dati1<-read.table("pesoalt1.txt")
dati1
# analisi della varianza
# indipendenza in media
# media dei pesi per altezze 1,2,3
attach(dati1)
x
mg<-tapply(dati1$x, dati1$ycl, mean)
mg
round(mg,1)
# tre variabili con il peso 
# dei 1=bassi, 2=medi, 3=alti
b<-(dati1$x[dati1$ycl==1]);b
m<-(dati1$x[dati1$ycl==2]);m
g<-(dati1$x[dati1$ycl==3]);g
# numerosita' dei gruppi
nb<-length(b); nb
nm<-length(m); nm
ng<-length(g); ng
n<-length(x); n
# medie dei gruppi
mb<-sum(b)/nb; mb
mm<-sum(m)/nm; mm
mg<-sum(g)/ng; mg
mt<-sum(x)/n; mt
# varianze
vt<-sum((x-mt)^2)/n; vt
vb<-(((mb-mt)^2)*nb + ((mm-mt)^2)*nm +
     ((mg-mt)^2)*ng)/n
vb    # varianza tra i gruppi
#
vw<-(sum((b-mb)^2)+sum((m-mm)^2)+sum((g-mg)^2))/n
vw    # varianza interna ai gruppi
vwb<-sum((b-mb)^2); vwb
vwm<-sum((m-mm)^2); vwm
vwg<-sum((g-mg)^2); vwg
vw<-(vwb+vwm+vwg)/n; vw  # varianza interna ai gruppi
# calcolo di eta quadro
eta2<-vb/vt; eta2
# analisi della varianza
# vb+vw=vt
vb+vw; vt
# la stessa analisi utilizzando le devianze
dt<-vt*n; dt
dw<-vw*n; dw
db<-vb*n; db
# verifica
db+dw; dt
# segno tilde ~  <-  Alt+126
mod<-aov(x~ycl)
summary(mod)
anova(mod)















































detach(dati1)