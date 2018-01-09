# lezione 2 approfondimento anova
# file lab2rivisto.r
rm(list=ls()); ls()
# peso x
x<-c(64,54,65,42,58,50,58,65,78,52,48,66,58,103,63,70,65,53,62,58)
x; length(x)
# altezza y
y<-c(168,166,162,151,168,170,167,172,175,165,158,173,
     163,185,165,170,175,158,160,163); y; length(y)
# suddivisione in classi con cut()  ##############################
summary(x)
xcl<-cut(x,c(40,57,64,105), labels=F); xcl
summary(y)
ycl<-cut(y,c(150,164,170,190), labels=F); ycl
# analisi della varianza
# media dei pesi per altezza 1,2,3
b<-(x[ycl==1]);b              # 65 42 48 58 53 62 58 peso dei bassi
m<-(x[ycl==2]);m              # 64 54 58 50 58 52 63 70 peso dei medi
a<-(x[ycl==3]);a              # 65 78 66 103 65 peso degli alti
nb<-length(b); nb             # 7 numero dei bassi
nm<-length(m); nm             # 8 numero dei medi
na<-length(a); na             # 5 numero degli alti
n<-length(x); n               # 20 numero totale
mb<-sum(b)/nb; mb             # 55.14286 media dei bassi
mm<-sum(m)/nm; mm             # 58.625 media dei medi
ma<-sum(a)/na; ma             # 75.4 media degli alti
mt<-sum(x)/n; mt              # 61.6 media totale
# varianze  
vt<-sum((x-mt)^2)/n; vt       # 154.74 varianza totale
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
mod<-aov(x~factor(ycl))
summary(mod)
anova(mod)




