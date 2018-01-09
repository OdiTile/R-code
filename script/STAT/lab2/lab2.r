# lezione 2
# file lab2.r
# dim car 12
# seleziona cartella di lavoro
rm(list=ls()); ls()
setwd(choose.dir())
getwd()
dir()
#setwd("C:/dati/lav/corsi/stat/Statistica1/Stat1B1213/lab2")
#setwd("H:/Statistica1/Stat1B1213/lab2")
# peso x
x<-c(64,54,65,42,58,50,58,65,78,52,48,66,58,103,63,70,65,53,62,58)
x; length(x)
# altezza y
y<-c(168,166,162,151,168,170,167,172,175,165,158,173,
     163,185,165,170,175,158,160,163); y; length(y)




# suddivisione in classi
# fax<-hist(x,c(40,57,64,105),plot=FALSE);fax
# alternativamente
summary(x)
fax<-table(cut(x, breaks=c(40,57,64,105))); fax
summary(y)
fay<-table(cut(y, breaks=c(150,164,170,190))); fay
# fxy<-table(fay,fax); fxy   # non funziona
#
#
xcl<-0
for(i in 1:length(x)){
if(x[i]<=57) xcl[i]<-1
if(x[i]>57 & x[i]<=64) xcl[i]<-2
if(x[i]>64) xcl[i]<-3}
xcl
#
ycl<-0
for(i in 1:length(y)){
if(y[i]<=164) ycl[i]<-1
if(y[i]>164 & y[i]<=170) ycl[i]<-2
if(y[i]>170) ycl[i]<-3}
ycl
# tabella di contingenza
fxy<-table(xcl,ycl); fxy

# suddivisione in classi con vettorializzazione
xcl<-0
xcl[x <= 57] <- 1
xcl[(x > 57) & (x<=64)] <- 2
xcl[x > 64] <- 3
xcl
#
ycl<-0
ycl[y <= 164] <- 1
ycl[(y > 164) & (y<=170)] <- 2
ycl[y > 170] <- 3
ycl
# tabella di contingenza
fxy<-table(xcl,ycl); fxy
# marginali come vettori
tr<-as.numeric(margin.table(fxy,1)); tr
tc<-as.numeric(margin.table(fxy,2)); tc
# calcolo delle frequenze attese
(tr)%*%t(tc)
fa<-((tr)%*%t(tc))/(sum(tc)); fa  #ok
chisq.test(fxy)$expected
# calcolo del chi quadro
chi2<-sum((fxy-fa)^2/fa) ; chi2   #ok
chisq.test(fxy)
# V di cramer

min(length(tr)-1,length(tc)-1)
sum(tc)
V<- sqrt(chi2/(sum(tc)*min(length(tr)-1,length(tc)-1))); V



