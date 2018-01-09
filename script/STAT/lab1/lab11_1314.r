# lezione 11
# file lab11_1314.r
# seleziona la cartella di lavoro
setwd(choose.dir())
getwd()
# alternativa
# setwd("C:/dati/lav/corsi/stat/Statistica1/Stat1A1314/lab1")
dir()
# x qualitativa nominale
# x= stato civile 1=nubile/cel 2=coniugato/a 3=vedovo/a 4=separato/a
x<-c(1,4,3,3,2,1,2,2,3,1,1,1,4,2,1,2,3,4,2,2)
x
length(x)
# y qualitativa ordinale
# y= scolarita' 1=Analfabeta 2=Obbligo 3=Superiori 4=Laurea
y<-c(4,2,1,2,4,3,3,2,4,2,3,1,3,3,3,4,2,2,3,3); y; length(y)
# z= numero di figli
# quantitativa sugli interi
z<-c(0,1,3,4,1,1,0,2,3,0,1,0,1,4,3,0,2,2,4,4); z; length(z)
# w= peso delle pesrone
# w quantitativa sui reali
w<-c(72.50,54.28,50.02,88.88,62.30,45.21,57.50,78.40,
     75.13,58.00,53.70,91.29,74.70,41.22,65.20,63.58,
     48.27,52.52,69.50,85.98)
w ; length(w)
# costruisco la matrice di dati
# 
dati<-data.frame(x,y,z,w); dati
str(dati)
write.table(dati, file="dati1.txt")
dir()
write.csv(dati, file="dati1.csv")
#
# nuova sessione
#
ls()
rm(list=ls())
ls()
dati<-read.table("dati1.txt")
dati
dati$x
dati$x<-factor(dati$x)
str(dati)
levels(dati$x)<-c("N","C","V","S")
str(dati)
# y ordinale
dati$y<-factor(dati$y)
levels(dati$y)<-c("A","O","S","L"); dati$y
dati$y<-ordered(dati$y); dati$y
#
#
# x nominale frequenze assolute
fax<-table(dati$x); fax
frx<-fax/length(dati$x); frx
sum(frx)
fcx<-cumsum(table(dati$x)); fcx
pie(frx)
pie(frx, labels=frx, col=rainbow(4), main="Diagramma a torta")
barplot(frx, col=rainbow(4), main="Diagramma a barre")
# y ordinale
fay<-table(dati$y); fay
fry<-fay/length(dati$y); fry
fcy<-cumsum(table(dati$y)); fcy
pie(fry, col=rainbow(4))
barplot(fry, col=c("black","red"))
plot(sort(as.numeric(dati$y)),type="o")
boxplot(sort(as.numeric(dati$y)))
plot(sort(as.numeric(dati$y)),type="s")
# dati quantitativi discreti
# 
faz<-table(dati$z); faz
plot(dati$z)
plot(sort(dati$z))
# dati quantitativi reali
# w peso
faw<-table(dati$w); faw
plot(sort(dati$w))
boxplot(dati$w
summary(dati$w)
faw<-table(cut(dati$w, breaks=c(40,50,58,70,95))); faw
hist(dati$w, c(40,50,58,70,95))
hist(dati$w, c(40,50,58,70,95), main="Istogramma del peso",
xlab="Classi di peso", ylab="Densità", col=rainbow(4))
hist(dati$w, breaks="Scott")
hist(dati$w,breaks=8)
