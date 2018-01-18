library(MASS)
library(ca)
data(farms)
farms
mjca(farms)
pchlist()
data(wg93)
wg93
mjca(wg93[,1:4])
summary(mjca(wg93[,1:4], lambda="Burt"))
plot(mjca(wg93[,1:4]))
data(smoke)
smoke
plot(ca(smoke, supcol=1))
plot(ca(smoke), mass=TRUE, contrib="absolute",
     map="rowgreen", arrows=c(FALSE,TRUE))
plot3d(ca(smoke,nd=3))

rm(list=objects())
objects()
library(ca)
library(xlsReadWrite)
dati<-read.xls( "c:\\dati\\lav\\Bolzan\\CompetenzePA\\DatiCompetenzePA.xls", 
          colNames = TRUE, 
          sheet = "Importanza", 
          type = "data.frame" )
dati
mjca(dati)
summary(mjca(dati))
plot(mjca(dati))
names(dati)
str(dati)
# riclassificazione delle variabili
v2r<-dati$v2; v2r
v2r[which(dati$v2<=5)]<-1
v2r[which(dati$v2>=6)]<-2
v2r[which(dati$v2>=9)]<-3; v2r

v3r<-dati$v3; v3r
v3r[which(dati$v3<=5)]<-1
v3r[which(dati$v3>=6)]<-2
v3r[which(dati$v3>=9)]<-3; v3r

v4r<-dati$v4; v4r
v4r[which(dati$v4<=5)]<-1
v4r[which(dati$v4>=6)]<-2
v4r[which(dati$v4>=9)]<-3; v4r

v5r<-dati$v5; v5r
v5r[which(dati$v5<=5)]<-1
v5r[which(dati$v5>=6)]<-2
v5r[which(dati$v5>=9)]<-3; v5r

v6r<-dati$v6; v6r
v6r[which(dati$v6<=5)]<-1
v6r[which(dati$v6>=6)]<-2
v6r[which(dati$v6>=9)]<-3; v6r

v7r<-dati$v7; v7r
v7r[which(dati$v7<=5)]<-1
v7r[which(dati$v7>=6)]<-2
v7r[which(dati$v7>=9)]<-3; v7r

v8r<-dati$v8; v8r
v8r[which(dati$v8<=5)]<-1
v8r[which(dati$v8>=6)]<-2
v8r[which(dati$v8>=9)]<-3; v8r

v9r<-dati$v9; v9r
v9r[which(dati$v9<=5)]<-1
v9r[which(dati$v9>=6)]<-2
v9r[which(dati$v9>=9)]<-3; v9r

v10r<-dati$v10; v10r
v10r[which(dati$v10<=5)]<-1
v10r[which(dati$v10>=6)]<-2
v10r[which(dati$v10>=9)]<-3; v10r

v11r<-dati$v11; v11r
v11r[which(dati$v11<=5)]<-1
v11r[which(dati$v11>=6)]<-2
v11r[which(dati$v11>=9)]<-3; v11r

v12r<-dati$v12; v12r
v12r[which(dati$v12<=5)]<-1
v12r[which(dati$v12>=6)]<-2
v12r[which(dati$v12>=9)]<-3; v12r

v13r<-dati$v13; v13r
v13r[which(dati$v13<=5)]<-1
v13r[which(dati$v13>=6)]<-2
v13r[which(dati$v13>=9)]<-3; v13r

v14r<-dati$v14; v14r
v14r[which(dati$v14<=5)]<-1
v14r[which(dati$v14>=6)]<-2
v14r[which(dati$v14>=9)]<-3; v14r

v15r<-dati$v15; v15r
v15r[which(dati$v15<=5)]<-1
v15r[which(dati$v15>=6)]<-2
v15r[which(dati$v15>=9)]<-3; v15r

v16r<-dati$v16; v16r
v16r[which(dati$v16<=5)]<-1
v16r[which(dati$v16>=6)]<-2
v16r[which(dati$v16>=9)]<-3; v16r

v17r<-dati$v17; v17r
v17r[which(dati$v17<=5)]<-1
v17r[which(dati$v17>=6)]<-2
v17r[which(dati$v17>=9)]<-3; v17r

v18r<-dati$v18; v18r
v18r[which(dati$v18<=5)]<-1
v18r[which(dati$v18>=6)]<-2
v18r[which(dati$v18>=9)]<-3; v18r

v19r<-dati$v19; v19r
v19r[which(dati$v19<=5)]<-1
v19r[which(dati$v19>=6)]<-2
v19r[which(dati$v19>=9)]<-3; v19r

v20r<-dati$v20; v20r
v20r[which(dati$v20<=5)]<-1
v20r[which(dati$v20>=6)]<-2
v20r[which(dati$v20>=9)]<-3; v20r

v21r<-dati$v21; v21r
v21r[which(dati$v21<=5)]<-1
v21r[which(dati$v21>=6)]<-2
v21r[which(dati$v21>=9)]<-3; v21r

v22r<-dati$v22; v22r
v22r[which(dati$v22<=5)]<-1
v22r[which(dati$v22>=6)]<-2
v22r[which(dati$v22>=9)]<-3; v22r

v23r<-dati$v23; v23r
v23r[which(dati$v23<=5)]<-1
v23r[which(dati$v23>=6)]<-2
v23r[which(dati$v23>=9)]<-3; v23r

v24r<-dati$v24; v24r
v24r[which(dati$v24<=5)]<-1
v24r[which(dati$v24>=6)]<-2
v24r[which(dati$v24>=9)]<-3; v24r

v25r<-dati$v25; v25r
v25r[which(dati$v25<=5)]<-1
v25r[which(dati$v25>=6)]<-2
v25r[which(dati$v25>=9)]<-3; v25r

v26r<-dati$v26; v26r
v26r[which(dati$v26<=5)]<-1
v26r[which(dati$v26>=6)]<-2
v26r[which(dati$v26>=9)]<-3; v26r

v27r<-dati$v27; v27r
v27r[which(dati$v27<=5)]<-1
v27r[which(dati$v27>=6)]<-2
v27r[which(dati$v27>=9)]<-3; v27r
# vedere recode() in car library

# esempio di tabulazione
x<-data.frame(v2r,v3r,v4r,v5r,v6r,v7r,v8r,v9r,v10r,
   v11r,v12r,v13r,v14r,v15r,v16r,v17r,v18r,v19r,
   v20r,v21r,v22r,v23r,v24r,v25r,v26r,v27r); x
table(x)
x1<-as.data.frame(table(x)); x1
x2<-x1[x1$Freq>0,]; x2
length(x2[,1])



v2r<-as.factor(v2r)
v3r<-as.factor(v3r)
v4r<-as.factor(v4r)
v5r<-as.factor(v5r)
v6r<-as.factor(v6r)
v7r<-as.factor(v7r)
v8r<-as.factor(v8r)
v9r<-as.factor(v9r)
v10r<-as.factor(v10r)
v11r<-as.factor(v11r)
v12r<-as.factor(v12r)
v13r<-as.factor(v13r)
v14r<-as.factor(v14r)
v15r<-as.factor(v15r)
v16r<-as.factor(v16r)
v17r<-as.factor(v17r)
v18r<-as.factor(v18r)
v19r<-as.factor(v19r)
v20r<-as.factor(v20r)
v21r<-as.factor(v21r)
v22r<-as.factor(v22r)
v23r<-as.factor(v23r)
v24r<-as.factor(v24r)
v25r<-as.factor(v25r)
v26r<-as.factor(v26r)
v27r<-as.factor(v27r)

datir<-(cbind(v2r,v3r,v4r,v5r,v6r,v7r,v8r,v9r,v10r,v11r,v12r,v13r,
v14r,v15r,v16r,v17r,v18r,v19r,v20r,v21r,v22r,v23r,v24r,v25r,v26r,v27r)); datir
str(datir)
attributes(datir)
mjca(datir)
summary(mjca(datir))
plot(mjca(datir))
# matrice ridotta
datir<-(cbind(v21r,v22r,v23r,v24r,v25r,v26r,v27r)); datir
datir<-na.omit(datir)
datir
mjca(datir)
summary(mjca(datir))
plot(mjca(datir))

# prova con recode (car)
rm(list=objects())
objects()
library(ca)
library(car)
library(xlsReadWrite)
dati<-read.xls( "c:\\dati\\lav\\Bolzan\\CompetenzePA\\DatiCompetenzePA.xls", 
          colNames = TRUE, 
          sheet = "Importanza", 
          type = "data.frame" )
dati[1:5,]
names(dati)
str(dati)
# riclassificazione delle variabili
# 1 insufficienta 1:5
# 2 sufficiente 6,7 + NA
# 3 buono 8,9,10
v2r<-recode(dati$v2,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v3r<-recode(dati$v3,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v4r<-recode(dati$v4,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v5r<-recode(dati$v5,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v6r<-recode(dati$v6,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v7r<-recode(dati$v7,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v8r<-recode(dati$v8,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v9r<-recode(dati$v9,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v10r<-recode(dati$v10,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v11r<-recode(dati$v11,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v12r<-recode(dati$v12,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v13r<-recode(dati$v13,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v14r<-recode(dati$v14,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v15r<-recode(dati$v15,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v16r<-recode(dati$v16,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v17r<-recode(dati$v17,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v18r<-recode(dati$v18,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v19r<-recode(dati$v19,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v20r<-recode(dati$v20,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v21r<-recode(dati$v21,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v22r<-recode(dati$v22,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v23r<-recode(dati$v23,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v24r<-recode(dati$v24,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v25r<-recode(dati$v25,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v26r<-recode(dati$v26,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
v27r<-recode(dati$v27,"1:5=1;c(6,7,NA)=2;8:10=3",as.factor.result=TRUE)
dati$v4;v4r
# str(v2r)
datir<-(cbind(v2r,v3r,v4r,v5r,v6r,v7r,v8r,v9r,v10r,v11r,v12r,v13r,
v14r,v15r,v16r,v17r,v18r,v19r,v20r,v21r,v22r,v23r,v24r,v25r,v26r,v27r)); datir
str(datir)
dati[18,];datir[18,]
datir<-datir[-18,] # tolgo l'intervista 18 per troppi NA
# datir
# corrispondenze multiple per dati su Importanza
mjca(datir)
summary(mjca(datir))
plot(mjca(datir))
# sort(mjca(datir)$rowdist)
# sort(mjca(datir)$rowinertia)
# mjca(datir)$rowcoord[,1:2]
# sort(mjca(datir)$coldist)
# mjca(datir)$colcoord[,1:2]


# prova con valutazione limitato - elevato
rm(list=objects())
objects()
library(ca)
library(car)
library(xlsReadWrite)
dati<-read.xls( "c:\\dati\\lav\\Bolzan\\CompetenzePA\\DatiCompetenzePA.xls", 
          colNames = TRUE, 
          sheet = "Importanza", 
          type = "data.frame" )
dati[1:5,]
names(dati)
str(dati)
# classificazione dicotomica 
# 1 valutazione limitata 1:7
# 2 valutazione elevata 8,9,10 + NA
v2r<-recode(dati$v2,"1:7=1;else = 2",as.factor.result=TRUE)
v3r<-recode(dati$v3,"1:7=1;else = 2",as.factor.result=TRUE)
v4r<-recode(dati$v4,"1:7=1;else = 2",as.factor.result=TRUE)
v5r<-recode(dati$v5,"1:7=1;else = 2",as.factor.result=TRUE)
v6r<-recode(dati$v6,"1:7=1;else = 2",as.factor.result=TRUE)
v7r<-recode(dati$v7,"1:7=1;else = 2",as.factor.result=TRUE)
v8r<-recode(dati$v8,"1:7=1;else = 2",as.factor.result=TRUE)
v9r<-recode(dati$v9,"1:7=1;else = 2",as.factor.result=TRUE)
v10r<-recode(dati$v10,"1:7=1;else = 2",as.factor.result=TRUE)
v11r<-recode(dati$v11,"1:7=1;else = 2",as.factor.result=TRUE)
v12r<-recode(dati$v12,"1:7=1;else = 2",as.factor.result=TRUE)
v13r<-recode(dati$v13,"1:7=1;else = 2",as.factor.result=TRUE)
v14r<-recode(dati$v14,"1:7=1;else = 2",as.factor.result=TRUE)
v15r<-recode(dati$v15,"1:7=1;else = 2",as.factor.result=TRUE)
v16r<-recode(dati$v16,"1:7=1;else = 2",as.factor.result=TRUE)
v17r<-recode(dati$v17,"1:7=1;else = 2",as.factor.result=TRUE)
v18r<-recode(dati$v18,"1:7=1;else = 2",as.factor.result=TRUE)
v19r<-recode(dati$v19,"1:7=1;else = 2",as.factor.result=TRUE)
v20r<-recode(dati$v20,"1:7=1;else = 2",as.factor.result=TRUE)
v21r<-recode(dati$v21,"1:7=1;else = 2",as.factor.result=TRUE)
v22r<-recode(dati$v22,"1:7=1;else = 2",as.factor.result=TRUE)
v23r<-recode(dati$v23,"1:7=1;else = 2",as.factor.result=TRUE)
v24r<-recode(dati$v24,"1:7=1;else = 2",as.factor.result=TRUE)
v25r<-recode(dati$v25,"1:7=1;else = 2",as.factor.result=TRUE)
v26r<-recode(dati$v26,"1:7=1;else = 2",as.factor.result=TRUE)
v27r<-recode(dati$v27,"1:7=1;else = 2",as.factor.result=TRUE)
# x<-data.frame(v2r,v3r,v4r,v5r);x[1:5,]
x<-data.frame(v2r,v3r,v4r,v5r,v6r,v7r,v8r,v9r,v10r,v11r,v12r,v13r
 ,v14r,v15r,v16r,v17r,v18r,v19r,v20r,v21r,v22r,v23r,v24r,v25r,v26r,v27r); x[1:5,]
str(x)
# table(x)
# x<-as.data.frame(table(x)); x
# dim(x)
# x<-x[-which(x$Freq==0),]; x
mjca(x)
summary(mjca(x))
plot(mjca(x))

# classificazione dicotomica 
# 1 valutazione limitata 1:8
# 2 valutazione elevata 9,10 + NA
v2r<-recode(dati$v2,"1:8=1;else = 2",as.factor.result=TRUE)
v3r<-recode(dati$v3,"1:8=1;else = 2",as.factor.result=TRUE)
v4r<-recode(dati$v4,"1:8=1;else = 2",as.factor.result=TRUE)
v5r<-recode(dati$v5,"1:8=1;else = 2",as.factor.result=TRUE)
v6r<-recode(dati$v6,"1:8=1;else = 2",as.factor.result=TRUE)
v7r<-recode(dati$v7,"1:8=1;else = 2",as.factor.result=TRUE)
v8r<-recode(dati$v8,"1:8=1;else = 2",as.factor.result=TRUE)
v9r<-recode(dati$v9,"1:8=1;else = 2",as.factor.result=TRUE)
v10r<-recode(dati$v10,"1:8=1;else = 2",as.factor.result=TRUE)
v11r<-recode(dati$v11,"1:8=1;else = 2",as.factor.result=TRUE)
v12r<-recode(dati$v12,"1:8=1;else = 2",as.factor.result=TRUE)
v13r<-recode(dati$v13,"1:8=1;else = 2",as.factor.result=TRUE)
v14r<-recode(dati$v14,"1:8=1;else = 2",as.factor.result=TRUE)
v15r<-recode(dati$v15,"1:8=1;else = 2",as.factor.result=TRUE)
v16r<-recode(dati$v16,"1:8=1;else = 2",as.factor.result=TRUE)
v17r<-recode(dati$v17,"1:8=1;else = 2",as.factor.result=TRUE)
v18r<-recode(dati$v18,"1:8=1;else = 2",as.factor.result=TRUE)
v19r<-recode(dati$v19,"1:8=1;else = 2",as.factor.result=TRUE)
v20r<-recode(dati$v20,"1:8=1;else = 2",as.factor.result=TRUE)
v21r<-recode(dati$v21,"1:8=1;else = 2",as.factor.result=TRUE)
v22r<-recode(dati$v22,"1:8=1;else = 2",as.factor.result=TRUE)
v23r<-recode(dati$v23,"1:8=1;else = 2",as.factor.result=TRUE)
v24r<-recode(dati$v24,"1:8=1;else = 2",as.factor.result=TRUE)
v25r<-recode(dati$v25,"1:8=1;else = 2",as.factor.result=TRUE)
v26r<-recode(dati$v26,"1:8=1;else = 2",as.factor.result=TRUE)
v27r<-recode(dati$v27,"1:8=1;else = 2",as.factor.result=TRUE)
# x<-data.frame(v2r,v3r,v4r,v5r);x[1:5,]
x<-data.frame(v2r,v3r,v4r,v5r,v6r,v7r,v8r,v9r,v10r,v11r,v12r,v13r
 ,v14r,v15r,v16r,v17r,v18r,v19r,v20r,v21r,v22r,v23r,v24r,v25r,v26r,v27r); x[1:5,]
str(x)
# table(x)
# x<-as.data.frame(table(x)); x
# dim(x)
# x<-x[-which(x$Freq==0),]; x
mjca(x)
summary(mjca(x))
plot(mjca(x))



