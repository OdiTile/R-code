# Importanza8.r
# Classificazione binaria
# Utilizzo della funzione recode() (car)
rm(list=objects())
objects()
# library(epicalc)
# library(ca)
library(car)
library(xlsReadWrite)
dati<-read.xls( "c:\\dati\\lav\\Bolzan\\CompetenzePA\\DatiCompetenzePA.xls", 
          colNames = TRUE, 
          sheet = "OsservateAttese", 
          type = "data.frame" )
# dati[1:5,]
# names(dati)
# sostituisco i dati mancanti con la mediana
for(i in 2:64){
dati[,i][which(is.na(dati[,i]))]<-round(median(dati[,i],na.rm=TRUE))}
dati[1:5,]
which(is.na(dati))
datir<-round(dati); datir[1:5,]
for(i in 2:53){
datir[,i]<-recode(datir[,i],"1:5=1;c(6,7,8)=2;c(9,10)=3",as.factor.result=TRUE)}
head(datir)
# datir[,54:56]
# quali sono le risposte convinte sul ruolo del comunne
# tra erogatore-54 mediatore-55 e promotore-56?
# which(datir[,54]>50)
# which(datir[,55]>50)
# which(datir[,56]>50)
# which(datir[,54:56]>50)


# mission attese - osservate

v65<-dati[,57]-dati[,54]
v65<-recode(v65,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v65
v66<-dati[,58]-dati[,55]
v66<-recode(v66,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v66
v67<-dati[,59]-dati[,56]
v67<-recode(v67,"lo:-6=1; -5:5=2; 5:hi=3",as.factor.result=FALSE); v67
# comuni riclassificati 1=1,2,3 2=4,5
v68<-recode(dati$v60,"1:3=1; 4:5=2 ",as.factor.result=FALSE); v68
datir<-cbind(datir, v65, v66,v67)
head(datir)

# fine ricodifica preliminare =========================================
# =====================================================================

# segmentazione binaria
library(rpart)
# erogatore 0-49 -> 0 (47) - 50-100 -> 1 (87)
y57<-(datir$v57>49)*1; table(y57)  # 0=47 - 1=87
# dummy -> fattori
v621<-(datir$v62==1)*1; # v621
v622<-(datir$v62==2)*1; # v622
v623<-(datir$v62==3)*1; # v623
v624<-(datir$v62==4)*1; # v624
v642<-(datir$v64==2)*1; # v642

# segmentazione binaria su erogatore 0 - 100
mod<-rpart(v57~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v63
	         +v621+v622+v623+v624+v642, data=dati)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, main="Erogatore 0 - 100%")
text(mod,digits=3)
mod

# segmentazione binaria su mediatore 0 - 100
mod<-rpart(v58~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v63
	         +v621+v622+v623+v624+v642, data=dati)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, main="Mediatore 0 - 100%")
text(mod,digits=3)
mod

# segmentazione binaria su promotore 0 - 100
mod<-rpart(v59~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v63
	         +v621+v622+v623+v624+v642, data=dati)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, main="Promotore 0 - 100%")
text(mod,digits=3)
mod


# segmentazione binaria su erogatore atteso - osservato
mod<-rpart((v57-v54)~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v63
	         +v621+v622+v623+v624+v642, data=dati)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, main="Erogatore atteso - osservato")
text(mod,digits=3)
mod

# segmentazione binaria su mediatore atteso - osservato
mod<-rpart((v58-v55)~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v63
	         +v621+v622+v623+v624+v642, data=dati)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, main="Mediatore atteso - osservato")
text(mod,digits=3)
mod

# segmentazione binaria su promotore atteso - osservato
mod<-rpart((v59-v56)~v28+v29+v30+v31+v32+v33+v34+v35+v36+v37
		   +v38+v39+v40+v41+v42+v43+v44+v45+v46+v47
		   +v48+v49+v50+v51+v52+v53+v60+v61+v63
	         +v621+v622+v623+v624+v642, data=dati)
print(mod$cptable)
# plot(moderog, uniform=TRUE, margin=0.1, branch=0.5, compress=TRUE)
plot(mod, main="Promotore atteso - osservato")
text(mod,digits=3)
mod



















#
# Analisi discriminante su moderatore > 50%
#
#
# riclassificazione importanza elevata verso tutto il resto

round(cumsum(table(dati$v57))/length(dati$v57),2)
round(cumsum(table(dati$v58))/length(dati$v58),2)
round(cumsum(table(dati$v59))/length(dati$v59),2)
# alto = ultimo 33%
# y57<-recode(datir[,57],"0:50=0; 51:100=1")
# y58<-recode(datir[,58],"0:25=0; 26:100=1")
# y59<-recode(datir[,59],"0:25=0; 26:100=1")
# alternativa
# y57<-recode(datir[,57],"0:40=1; 41:50=2; 51:100=3",as.factor.result=TRUE)
# y58<-recode(datir[,58],"0:15=1; 16:25=2; 26:100=3",as.factor.result=TRUE)
# y59<-recode(datir[,59],"0:20=1; 21:25=2; 26:100=3",as.factor.result=TRUE)
# alto = ultimo 50%
# y57<-recode(datir[,57],"0:45=0; 46:100=1")
# y58<-recode(datir[,58],"0:25=0; 26:100=1")
# y59<-recode(datir[,59],"0:25=0; 26:100=1")


# dummy -> fattori
v621<-(datir$v62==1)*1; # v621
v622<-(datir$v62==2)*1; # v622
v623<-(datir$v62==3)*1; # v623
v624<-(datir$v62==4)*1; # v624

v642<-(datir$v64==2)*1; # v642

# ricodifica senza recode
# y57<-(datir$v57>45)*1;
# y57; table(y57)
y57<-(datir$v57>49)*1; table(y57)  # 0=47 - 1=87
y57<-(datir$v57>50)*1; table(y57)  # 0=90 - 1=44

library(MASS)

# attese<-dati[,28:53]; head(attese)
# attese<-c(dati$v28,dati$v29,dati$v30,dati$v31,dati$v32)
rl1<-glm(y57~dati$v28+dati$v29+dati$v30+dati$v31+dati$v32
		   +dati$v33+dati$v34+dati$v35+dati$v36+dati$v37
		   +dati$v38+dati$v39+dati$v40+dati$v41+dati$v42
		   +dati$v43+dati$v44+dati$v45+dati$v46+dati$v47
		   +dati$v48+dati$v49+dati$v50+dati$v51+dati$v52+dati$v53
		   +dati$v60+dati$v61+v621+v622+v623+dati$v63+v642, binomial)
rl1<-glm(y57~dati$v46+dati$v51, binomial)

summary(rl1)

discr1<-lda(y57~dati$v28+dati$v29+dati$v30+dati$v31+dati$v32
		   +dati$v33+dati$v34+dati$v35+dati$v36+dati$v37
		   +dati$v38+dati$v39+dati$v40+dati$v41+dati$v42
		   +dati$v43+dati$v44+dati$v45+dati$v46+dati$v47
		   +dati$v48+dati$v49+dati$v50+dati$v51+dati$v52+dati$v53
		   +dati$v60+dati$v61+v621+v622+v623+dati$v63+v642)
discr1
# discr1<-lda(y57~dati$v60+dati$v61+v621+v622+v623+dati$v63+v642)

# plot(discr1, main="Analisi discriminante su Erogatore")
# eqscplot(predict(discr1)$x, main="Rispondenti su scale equispaziate")
plot(discr1, type="density", dimen=1)
# predict(discr1)

# Mediatore
y58<-(datir$v58>24)*1; table(y58)  # 0=35 - 1=99
discr2<-lda(y58~dati$v28+dati$v29+dati$v30+dati$v31+dati$v32
		   +dati$v33+dati$v34+dati$v35+dati$v36+dati$v37
		   +dati$v38+dati$v39+dati$v40+dati$v41+dati$v42
		   +dati$v43+dati$v44+dati$v45+dati$v46+dati$v47
		   +dati$v48+dati$v49+dati$v50+dati$v51+dati$v52+dati$v53
		   +dati$v60+dati$v61+v621+v622+v623+dati$v63+v642)
discr2
plot(discr2, type="density", dimen=1)

# Promotore
y59<-(datir$v59>24)*1; table(y59)  # 0=90 - 1=44
discr3<-lda(y59~dati$v28+dati$v29+dati$v30+dati$v31+dati$v32
		   +dati$v33+dati$v34+dati$v35+dati$v36+dati$v37
		   +dati$v38+dati$v39+dati$v40+dati$v41+dati$v42
		   +dati$v43+dati$v44+dati$v45+dati$v46+dati$v47
		   +dati$v48+dati$v49+dati$v50+dati$v51+dati$v52+dati$v53
		   +dati$v60+dati$v61+v621+v622+v623+dati$v63+v642)
discr3
plot(discr3, type="density", dimen=1)






library(epicalc)
#y60<-(datir$v60==2)*1; table(y60)

tab1(y57,dati$v60)
tabpct(y57,dati$v60)
tabpct(dati$v60,y57)
table(y57,dati$v60)
chisq.test(table(y57,dati$v60))
table(y57,y60)
tabpct(y57,y60)
tabpct(y60,y57)
chisq.test(table(y57,y60))

# discr1<-lda(y57~dati$v60+dati$v61+v621+v622+v623+dati$v63+v642)
discr1<-lda(y57~dati$v60); plot(discr1)
discr1<-lda(y57~dati$v61); plot(discr1)
discr1<-lda(y57~v621); plot(discr1)
discr1<-lda(y57~v622); plot(discr1)
discr1<-lda(y57~v623); plot(discr1)
discr1<-lda(y57~dati$v63); plot(discr1)
discr1<-lda(y57~v642); plot(discr1)

(discr1<-lda(y57~dati$v60+dati$v61+v621+v622+v623+dati$v63+v642))
plot(discr1, main="Analisi discriminante su Erogatore")
eqscplot(predict(discr1)$x, main="Rispondenti su scale equispaziate")
plot(discr1, type="density", dimen=1)
predict(discr1)
#summary(discr1)

(discr2<-lda(y58~dati$v60+dati$v61+v621+v622+v623+dati$v63+v642))
plot(discr2, main="Analisi discriminante su Mediatore")
# eqscplot(discr2)
plot(discr2, type="density", dimen=1)
# predict(discr2)

(discr3<-lda(y59~dati$v60+dati$v61+v621+v622+v623+dati$v63+v642))
plot(discr3, main="Analisi discriminante su Promotore")
# eqscplot(discr3)
plot(discr3, type="density", dimen=1)
# predict(discr2)

# regressione logistica

mod57<-glm(y57~datir$v60+datir$v61+datir$v63+v621+v622+v623+v642+v65+v66+v67, 
family=binomial())
summary(mod57)

mod58<-glm(y58~datir$v60+datir$v61+datir$v63+v621+v622+v623+v642+v65+v66+v67, 
family=binomial())
summary(mod58)

mod59<-glm(y59~datir$v60+datir$v61+datir$v63+v621+v622+v623+v642+v65+v66+v67, 
family=binomial())
summary(mod59)

# library(epicalc)
logistic.display(mod57)
logistic.display(mod58)
logistic.display(mod59)
# tabelle di contingenza
tabpct(y57,datir$v60)
# stima del rischio attraverso l'odds ratio
cc(y57,datir$v61, design="case-control")
















