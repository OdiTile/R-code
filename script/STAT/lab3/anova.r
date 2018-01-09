 ###################################################
 ### Grafico Analisi della Varianza
 ###################################################
 dv <- c(8,6,5,1,2.5,3)
 factor <- factor(c('A','A','A','B','B','B'))
 factor1 <- c(1,2,3,5,6,7)
 factor1 <- c(1-.5,2-.5,3-.5,5,6,7)
 mean.a <- mean(dv[1:3])
 mean.b <- mean(dv[4:6])
 mean.ab <- mean(dv)
 
 # 2x2 grafici, spazio dei margini, riempimento area grafica
 old <- par(mfrow=c(2,2),mar=c(2,1,0.1,0.1),xpd=NA)  
 #grafico grezzo con medie
 plot(factor1,dv,pch=16,cex=1,axes=F,xlab='',ylab='',ylim=c(0,10),xlim=c(0,8))
 axis(1, at=c(2,6),lab=c('Group A','Group B'))
 axis(2,labels=F)
 lines(factor1[1:3],rep(mean(dv[1:3]),3), col="black",lwd=2) #media del fattore A
 lines(factor1[4:6],rep(mean(dv[4:6]),3),col="black",lwd=2)  #media del fattore B
 lines(factor1,rep(mean(dv),6), col="black",lwd=2)           #media generale
 
 text(4,7.5,'Media gruppo A',pos=4)
 arrows(2.65,6.5,3.9,7.5,code=1, length=.1)
 text(0,3,'Media generale',pos=4)
 arrows(2.5,3,3.9,4,code=2, length=.1)
 text(1,1,'Media gruppo B',pos=4)
 arrows(4,1,5.5,2,code=2, length=.1)
 text(0,10,'a',font=2,cex=2)
 box()                                                       # riquadro
 
 #grafico spiegazione medie interne ai gruppi
 plot(factor1,dv,pch=16,cex=1,axes=F,xlab='',ylab='',ylim=c(0,10),xlim=c(0,8))
 #text(2,-1,'Group A')
 #text(6,-1,'Group B')
 axis(1, at=c(2,6),lab=c('Gruppo A','Gruppo B'))
 axis(2,labels=F)
 text(0,10,'b',font=2,cex=2)
 box()
 
 # media di A
 lines(factor1[1:3],rep(mean(dv[1:3]),3), col="black",lwd=2)
 # media di B
 lines(factor1[4:6],rep(mean(dv[4:6]),3),col="black",lwd=2)
 # media generale
 lines(factor1,rep(mean(dv),6), col="black",lwd=2)
 
 #componente spiegata 
 # arrows(factor1, c(rep(mean.a,3), rep(mean.b,3)),factor1, c(rep(mean.ab,3), 
 #      rep(mean.ab,3)),lwd=1,lty=1, code=3,length=.1, col='gray')
 # Errore: unexpected ')' in "      rep(mean.ab,3))"
 arrows(factor1, c(rep(mean.a,3), rep(mean.b,3)),factor1, c(rep(mean.ab,3), 
       rep(mean.ab,3)),lwd=1,lty=1, code=3,length=.1, col='red')
 points(factor1,dv,pch=16,cex=1,col='black') # porta in primo piano i punti
 
 rect(3,5,8.3,10.4,col='white')
 text(3,9.75,expression(paste(SS[int], "= somma dei quadrati")),pos=4)
 text(4,9.25,expression(paste("delle distanze interne")),pos=4)
 text(3,8,expression(paste(MS[int], "= media della")),pos=4)
 text(4,7.5,expression(paste("var spiegata")),pos=4)
 text(4.5,6,expression(paste("= ", over(SS[int],df[int]))),pos=4)
 
 text(1,1,'Var. spiegata\n(distanze)',pos=4)
 arrows(3.5,1.5,4.9,3,code=2, length=.1)
 arrows(3.5,1.5,1.7,5,code=2, length=.1)
 
 #componente non spiegata
 plot(factor1,dv,pch=16,cex=1,axes=F,xlab='',ylab='',ylim=c(0,10),xlim=c(0,8))
 # media di A
 lines(factor1[1:3],rep(mean(dv[1:3]),3), col="black",lwd=2)
 # media di B
 lines(factor1[4:6],rep(mean(dv[4:6]),3),col="black",lwd=2)
 # media generale
 lines(factor1,rep(mean(dv),6), col="black",lwd=2)
 
 # componente non spiegata
 arrows(factor1, dv ,factor1, c(rep(mean.a,3), rep(mean.b,3)),
      lwd=1,lty=1, code=3,length=.1,col='red')
 points(factor1,dv,pch=16,cex=1, col='black')
 axis(1, at=c(2,6),lab=c('Gruppo A','Gruppo B'))
 axis(2,labels=F)
 text(0,10,'c',font=2,cex=2)
 box()
 
 
 rect(3,5,8.3,10.4,col='white')
 text(3,9.75,expression(paste(SS[res.], "= somma dei quadrati")),pos=4)
 text(4.2,9.25,expression(paste("delle distanze non sp.")),pos=4)
 text(3,8,expression(paste(MS[res.], "= media della")),pos=4)
 text(4.3,7.5,expression(paste("varianza residua")),pos=4)
 text(4.5,6,expression(paste("= ", over(SS[res.],df[res.]))),pos=4)
 
 text(.5,1,'Var. non spiegata\n(distanze)',pos=4)
 arrows(3.5,1.5,4.9,1.5,code=2, length=.1)
 arrows(3.5,1.5,2.6,5.5,code=2, length=.1)
 
 
 # distribuzione F di Snedecor
 plot(seq(0,5,length=100), df(seq(0,5,length=100), 
      df1=3, df2=10),type='l',xlab='', ylab='',axes=F,ylim=c(0,1))
 lines(c(0,5),c(0,0), col='gray')
 box()
 rect(1,.85,5.2,1.04,col='white')
 #text(1.25,.92,expression(paste("F-ratio=",over(Explained,Unexplained), " = ",over(MS[groups], MS[residual]))),pos=4) 
 text(1.25,.92,expression(paste("Rapporto F=",over(Spiegata," Non spiegata"), " = ",over(MS[int], MS[res]))),pos=4) 
 
 text(2,.5,expression(paste('distribuzione F\n(Distribuzione dei possibili\nvalori attesi del test F quando')),pos=4)
 text(2,.44,expression(paste('vale ', H[0],')')),pos=4)
 arrows(2,.5,1.5,.4,code=2,length=.1)
 text(0,1,'d',font=2,cex=2)
 par(old) 
