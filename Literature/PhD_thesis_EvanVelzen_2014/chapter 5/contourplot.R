#figure 5: individual simulation runs

#preference
palette(gray.colors(10,start=0.2,end=1,gamma=1))
stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)
filled.contour(x=seq(500,50000,500),y=seq(0,0.98,0.02),z=stuff2,levels=c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.7,1),
col=c(10,8,7,6,4,3,2,1),asp=25000,xlab="generation",ylab="host preference",cex.lab=1.3,font.lab=2,plot.axes={axis(1,seq(0,50000,10000),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})

#venom division
stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)
filled.contour(x=seq(500,50000,500),y=seq(0,0.98,0.02),z=stuff2,levels=c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.7,1),
col=c(10,8,7,6,4,3,2,1),asp=25000,xlab="generation",ylab="venom division",cex.lab=1.3,font.lab=2,plot.axes={axis(1,seq(0,50000,10000),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})

#venom production
stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)
filled.contour(x=seq(500,50000,500),y=seq(0,0.98,0.02),z=stuff2,levels=c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.7,1),
col=c(10,8,7,6,4,3,2,1),asp=25000,xlab="generation",ylab="venom production",cex.lab=1.3,font.lab=2,plot.axes={axis(1,seq(0,50000,10000),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})

