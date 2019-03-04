palette(gray.colors(10,start=0.2,end=1,gamma=1))

stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)
filled.contour(x=seq(2,200,2),y=seq(0,0.98,0.02),z=stuff2,levels=c(0,0.01,0.1,0.15,0.2,0.3,0.4,0.5,1),
col=c(10,8,7,6,4,3,2,1),asp=100,xlab="Generation (*1000)",ylab="Preference",cex.lab=1.5,font.lab=2,plot.axes={axis(1,seq(0,200,40),cex.axis=1.3)
axis(2,seq(0,1,0.2),cex.axis=1.3)})

stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)
filled.contour(x=seq(2,200,2),y=seq(0,0.98,0.02),z=stuff2,levels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,1),
col=c(10,8,7,6,4,3,2,1),asp=100,xlab="Generation (*1000)",ylab="Preference",cex.lab=1.5,font.lab=2,plot.axes={axis(1,seq(0,200,40),cex.axis=1.3)
axis(2,seq(0,1,0.2),cex.axis=1.3)})