stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)
palette(gray.colors(10,start=0.2,end=1,alpha=1))
filled.contour(x=seq(2000,200000,2000),y=seq(0,0.98,0.02),z=stuff2,
levels=c(0,0.01,0.2,0.3,0.4,0.5,0.6,0.7,1),col=c(10,7,6,5,4,3,2,1),asp=100000)

palette(c(rainbow(7,start=0.05,end=0.9,v=0.9,s=0.6,alpha=0.7),"black"))
barplot(stuff2,col=c(1,2,3,4,5,6,7,8),axes=T,axis.lty=1,las=1,axisname=T,
cex.axis=1.5,names.arg=seq(1.1,1.8,0.05),cex.names=1.5)

palette(rainbow(6,start=0.03,end=0.23,v=0.9,s=0.9,alpha=0.9))
stuff2<-as.matrix(stuff[49:54,4:16])
barplot(stuff2,col=c(1,2,3,4,5,6,7,8),axes=T,axis.lty=1,las=1,axisname=T,
cex.axis=1.5,names.arg=seq(1.2,1.8,0.05),cex.names=1.5)

stuff3<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff3[17:24,5:17])
palette(c(rainbow(7,start=0.05,end=0.9,v=0.9,s=0.6,alpha=0.7),"black"))
barplot(stuff2,col=c(1,2,3,4,5,6,7,8),axes=T,axis.lty=1,las=1,axisname=T,
cex.axis=1.5,names.arg=seq(1.2,1.8,0.05),cex.names=1.5)