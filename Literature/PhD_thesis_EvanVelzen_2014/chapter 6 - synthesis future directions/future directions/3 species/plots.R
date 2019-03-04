palette(c(rainbow(10,start=0.02,end=0.9,v=0.9,s=0.6,alpha=0.8),"black"))
stuff<-read.table(file=file.choose(),header=T)

stuff2<-as.matrix(stuff[1:11,2:12])
stuff2<-as.matrix(stuff[1:11,4:14])
stuff2<-as.matrix(stuff[12:22,4:14])
barplot(stuff2,col=c(1,2,3,4,5,6,7,8,9,10,11),axes=T,axis.lty=1,las=1,axisname=T,
cex.axis=1.5,names.arg=seq(1.1,1.6,0.05),cex.names=1.5)

palette(rainbow(6,start=0.35,end=1,v=0.9,s=0.6,alpha=0.8))
stuff2<-as.matrix(stuff[49:54,4:16])
barplot(stuff2,col=c(1,2,3,4,5,6,7,8),axes=T,axis.lty=1,las=1,axisname=T,
cex.axis=1.5,names.arg=seq(1.2,1.8,0.05),cex.names=1.5)

stuff3<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff3[17:24,5:17])
palette(c(rainbow(7,start=0.05,end=0.9,v=0.9,s=0.6,alpha=0.7),"black"))
barplot(stuff2,col=c(1,2,3,4,5,6,7,8),axes=T,axis.lty=1,las=1,axisname=T,
cex.axis=1.5,names.arg=seq(1.2,1.8,0.05),cex.names=1.5)