palette(c(rainbow(7,start=0.05,end=0.9,v=0.9,s=0.6,alpha=0.7),"black"))


stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff[57:64,4:16])
barplot(stuff2,col=c(1,2,3,4,5,6,7,8),axes=T,axis.lty=1,las=1,axisname=T,
cex.axis=1.5,names.arg=seq(1.1,1.7,0.05),cex.names=1.5)