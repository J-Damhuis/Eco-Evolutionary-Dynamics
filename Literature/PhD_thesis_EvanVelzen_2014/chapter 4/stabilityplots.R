par(family="serif")
palette(gray.colors(10,start=0.2,end=1,gamma=1))
#10 = unstable
#3 = stable
#7 = neutrally stable

#stable/neutral: nomax
#stable/neutral/unstable: negmulti ov diff=0, 0.05

stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)

rm(stuff)
rm(stuff2)
stuff<-read.table(file=file.choose(),header=F)
stuff2<-matrix(nrow=100,ncol=100)
for (i in seq(1,100,1)){
for (j in seq(1,100,1)){
stuff2[i,j]<-stuff[(i-1)*100+j,1]
}
}

#for stable/unstable only
filled.contour(x=seq(0.02,1.01,0.01),y=seq(1.11,2.1,0.01),z=stuff2,levels=c(0,1,100),
col=c(3,10),asp=1,xlab="k",ylab=expression(lambda),font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(0,1,0.2),cex.axis=1.2)
axis(2,seq(1.1,2.1,0.2),cex.axis=1.2)})
text(0.55,2,"(  )",cex=1.5,font=2,col="white")
text(0.55,2,"c",cex=1.5,font=4,col="white")

#for unstable/neutrally stable only
filled.contour(x=seq(0.02,1.01,0.01),y=seq(1.11,2.1,0.01),z=stuff2,levels=c(0,1.00001,100),
col=c(7,10),asp=1,xlab="k",ylab=expression(lambda),font.lab=2,cex.lab=2,plot.axes={axis(1,seq(0,1,0.2),cex.axis=1.6)
axis(2,seq(1.1,2.1,0.2),cex.axis=1.6)})
text(0.52,2,"(  )",cex=2,font=2,col="black")
text(0.52,2,"p",cex=2,font=4,col="black")

#for unstable/neutrally stable/stable
filled.contour(x=seq(0.02,1.01,0.01),y=seq(1.11,2.1,0.01),z=stuff2,levels=c(0,0.9999,1.00001,100),
col=c(3,7,10),asp=1,xlab="k",ylab=expression(lambda),font.lab=2,cex.lab=2,plot.axes={axis(1,seq(0,1,0.2),cex.axis=1.6)
axis(2,seq(1.1,2.1,0.2),cex.axis=1.6)})
text(0.52,2,"(  )",cex=2,font=2,col="white")
text(0.52,2,"d",cex=2,font=4,col="white")

==
#versions with colours reversed: stability calculated by using instability of 1P-H system instead

stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)

rm(stuff)
rm(stuff2)
stuff<-read.table(file=file.choose(),header=F)
stuff2<-matrix(nrow=100,ncol=100)
for (i in seq(1,100,1)){
for (j in seq(1,100,1)){
stuff2[i,j]<-stuff[(i-1)*100+j,1]
}
}

#for stable/unstable only
filled.contour(x=seq(0.02,1.01,0.01),y=seq(1.11,2.1,0.01),z=stuff2,levels=c(0,0.99999,100),
col=c(10,3),asp=1,xlab="k",ylab=expression(lambda),font.lab=2,cex.lab=2,plot.axes={axis(1,seq(0,1,0.2),cex.axis=1.6)
axis(2,seq(1.1,2.1,0.2),cex.axis=1.6)})
text(0.52,2,"(  )",cex=2,font=2,col="black")
text(0.52,2,"l",cex=2,font=4,col="black")

#for unstable/neutrally stable only
filled.contour(x=seq(0.04,1.02,0.02),y=seq(1.12,2.1,0.02),z=stuff2,levels=c(0,1.00001,100),
col=c(10,7),asp=1,xlab="k",ylab=expression(lambda),font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(0,1,0.2),cex.axis=1.2)
axis(2,seq(1.1,2.1,0.2),cex.axis=1.2)})

#for unstable/neutrally stable/stable
filled.contour(x=seq(0.02,1.01,0.01),y=seq(1.11,2.1,0.01),z=stuff2,levels=c(0,0.99999,1.00001,100),
col=c(10,7,3),asp=1,xlab="k",ylab=expression(lambda),font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(0,1,0.2),cex.axis=1.2)
axis(2,seq(1.1,2.1,0.2),cex.axis=1.2)})
text(0.55,2,"(  )",cex=1.5,font=2,col="white")
text(0.55,2,"d",cex=1.5,font=4,col="white")