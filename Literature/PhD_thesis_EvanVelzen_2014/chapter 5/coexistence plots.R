#figure  6: branching and coexistence plots
palette(gray.colors(10,start=0.2,end=1,gamma=1))

#branching plots
stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff[1:8,2:5])
filled.contour(x=c(seq(5,30,5),40,50),y=c(0.1,0.3,0.5,1),z=stuff2,levels=c(0,0.01,0.5,0.95,1),
col=c(10,8,6,2),asp=50,xlab=expression(bold(c[max])),ylab="k",font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(10,50,10),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})

stuff2<-as.matrix(stuff[9:16,2:5])
filled.contour(x=c(seq(5,30,5),40,50),y=c(0.1,0.3,0.5,1),z=stuff2,levels=c(0,0.01,0.5,0.95,1),
col=c(10,8,6,2),asp=50,xlab=expression(bold(c[max])),ylab="k",font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(10,50,10),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})

stuff2<-as.matrix(stuff[17:24,2:5])
filled.contour(x=c(seq(5,30,5),40,50),y=c(0.1,0.3,0.5,1),z=stuff2,levels=c(0,0.01,0.5,0.95,1),
col=c(10,8,6,2),asp=50,xlab=expression(bold(c[max])),ylab="k",font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(10,50,10),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})

#coexistence plots
stuff2<-as.matrix(stuff[1:8,6:9])
filled.contour(x=c(seq(5,30,5),40,50),y=c(0.1,0.3,0.5,1),z=stuff2,levels=c(0,0.01,0.5,0.95,1),
col=c(10,8,6,2),asp=50,xlab=expression(bold(c[max])),ylab="k",font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(10,50,10),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})

stuff2<-as.matrix(stuff[9:16,6:9])
filled.contour(x=c(seq(5,30,5),40,50),y=c(0.1,0.3,0.5,1),z=stuff2,levels=c(0,0.01,0.5,0.95,1),
col=c(10,8,6,2),asp=50,xlab=expression(bold(c[max])),ylab="k",font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(10,50,10),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})

stuff2<-as.matrix(stuff[17:24,6:9])
filled.contour(x=c(seq(5,30,5),40,50),y=c(0.1,0.3,0.5,1),z=stuff2,levels=c(0,0.01,0.5,0.95,1),
col=c(10,8,6,2),asp=50,xlab=expression(bold(c[max])),ylab="k",font.lab=2,cex.lab=1.5,plot.axes={axis(1,seq(10,50,10),cex.axis=1.2)
axis(2,seq(0,1,0.2),cex.axis=1.2)})
