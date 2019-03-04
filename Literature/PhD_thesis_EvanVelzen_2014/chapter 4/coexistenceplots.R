stuff<-read.table(file=file.choose(),header=T)
stuff2<-as.matrix(stuff)
palette(gray.colors(10,start=0.2,end=1,gamma=1))

#final versions: with correlation on the y-axis

#figure 2: coexistence with cmax=5
filled.contour(x=seq(0.1,1,0.1),y=seq(0,1,0.1),z=stuff2,levels=c(0,0.05,0.95,1),
col=c(10,7,2),asp=0.9,xlab="aggregation (k)",ylab="correlation (r)",font.lab=2,cex.lab=1.7,plot.axes={axis(1,seq(0.2,1,0.2),cex.axis=1.5)
axis(2,seq(0,1,0.2),cex.axis=1.5)})
text(0.55,0.9,"(  )",cex=1.8,font=2)
text(0.55,0.9,"b",cex=1.8,font=4)

#figure 3: different within-host carrying capacity
stuff2<-as.matrix(stuff[1:9,3:13])
stuff2<-as.matrix(stuff[10:18,3:13])
stuff2<-as.matrix(stuff[19:27,3:13])
stuff2<-as.matrix(stuff[28:36,3:13])
filled.contour(x=c(4,5,6,8,10,15,20,25,30),y=seq(0,1,0.1),z=stuff2,levels=c(0,0.05,0.95,1),
col=c(10,7,2),asp=26,xlab=expression(bold(c[max])),ylab="correlation (r)",font.lab=2,cex.lab=1.7,plot.axes={axis(1,seq(5,30,5),cex.axis=1.5)
axis(2,seq(0,1,0.2),cex.axis=1.5)})
text(17,0.9,"(  )",cex=1.8,font=2)
text(17,0.9,"h",cex=1.8,font=4)

#figure 4: asymmetric within-host competition
stuff2<-as.matrix(stuff[1:6,3:13])
stuff2<-as.matrix(stuff[7:12,3:13])
stuff2<-as.matrix(stuff[13:18,3:13])
filled.contour(x=seq(1,3.5,0.5),y=seq(0,1,0.1),z=stuff2,levels=c(0,0.05,0.95,1),
col=c(10,7,2),asp=2.5,xlab=expression(bold(w[G])),ylab="correlation (r)",font.lab=2,cex.lab=1.7,plot.axes={axis(1,seq(1,3.5,0.5),cex.axis=1.5)
axis(2,seq(0,1,0.2),cex.axis=1.5)})
text(2.2,0.9,"(  )",cex=1.8,font=2)
text(2.2,0.9,"e",cex=1.8,font=4)
