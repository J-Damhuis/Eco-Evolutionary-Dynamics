#figure 1: abundance series
plotabundance<-function(c1, c2, time1, time2, length1, length2, skip1, skip2){
plot.new()
par(mfrow=c(1,2),mar=c(4.5,4.3,1,1.2),cex=1.1,cex.lab=1.2,lwd=2,bg="transparent",las=1)
plant1<-c(c1*2,c2*2)
plot(x=stuff2[(time1+1):(time1+length1),1],y=stuff2[(time1+1):(time1+length1),plant1[1]],type="l",lty="solid",xlab="time",
ylab="plant biomass        ",ylim=c(0.09,3.1),xlim=c(time1+skip1,time1+length1-skip2))
points(x=stuff2[(time1+1):(time1+length1),1],y=stuff2[(time1+1):(time1+length1),plant1[1]+1],lty="longdash",type="l")
text(x=time1+40,y=2.9,"A",cex=1.3)
plot(x=stuff2[(time2+1):(time2+length2),1],y=stuff2[(time2+1):(time2+length2),plant1[2]],type="l",xlab="time",
ylab="",ylim=c(0.09,3.1),xlim=c(time2+skip1,time2+length2-skip2))
points(x=stuff2[(time2+1):(time2+length2),1],y=stuff2[(time2+1):(time2+length2),plant1[2]+1],lty="longdash",type="l")
text(x=time2+40,y=2.9,"B",cex=1.3)
}
plotabundance(2,4,0,5000,800,800,29,29)


#figure 2: fitnesslandscapes
plotfitnesslandscapes<-function(x1, x2){
plot.new()
par(mfrow=c(1,2),cex=1.1,cex.lab=1.2,lwd=2,las=1,mar=c(4.5,4.5,1,1))
plot(x=stuff[1:51,1],y=stuff[1:51,6],type="l",lty="solid",xlab="preference, x",ylab="fitness",lab=c(5,4,2),xlim=c(0.03,0.97),ylim=c(0.96,1))
text(x=0.08,y=0.997,"A",cex=1.3)
plot(x=stuff[1:51,1],y=stuff[1:51,x1],type="l",lty="solid",xlab="preference, x",ylab="",lab=c(5,4,2),xlim=c(0.03,0.97),ylim=c(0.96,1))
points(x=stuff[1:51,1],y=stuff[1:51,x2],type="l",lty="longdash")
text(x=0.08,y=0.997,"B",cex=1.3)
}
plotfitnesslandscapes(4,2)


#figure 3: simulation runs
palette(gray.colors(10,start=0.2,end=1,gamma=1))
filled.contour(x=seq(1000,100000,1000),y=seq(0,0.98,0.02),z=stuff2,levels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,1),col=c(10,7,6,5,4,3,2,1))
palette(gray.colors(10,start=0.2,end=1,gamma=1))
filled.contour(x=seq(10,1000,10),y=seq(0,0.98,0.02),z=stuff2,levels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.8,1),col=c(10,7,6,5,4,3,2,1))


#figure 4: barplots T=4
palette(c(rainbow(7,start=0.05,end=0.9,v=0.9,s=0.6,gamma=0.7),"black"))
barplot(stuff2,col=c(1,2,3,4,5,6,7,8),xlim=c(0,30),axes=T,axisname=T,
names.arg=c(seq(1.05,1.8,0.05),1.9,2.0,seq(2.2,3.6,0.2)))


#figure 5: pips
filled.contour(x=seq(0.5,1,0.005),y=seq(0.5,1,0.005),z=stuff2,levels=c(0,1,2),col=c("white","black"),asp=1)
filled.contour(x=seq(0.5,0.995,0.005),y=seq(0.5,1,0.005),z=stuff2,levels=c(0,1,2),col=c("white","black"),asp=1)


#figure 6: barplots all r and T
plotall<-function(lim){
plot.new()
par(mfcol=c(5,5),mar=c(1,1,1,1))
a<-1
for(i in seq(1,25,1)){
stuff4<-as.matrix(stuff[a:(a+7),4:18])
if (i==5)
barplot(stuff4,col=c(1,2,3,4,5,6,7,8),xlim=c(0,lim),axes=T,axisname=T,names.arg=seq(1.1,1.8,0.05))
else {
if (i>5&&i%%5==0){
barplot(stuff4,col=c(1,2,3,4,5,6,7,8),xlim=c(0,lim),axes=F,axisname=T,names.arg=seq(1.1,1.8,0.05))}
else {
if (i<5)
barplot(stuff4,col=c(1,2,3,4,5,6,7,8),xlim=c(0,lim),axes=T,axisname=F)
else barplot(stuff4,col=c(1,2,3,4,5,6,7,8),xlim=c(0,lim),axes=F,axisname=F)
}}
a<-a+8
}}

ploth<-function(){
plot.new()
par(mfcol=c(1,5),mar=c(20,1,10,1))
a<-1
for(i in seq(1,25,1)){
stuff4<-as.matrix(stuff[a:(a+7),4:18])
if(i%%5==0)
barplot(stuff4,col=c(1,2,3,4,5,6,7,8),xlim=c(0,16.5),axes=F,axisname=T,names.arg=seq(1.1,1.8,0.05))
a<-a+8
}}

plotv<-function(){
plot.new()
par(mfcol=c(5,1),mar=c(1,10,1,10))
a<-1
for(i in seq(1,5,1)){
stuff4<-as.matrix(stuff[a:(a+7),4:18])
barplot(stuff4,col=c(1,2,3,4,5,6,7,8),xlim=c(0,16.5),axes=T,axisname=F)
a<-a+8
}}


#figure 7: extinctionrates
palette(gray.colors(5,start=0.2,end=1,gamma=1))
filled.contour(x=c(0.5,0.75,1,1.5,2),y=seq(2.5,6.5,1),z=stuff,levels=c(0,0.02,0.05,0.1,0.15,0.25),col=c(5,4,3,2,1),
asp=3/8,plot.axes={axis(1,seq(0.5,2,0.5))
axis(2,seq(2.5,6.5,1))},
plot.title=title(main="extinction rate", xlab="Intrinsic growth rate", ylab="Total nutrients"))
