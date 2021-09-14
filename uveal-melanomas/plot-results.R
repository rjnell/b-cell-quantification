system("taskkill /F /IM Microsoft.Photos.exe /T")
graphics.off()

library(readxl)
data = read_xlsx("D:/SURFDRIVE/ORGANIZED/Projecten/Project 'B-celkwantificatie' (artikel Wim)/data.xlsx")

data = as.matrix(data[2:5,c(1,6:16)])
#data = as.matrix(data[2:5,c(1,12:21)])


file = "uveal-melanomas/fraction-b-cells.png"
png(file, res=600, 2500, 2500)  
par(mar=c(7.5,6,2.5,5))
xlim = c(0.5,4.5)
ylim = c(-7.5,80)

plot(xlim, 
     ylim, 
     type = "n", 
     axes = F, 
     xlab = "",
     ylab = "",
     xaxs = "i", 
     yaxs = "i")
#xlim[2] = nrow(data)+1
ylim = c(0,100)
ylim[2] = 40
xat = seq(xlim[1]+1, xlim[2]-1, by=1)
yat = seq(ylim[1], ylim[2], by=10)

#axis(side = 1, at = xat, labels = rep("",length(xat)), col = "#b1b1b1", lwd = 1.4, col.axis="#333333", tck = 0)
axis(side = 2, at = yat, labels = paste0(yat,"%"), las=2, col = "#b1b1b1", lwd = 1.4, col.axis="#333333", cex=0.9)
mtext(side = 2, text = "Fraction", line = 4.3, at=20)
mtext(side = 2, text = "B cells", line = 3.5, at=20)
#text(mean(xlim), 110, xpd=T, cex=1, font=2, labels=tumor)
xat = c(1.5,2.5,3.5)
ylim[1] = -5
ylim[2] = 45
segments(xat, ylim[1], xat, ylim[2], col="#eeeeee", lwd=1.4, xpd=T)
segments(xat, ylim[1], xat, 0, col="#b1b1b1", lwd=1.4, xpd=T)

segments(xlim[1], yat, xlim[2], col="#eeeeee", lwd=1.4, xpd=T)
segments(xlim[1], ylim[1], xlim[1], ylim[2], col="#b1b1b1", lwd=1.4, xpd=T)
#segments(xlim[1], ylim[1], xlim[2], ylim[1], col="#b1b1b1", lwd=1.4, xpd=T)
segments(xlim[1], 0, xlim[2], 0, col="#b1b1b1", lwd=1.4, xpd=T)
segments(3.5, 0, 3.5, ylim[2], col="#b1b1b1", lwd=1.4, xpd=T, lty=3)
for (i in 1:nrow(data)) {
  col = "#333333"
  #rect(i-0.4,0,i,as.numeric(data[i,5]), border=NA, col=col)
  arrows(i-0.2+0.075, as.numeric(data[i,6]), i-0.2+0.075, as.numeric(data[i,7]), code=3, angle=90, length=0.05, col="#b1b1b1", lwd=1.4)
  points(i-0.2+0.075,as.numeric(data[i,5]), border=NA, col="white", pch=16)
  points(i-0.2+0.075,as.numeric(data[i,5]), border=NA, col=col, pch=1, cex=0.9)
  
  #rect(i+0,0,i+0.4,as.numeric(data[i,2]), border=NA, col=col)
  
  arrows(i+0.2-0.075, as.numeric(data[i,3]), i+0.2-0.075, as.numeric(data[i,4]), code=3, angle=90, length=0.05, col="#b1b1b1", lwd=1.4)
  points(i+0.2-0.075,as.numeric(data[i,2]), border=NA, col=col, pch=16)
  
  
  #m = max(as.numeric(data[i,c(4,7)]))
  #segments(i+0.2-0.075, as.numeric(data[i,4])+5, i+0.2-0.075, m+7.5, col="#333333", lwd=1.4)
  #segments(i-0.2+0.075, as.numeric(data[i,7])+5, i-0.2+0.075, m+7.5, col="#333333", lwd=1.4)
  #segments(i+0.2-0.075, m+7.5, i-0.2+0.075, m+7.5, col="#333333", lwd=1.4)
  #if(i==4) {
  #  text(i,m+5,labels="*",cex=0.9,xpd=T,pos=3)  
  #}
  #else {
  #  text(i,m+7.5,labels="n/s",cex=0.9,xpd=T,pos=3)  
  #}
  
  
  text(i,-15,labels=data[i,1],cex=0.9,xpd=T)
}


points(4.95,30, border=NA, col=col, pch=1, cex=0.9, xpd=T)
text(5,33,labels="classic", font=3,cex=0.9,xpd=T, pos=4)
text(5,27,labels="model", font=3,cex=0.9,xpd=T, pos=4)


points(4.95,10, border=NA, col=col, pch=16, xpd=T)
text(5,13,labels="adjusted", font=3,cex=0.9,xpd=T, pos=4)
text(5,7,labels="model", font=3,cex=0.9,xpd=T, pos=4)
#i=1
#rect(i-0.4,-30,i+0.4,-37.5, border=NA, col=col, xpd=T)
#text(i+0.3,-33.75,labels="B cells", cex=0.9,xpd=T, pos=4)

dev.off()
system(paste("open",file))



























system("taskkill /F /IM Microsoft.Photos.exe /T")
graphics.off()

library(readxl)
data = read_xlsx("D:/SURFDRIVE/ORGANIZED/Projecten/Project 'B-celkwantificatie' (artikel Wim)/data.xlsx")

#data = as.matrix(data[2:5,c(1,6:16)])
data = as.matrix(data[2:5,c(1,22:27)])


file = "uveal-melanomas/fraction-switched-b-cells.png"
png(file, res=600, 2500, 2500)  
par(mar=c(7.5,6,2.5,5))
xlim = c(0.5,4.5)
ylim = c(-7.5,80)

plot(xlim, 
     ylim, 
     type = "n", 
     axes = F, 
     xlab = "",
     ylab = "",
     xaxs = "i", 
     yaxs = "i")
#xlim[2] = nrow(data)+1
ylim = c(0,100)
ylim[2] = 40
xat = seq(xlim[1]+1, xlim[2]-1, by=1)
yat = seq(ylim[1], ylim[2], by=10)

#axis(side = 1, at = xat, labels = rep("",length(xat)), col = "#b1b1b1", lwd = 1.4, col.axis="#333333", tck = 0)
axis(side = 2, at = yat, labels = paste0(yat,"%"), las=2, col = "#b1b1b1", lwd = 1.4, col.axis="#333333", cex=0.9)
mtext(side = 2, text = "Fraction", line = 4.3, at=20)
mtext(side = 2, text = "switched B cells", line = 3.5, at=20)
#text(mean(xlim), 110, xpd=T, cex=1, font=2, labels=tumor)
xat = c(1.5,2.5,3.5)
ylim[1] = -5
ylim[2] = 45
segments(xat, ylim[1], xat, ylim[2], col="#eeeeee", lwd=1.4, xpd=T)
segments(xat, ylim[1], xat, 0, col="#b1b1b1", lwd=1.4, xpd=T)

segments(xlim[1], yat, xlim[2], col="#eeeeee", lwd=1.4, xpd=T)
segments(xlim[1], ylim[1], xlim[1], ylim[2], col="#b1b1b1", lwd=1.4, xpd=T)
#segments(xlim[1], ylim[1], xlim[2], ylim[1], col="#b1b1b1", lwd=1.4, xpd=T)
segments(xlim[1], 0, xlim[2], 0, col="#b1b1b1", lwd=1.4, xpd=T)
segments(3.5, 0, 3.5, ylim[2], col="#b1b1b1", lwd=1.4, xpd=T, lty=3)
for (i in 1:nrow(data)) {
  col = "#333333"
  #rect(i-0.4,0,i,as.numeric(data[i,5]), border=NA, col=col)
  arrows(i-0.2+0.075, as.numeric(data[i,6]), i-0.2+0.075, as.numeric(data[i,7]), code=3, angle=90, length=0.05, col="#b1b1b1", lwd=1.4)
  points(i-0.2+0.075,as.numeric(data[i,5]), border=NA, col="white", pch=16)
  points(i-0.2+0.075,as.numeric(data[i,5]), border=NA, col=col, pch=1, cex=0.9)
  
  #rect(i+0,0,i+0.4,as.numeric(data[i,2]), border=NA, col=col)
  
  arrows(i+0.2-0.075, as.numeric(data[i,3]), i+0.2-0.075, as.numeric(data[i,4]), code=3, angle=90, length=0.05, col="#b1b1b1", lwd=1.4)
  points(i+0.2-0.075,as.numeric(data[i,2]), border=NA, col=col, pch=16)
  
  
  #m = max(as.numeric(data[i,c(4,7)]))
  #segments(i+0.2-0.075, as.numeric(data[i,4])+5, i+0.2-0.075, m+7.5, col="#333333", lwd=1.4)
  #segments(i-0.2+0.075, as.numeric(data[i,7])+5, i-0.2+0.075, m+7.5, col="#333333", lwd=1.4)
  #segments(i+0.2-0.075, m+7.5, i-0.2+0.075, m+7.5, col="#333333", lwd=1.4)
  #if(i==4) {
  #  text(i,m+5,labels="*",cex=0.9,xpd=T,pos=3)  
  #}
  #else {
  #  text(i,m+7.5,labels="n/s",cex=0.9,xpd=T,pos=3)  
  #}
  
  
  text(i,-15,labels=data[i,1],cex=0.9,xpd=T)
}


points(4.95,30, border=NA, col=col, pch=1, cex=0.9, xpd=T)
text(5,33,labels="classic", font=3,cex=0.9,xpd=T, pos=4)
text(5,27,labels="model", font=3,cex=0.9,xpd=T, pos=4)


points(4.95,10, border=NA, col=col, pch=16, xpd=T)
text(5,13,labels="adjusted", font=3,cex=0.9,xpd=T, pos=4)
text(5,7,labels="model", font=3,cex=0.9,xpd=T, pos=4)
#i=1
#rect(i-0.4,-30,i+0.4,-37.5, border=NA, col=col, xpd=T)
#text(i+0.3,-33.75,labels="B cells", cex=0.9,xpd=T, pos=4)

dev.off()
system(paste("open",file))




