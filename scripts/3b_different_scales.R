anch_brks <- seq(0,4,.5)
anch_cols <- cols(length(anch_brks)-1)
sard_brks <- seq(0,3,.5)
sard_cols <- cols(length(sard_brks)-1)

setwd("~/Documents/R/Github/calcofi_spf/figures")
png('spf_biomass_comparison.png',height=8.25,width=10,units='in',res=300)
par(mfrow=c(2,2))
plot(1,1,xlim=c(-125,-117),ylim=c(30,35),xlab='',ylab='',las=1,asp=1)
image(anch_hi_interp,
      breaks=anch_brks,col=anch_cols,
      las=1,asp=1,add=T)
points(calcofi_stations$Longitude,calcofi_stations$Latitude,
       col='purple',cex=.5,lwd=1.2,pch=20)
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,2.5)
mtext(expression(paste('Latitude (',degree,'N)')),2,2)

plot(1,1,xlim=c(-125,-117),ylim=c(30,35),xlab='',ylab='',las=1,asp=1)
image(anch_lo_interp,
      breaks=anch_brks,col=anch_cols,
      las=1,asp=1,add=T)
points(calcofi_stations$Longitude,calcofi_stations$Latitude,
       col='purple',cex=.5,lwd=1.2,pch=20)
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,2.5)
mtext(expression(paste('Latitude (',degree,'N)')),2,2)

plot(1,1,xlim=c(-125,-117),ylim=c(30,35),xlab='',ylab='',las=1,asp=1)
image(sard_hi_interp,
      breaks=sard_brks,col=sard_cols,
      las=1,asp=1,add=T)
points(calcofi_stations$Longitude,calcofi_stations$Latitude,
       col='purple',cex=.5,lwd=1.2,pch=20)
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,2.5)
mtext(expression(paste('Latitude (',degree,'N)')),2,2)

plot(1,1,xlim=c(-125,-117),ylim=c(30,35),xlab='',ylab='',las=1,asp=1)
image(sard_lo_interp,
      breaks=sard_brks,col=sard_cols,
      las=1,asp=1,add=T)
points(calcofi_stations$Longitude,calcofi_stations$Latitude,
       col='purple',cex=.5,lwd=1.2,pch=20)
contour(sard_lo_interp,
        levels=seq(0,.05,.05),
        add=T,col='gold',
        method='simple',labels='')
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,2.5)
mtext(expression(paste('Latitude (',degree,'N)')),2,2)
# image(sard_lo_interp,asp=1,breaks=seq(0,.02,.001),col=cols(length(seq(0,.02,.001))-1))
dev.off()


lth <- length(anch_brks)-1
lth2 <- length(sard_brks)-1
png("colorbar.png", height = 10, width = 1.5, units = 'in', res=300)
par(mfrow=c(2,1),mar=c(2.5,1,2.5,5))
image(1:2,1:length(anch_brks),t(matrix(c(seq(1,lth),seq(1,lth)),lth,2)),col=anch_cols,xaxt='n',yaxt='n',xlab='',ylab='')
mtext(expression('Log'[10]*'(anchovy biomass [metric tons])'),4,line=2.5)
axis(4,seq(0,8,1)+1,labels=anch_brks,las=1)

image(1:2,1:length(sard_brks),t(matrix(c(seq(1,lth2),seq(1,lth2)),lth2,2)),col=sard_cols,xaxt='n',yaxt='n',xlab='',ylab='')
mtext(expression('Log'[10]*'(sardine biomass [metric tons])'),4,line=2.5)
axis(4,seq(0,6,1)+1,labels=sard_brks,las=1)
dev.off()
