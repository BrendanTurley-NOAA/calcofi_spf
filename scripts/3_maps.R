library(akima)
library(lubridate)
library(rgdal)

### load map
setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
world <- readOGR('ne_10m_admin_0_countries.shp')

### interpolating maps for 10 years with highest biomass versus 10 years with lowest

### keep only standard stations
setwd("~/Documents/R/Github/calcofi_spf/data")
calcofi_stations <- read.csv('CalCOFI75.csv')
calcofi_stations$statline <- as.factor(paste(calcofi_stations$Line,calcofi_stations$Station,sep=' '))
calcofi_stations$Longitude <- (-calcofi_stations$Longitude)

### load stock assessment output
stock_assess_meta <- read.csv('assessment_output.csv',nrows=5)

stock_assess <- read.csv('assessment_output.csv',skip=5,nrows=68)
names(stock_assess) <- c('year',
                         'sard_best',
                         'sard_2020',
                         'sard_2009',
                         'sard_1991',
                         'anch_2017')

anch_order <- order(stock_assess$anch_2017,decreasing = T)
sard_order <- order(stock_assess$sard_best,decreasing = T)

anch_hilo <- stock_assess$year[anch_order]
sard_hilo <- stock_assess$year[sard_order]

### load egg data from erddap
anch_eggs <- read.csv('anch_eggs.csv')
sard_eggs <- read.csv('sard_eggs.csv')

### make time
anch_eggs$time <- ymd_hms(anch_eggs$time)
sard_eggs$time <- ymd_hms(sard_eggs$time)

### make station-line factors
anch_eggs$statline <- as.factor(anch_eggs$statline)
sard_eggs$statline <- as.factor(sard_eggs$statline)



### anchovy
head(anch_hilo,10)
tail(anch_hilo,10)
### high biomass
ind <- which(is.element(year(anch_eggs$time),head(anch_hilo,10)) & # subset for years of highest biomass
                  is.element(month(anch_eggs$time),1:5)) # subset for spawning season
anch_hi <- anch_eggs[ind,]
### low biomass
ind <- which(is.element(year(anch_eggs$time),tail(anch_hilo,10)) & # subset for years of highest biomass
               is.element(month(anch_eggs$time),1:5)) # subset for spawning season
anch_lo <- anch_eggs[ind,]

### sardine
head(sard_hilo,10)
tail(sard_hilo,10)
### high biomass
ind <- which(is.element(year(sard_eggs$time),head(sard_hilo,10)) & # subset for years of highest biomass
               is.element(month(sard_eggs$time),1:5)) # subset for spawning season
sard_hi <- sard_eggs[ind,]
### low biomass
ind <- which(is.element(year(sard_eggs$time),tail(sard_hilo,10)) & # subset for years of highest biomass
               is.element(month(sard_eggs$time),1:5)) # subset for spawning season
sard_lo <- sard_eggs[ind,]


### interpolate
### set resolution for interpolation
resolution <- .125
### function for aggregation
func <- mean

anch_hi_mean <- aggregate(anch_hi$eggs_10m2,by=list(anch_hi$statline),func,na.rm=T)
names(anch_hi_mean) <- c('statline','eggs_10m2')
anch_hi_mean <- merge(anch_hi_mean,calcofi_stations,by=c('statline'),all=T)
ind <- which(is.na(anch_hi_mean$eggs_10m2))
anch_hi_mean <- anch_hi_mean[-ind,]
anch_hi_interp <- interp(anch_hi_mean$Longitude,anch_hi_mean$Latitude,log(anch_hi_mean$eggs_10m2+1,base=10),
                         xo=seq(floor(min(anch_hi_mean$Longitude)), ceiling(max(anch_hi_mean$Longitude)), resolution),
                         yo=seq(floor(min(anch_hi_mean$Latitude)), ceiling(max(anch_hi_mean$Latitude)), resolution),
                         jitter = 10^-1, jitter.iter = 6,duplicate = 'mean')


anch_lo_mean <- aggregate(anch_lo$eggs_10m2,by=list(anch_lo$statline),func,na.rm=T)
names(anch_lo_mean) <- c('statline','eggs_10m2')
anch_lo_mean <- merge(anch_lo_mean,calcofi_stations,by=c('statline'),all=T)
ind <- which(is.na(anch_lo_mean$eggs_10m2))
anch_lo_mean <- anch_lo_mean[-ind,]
anch_lo_interp <- interp(anch_lo_mean$Longitude,anch_lo_mean$Latitude,log(anch_lo_mean$eggs_10m2+1,base=10),
                         xo=seq(floor(min(anch_lo_mean$Longitude)), ceiling(max(anch_lo_mean$Longitude)), resolution),
                         yo=seq(floor(min(anch_lo_mean$Latitude)), ceiling(max(anch_lo_mean$Latitude)), resolution),
                         jitter = 10^-1, jitter.iter = 6,duplicate = 'mean')



sard_hi_mean <- aggregate(sard_hi$eggs_10m2,by=list(sard_hi$statline),func,na.rm=T)
names(sard_hi_mean) <- c('statline','eggs_10m2')
sard_hi_mean <- merge(sard_hi_mean,calcofi_stations,by=c('statline'),all=T)
ind <- which(is.na(sard_hi_mean$eggs_10m2))
sard_hi_mean <- sard_hi_mean[-ind,]
sard_hi_interp <- interp(sard_hi_mean$Longitude,sard_hi_mean$Latitude,log(sard_hi_mean$eggs_10m2+1,base=10),
                         xo=seq(floor(min(sard_hi_mean$Longitude)), ceiling(max(sard_hi_mean$Longitude)), resolution),
                         yo=seq(floor(min(sard_hi_mean$Latitude)), ceiling(max(sard_hi_mean$Latitude)), resolution),
                         jitter = 10^-1, jitter.iter = 6,duplicate = 'mean')


sard_lo_mean <- aggregate(sard_lo$eggs_10m2,by=list(sard_lo$statline),func,na.rm=T)
names(sard_lo_mean) <- c('statline','eggs_10m2')
sard_lo_mean <- merge(sard_lo_mean,calcofi_stations,by=c('statline'),all=T)
ind <- which(is.na(sard_lo_mean$eggs_10m2))
sard_lo_mean <- sard_lo_mean[-ind,]
sard_lo_interp <- interp(sard_lo_mean$Longitude,sard_lo_mean$Latitude,log(sard_lo_mean$eggs_10m2+1,base=10),
                         xo=seq(floor(min(sard_lo_mean$Longitude)), ceiling(max(sard_lo_mean$Longitude)), resolution),
                         yo=seq(floor(min(sard_lo_mean$Latitude)), ceiling(max(sard_lo_mean$Latitude)), resolution),
                         jitter = 10^-1, jitter.iter = 6,duplicate = 'mean')


### make maps
cols <- colorRampPalette(c('#172935','#154C53','#167168','#3B976F','#76BC6B','#BFDC63'))


brks <- seq(0,3.5,.5)
cols <- cols(length(brks)-1)

setwd("~/Documents/R/Github/calcofi_spf/figures")
png('spf_biomass_comparison.png',height=8.25,width=10,units='in',res=300)
par(mfrow=c(2,2))
plot(1,1,xlim=c(-125,-117),ylim=c(30,35),xlab='',ylab='',las=1,asp=1)
image(anch_hi_interp,
      breaks=c(3.5,10),col=cols[length(cols)],
      las=1,asp=1,add=T)
image(anch_hi_interp,
      breaks=brks,col=cols,
      las=1,asp=1,add=T)
points(calcofi_stations$Longitude,calcofi_stations$Latitude,
       col='purple',cex=.5,lwd=1.2,pch=20)
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,2.5)
mtext(expression(paste('Latitude (',degree,'N)')),2,2)
mtext('10 highest biomass years')

plot(1,1,xlim=c(-125,-117),ylim=c(30,35),xlab='',ylab='',las=1,asp=1)
image(anch_lo_interp,
      breaks=brks,col=cols,
      las=1,asp=1,add=T)
points(calcofi_stations$Longitude,calcofi_stations$Latitude,
       col='purple',cex=.5,lwd=1.2,pch=20)
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,2.5)
mtext(expression(paste('Latitude (',degree,'N)')),2,2)
mtext('10 lowest biomass years')

plot(1,1,xlim=c(-125,-117),ylim=c(30,35),xlab='',ylab='',las=1,asp=1)
image(sard_hi_interp,
      breaks=brks,col=cols,
      las=1,asp=1,add=T)
points(calcofi_stations$Longitude,calcofi_stations$Latitude,
       col='purple',cex=.5,lwd=1.2,pch=20)
plot(world,col='gray70',add=T)
mtext(expression(paste('Longitude (',degree,'W)')),1,2.5)
mtext(expression(paste('Latitude (',degree,'N)')),2,2)

plot(1,1,xlim=c(-125,-117),ylim=c(30,35),xlab='',ylab='',las=1,asp=1)
image(sard_lo_interp,
      breaks=brks,col=cols,
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


lth <- length(brks)-1
png("colorbar.png", height = 5, width = 1.5, units = 'in', res=300)
par(mar=c(1,1,1,5))
image(1:2,1:length(brks),t(matrix(c(seq(1,lth),seq(1,lth)),lth,2)),col=cols,xaxt='n',yaxt='n',xlab='',ylab='')
mtext(expression('Log'[10]*'(biomass [metric tons])'),4,line=3)
axis(4,seq(0,7,1)+1,labels=brks,las=1)
dev.off()


