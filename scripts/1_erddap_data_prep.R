library(lubridate)
library(rerddap)

setwd("~/Documents/R/Github/calcofi_spf/data")

### connecting to ERDDAP to retrieve data
calcofi_eggs <- info('erdCalCOFIeggcnt')

### subset for anchovy/sardine, station and line, and pairovet only
# pairovet=T
# anch_eggs <- tabledap(calcofi_eggs,'calcofi_species_code=31','line>=76.7','line<=93.3','station<=120','net_type="PV"')
# sard_eggs <- tabledap(calcofi_eggs,'calcofi_species_code=19','line>=76.7','line<=93.3','station<=120','net_type="PV"')

### bongo nets only
pairovet=F
anch_eggs_c1 <- tabledap(calcofi_eggs,'calcofi_species_code=31','line>=76.7','line<=93.3','station<=120','net_type="CB"')
anch_eggs_cb <- tabledap(calcofi_eggs,'calcofi_species_code=31','line>=76.7','line<=93.3','station<=120','net_type="C1"')
anch_eggs <- rbind(anch_eggs_c1,anch_eggs_cb)
sard_eggs_cb <- tabledap(calcofi_eggs,'calcofi_species_code=19','line>=76.7','line<=93.3','station<=120','net_type="CB"')
sard_eggs_c1 <- tabledap(calcofi_eggs,'calcofi_species_code=19','line>=76.7','line<=93.3','station<=120','net_type="C1"')
sard_eggs <- rbind(sard_eggs_c1,sard_eggs_cb)

anch_eggs$longitude <- as.numeric(anch_eggs$longitude)
anch_eggs$latitude <- as.numeric(anch_eggs$latitude)
anch_eggs$eggs_10m2 <- as.numeric(anch_eggs$eggs_10m2)
sard_eggs$longitude <- as.numeric(sard_eggs$longitude)
sard_eggs$latitude <- as.numeric(sard_eggs$latitude)
sard_eggs$eggs_10m2 <- as.numeric(sard_eggs$eggs_10m2)

### make time
anch_eggs$time <- ymd_hms(anch_eggs$time)
sard_eggs$time <- ymd_hms(sard_eggs$time)

### make station-line factors
anch_eggs$statline <- as.factor(paste(anch_eggs$line,anch_eggs$station,sep=' '))
sard_eggs$statline <- as.factor(paste(sard_eggs$line,sard_eggs$station,sep=' '))

### keep only standard stations
calcofi_stations <- read.csv('CalCOFI75.csv')
calcofi_stations$statline <- as.factor(paste(calcofi_stations$Line,calcofi_stations$Station,sep=' '))

ind <- is.element(anch_eggs$statline,calcofi_stations$statline)
anch_eggs <- anch_eggs[ind,]
ind <- is.element(sard_eggs$statline,calcofi_stations$statline)
sard_eggs <- sard_eggs[ind,]

### saving data for interpolation later
setwd("~/Documents/R/Github/calcofi_spf/data")
write.csv(anch_eggs,'anch_eggs.csv',row.names=F)
write.csv(sard_eggs,'sard_eggs.csv',row.names=F)


### -------------- subset for Santa Barbara Channel (SBC) --------------
latbox_s <- 34.022787
latbox_n <- 34.449671
lonbox_e <- (-119.251099)
lonbox_w <- (-120.437622)

ind <- which(anch_eggs$latitude>=latbox_s & 
               anch_eggs$latitude<=latbox_n & 
               anch_eggs$longitude>=lonbox_w & 
               anch_eggs$longitude<=lonbox_e)
anch_sbc <- anch_eggs[ind,]

ind <- which(sard_eggs$latitude>=latbox_s & 
               sard_eggs$latitude<=latbox_n & 
               sard_eggs$longitude>=lonbox_w & 
               sard_eggs$longitude<=lonbox_e)
sard_sbc <- sard_eggs[ind,]


# dummy year month
if(pairovet==T){
  inx=1985
} else {
  inx=1951
}
yr_mth <- expand.grid(1:12,inx:2018)
names(yr_mth) <- c('month','year')

### aggregate by year and month
anch_sbc_mean <- aggregate(anch_sbc$eggs_10m2,
                           by=list(year(anch_sbc$time),month(anch_sbc$time)),
                           mean,na.rm=T)
names(anch_sbc_mean) <- c('year','month','eggs_10m2')
anch_sbc_mean <- merge(anch_sbc_mean,yr_mth,by=c('year','month'),all.y = T)

sard_sbc_mean <- aggregate(sard_sbc$eggs_10m2,
                           by=list(year(sard_sbc$time),month(sard_sbc$time)),
                           mean,na.rm=T)
names(sard_sbc_mean) <- c('year','month','eggs_10m2')
sard_sbc_mean <- merge(sard_sbc_mean,yr_mth,by=c('year','month'),all.y = T)

### average over spawning seasons for respective species (anchovy: Jan-May, sardine: May-Sep)
anch_sbc_yrmean <- rep(NA,length(inx:2018))
sard_sbc_yrmean <- rep(NA,length(inx:2018))
for(i in sort(unique(yr_mth$year))){
  temp <- anch_sbc_mean[which(anch_sbc_mean$year==i),]
  anch_sbc_yrmean[i-(inx-1)] <- mean(temp$eggs_10m2[1:5],na.rm=T)
  temp2 <- sard_sbc_mean[which(sard_sbc_mean$year==i),]
  sard_sbc_yrmean[i-(inx-1)] <- mean(temp2$eggs_10m2[3:9],na.rm=T)
}

eggs_sbc_yrmth <- data_frame(year=anch_sbc_mean$year,
                         month=anch_sbc_mean$month,
                         anch_eggs_10m2=anch_sbc_mean$eggs_10m2,
                         sard_eggs_10m2=sard_sbc_mean$eggs_10m2)

eggs_sbc_yr <- data.frame(year=sort(unique(yr_mth$year)),
                      anch_eggs_10m2=anch_sbc_yrmean,
                      sard_eggs_10m2=sard_sbc_yrmean)

setwd("~/Documents/R/Github/calcofi_spf/data")
write.csv(eggs_sbc_yrmth,'eggs_sbc_yrmth.csv',row.names = F)
write.csv(eggs_sbc_yr,'eggs_sbc_yr.csv',row.names = F)

### -------------- core CalCOFI region --------------

### aggregate by year and month
anch_all_mean <- aggregate(anch_eggs$eggs_10m2,
                           by=list(year(anch_eggs$time),month(anch_eggs$time)),
                           mean,na.rm=T)
names(anch_all_mean) <- c('year','month','eggs_10m2')
anch_all_mean <- merge(anch_all_mean,yr_mth,by=c('year','month'),all.y = T)

sard_all_mean <- aggregate(sard_eggs$eggs_10m2,
                           by=list(year(sard_eggs$time),month(sard_eggs$time)),
                           mean,na.rm=T)
names(sard_all_mean) <- c('year','month','eggs_10m2')
sard_all_mean <- merge(sard_all_mean,yr_mth,by=c('year','month'),all.y = T)

### average over spawning seasons for respective species (anchovy: Jan-May, sardine: May-Sep)
anch_all_yrmean <- rep(NA,length(inx:2018))
sard_all_yrmean <- rep(NA,length(inx:2018))
for(i in sort(unique(yr_mth$year))){
  temp <- anch_all_mean[which(anch_all_mean$year==i),]
  anch_all_yrmean[i-(inx-1)] <- mean(temp$eggs_10m2[1:5],na.rm=T)
  temp2 <- sard_all_mean[which(sard_all_mean$year==i),]
  sard_all_yrmean[i-(inx-1)] <- mean(temp2$eggs_10m2[3:9],na.rm=T)
}

eggs_all_yrmth <- data_frame(year=anch_all_mean$year,
                             month=anch_all_mean$month,
                             anch_eggs_10m2=anch_all_mean$eggs_10m2,
                             sard_eggs_10m2=sard_all_mean$eggs_10m2)

eggs_all_yr <- data.frame(year=sort(unique(yr_mth$year)),
                          anch_eggs_10m2=anch_all_yrmean,
                          sard_eggs_10m2=sard_all_yrmean)

setwd("~/Documents/R/Github/calcofi_spf/data")
write.csv(eggs_all_yrmth,'eggs_all_yrmth.csv',row.names = F)
write.csv(eggs_all_yr,'eggs_all_yr.csv',row.names = F)
