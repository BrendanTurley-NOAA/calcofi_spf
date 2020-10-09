library(akima)
library(dplyr)
library(lubridate)
library(rerddap)

setwd("~/Documents/R/Github/calcofi_spf/data")

### connecting to ERDDAP to retrieve data
calcofi_eggs <- info('erdCalCOFIeggcnt')

anch_eggs_2 <- tabledap(calcofi_eggs,'calcofi_species_code=31','line>=76.7','line<=93.3','station<=120','net_type="PV"')
sard_eggs <- tabledap(calcofi_eggs,'calcofi_species_code=19','line>=76.7','line<=93.3','station<=120','net_type="PV"')

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


# subset for Santa Barbara Channel (SBC)
latbox_s <- 34.022787
latbox_n <- 34.449671
lonbox_e <- (-119.251099)
lonbx_w <- (-120.437622)