

setwd("~/Documents/R/Github/calcofi_spf/data")

### load aggregated CalCOFI egg data
eggs_all_yr <- read.csv('eggs_all_yr.csv')
eggs_sbc_yr <- read.csv('eggs_sbc_yr.csv')

### load stock assessment output
stock_assess_meta <- read.csv('assessment_output.csv',nrows=5)

stock_assess <- read.csv('assessment_output.csv',skip=5,nrows=68)
names(stock_assess) <- c('year',
                         'sard_best',
                         'sard_2020',
                         'sard_2009',
                         'sard_1991',
                         'anch_2017')

### correlation analysis

### 1 - stock versus core CalCOFI region
# anchovy
cor.test(stock_assess$anch_2017,eggs_all_yr$anch_eggs_10m2,method='spearman')
plot(stock_assess$anch_2017,eggs_all_yr$anch_eggs_10m2,log='xy')
# sardine
cor.test(stock_assess$sard_best,eggs_all_yr$sard_eggs_10m2,method='spearman')
plot(stock_assess$sard_best,eggs_all_yr$sard_eggs_10m2+1,log='xy')

### 2- stock versus Santa Barbara Channel (SBC)
# anchovy
cor.test(stock_assess$anch_2017,eggs_sbc_yr$anch_eggs_10m2,method='spearman')
plot(stock_assess$anch_2017,eggs_sbc_yr$anch_eggs_10m2+1,log='xy')
# sardine
cor.test(stock_assess$sard_best,eggs_sbc_yr$sard_eggs_10m2,method='spearman')
plot(stock_assess$sard_best,eggs_sbc_yr$sard_eggs_10m2+1,log='xy')



### combined plot
par(mfrow=c(2,2))
plot(log(stock_assess$anch_2017,base=10),
     log(eggs_all_yr$anch_eggs_10m2+1,base=10),
     xlab='Log(anchovy biomass)',
     ylab='Log(anchovy eggs 10m2)',
     pch=21, las=1, col='white',bg='gray40')

plot(log(stock_assess$anch_2017,base=10),
     log(eggs_sbc_yr$anch_eggs_10m2+1,base=10),
     xlab='Log(anchovy biomass)',
     ylab='Log(anchovy eggs 10m2)',
     pch=21, las=1, col='white',bg='gray40')

plot(log(stock_assess$sard_best,base=10),
     log(eggs_all_yr$sard_eggs_10m2+1,base=10),
     xlab='Log(sardine biomass)',
     ylab='Log(sardine eggs 10m2)',
     pch=21, las=1, col='white',bg='gray40')

plot(log(stock_assess$sard_best,base=10),
     log(eggs_sbc_yr$sard_eggs_10m2+1,base=10),
     xlab='Log(sardine biomass)',
     ylab='Log(sardine eggs 10m2)',
     pch=21, las=1, col='white',bg='gray40')



### interpolating maps for 10 years with highest biomass versus 10 years with lowest
anch_order <- order(stock_assess$anch_2017,decreasing = T)
sard_order <- order(stock_assess$sard_best,decreasing = T)

stock_assess$year[anch_order]
stock_assess$year[sard_order]
