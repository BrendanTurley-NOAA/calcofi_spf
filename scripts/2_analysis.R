

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
method <- 'spearman'
### 1 - stock versus core CalCOFI region
# anchovy
anch_cor_w <- cor.test(stock_assess$anch_2017,eggs_all_yr$anch_eggs_10m2,method=method)
# sardine
sard_cor_w <- cor.test(stock_assess$sard_best,eggs_all_yr$sard_eggs_10m2,method=method)

### 2- stock versus Santa Barbara Channel (SBC)
# anchovy
anch_cor_sbc <- cor.test(stock_assess$anch_2017,eggs_sbc_yr$anch_eggs_10m2,method=method)
# sardine
sard_cor_sbc <- cor.test(stock_assess$sard_best,eggs_sbc_yr$sard_eggs_10m2,method=method)

### export results to table
cor_table <- matrix(NA,4,2)
cor_table[1,] <- cbind(anch_cor_w$estimate,anch_cor_w$p.value)
cor_table[2,] <- cbind(sard_cor_w$estimate,sard_cor_w$p.value)
cor_table[3,] <- cbind(anch_cor_sbc$estimate,anch_cor_sbc$p.value)
cor_table[4,] <- cbind(sard_cor_sbc$estimate,sard_cor_sbc$p.value)
colnames(cor_table) <- c('rho','p-value')
row.names(cor_table) <- c('anchovy whole','sardine whole','anchovy sbc','sardine sbc')
# setwd("~/Documents/R/Github/calcofi_spf/results")
# write.csv(cor_table,'correlation_results.csv')

### combined plot

setwd("~/Documents/R/Github/calcofi_spf/figures")
png('fig1_corr_comparison.png',height=8,width=8,units='in',res=300)
par(mfrow=c(2,2),mar=c(5,4.5,2,1))
plot((stock_assess$anch_2017),
     (eggs_all_yr$anch_eggs_10m2+1),
     xlab="Anchovy biomass (metric tons)",
     ylab=expression('Anchovy eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='xy')
mtext('Core CalCOFI region')

plot((stock_assess$anch_2017),
     (eggs_sbc_yr$anch_eggs_10m2+1),
     xlab="Anchovy biomass (metric tons)",
     ylab=expression('Anchovy eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='xy')
mtext('Santa Barbara Channel')

plot((stock_assess$sard_best),
     (eggs_all_yr$sard_eggs_10m2+1),
     xlab="Sardine biomass (metric tons)",
     ylab=expression('Sardine eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='xy')

plot((stock_assess$sard_best),
     (eggs_sbc_yr$sard_eggs_10m2+1),
     xlab="Sardine biomass (metric tons)",
     ylab=expression('Sardine eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='xy')
dev.off()


### alternate plot with srdine assessment years different markers
setwd("~/Documents/R/Github/calcofi_spf/figures")
png('fig1_corr_comparison_1.png',height=8,width=8,units='in',res=300)
par(mfrow=c(2,2),mar=c(5,4.5,2,1))
plot((stock_assess$anch_2017),
     (eggs_all_yr$anch_eggs_10m2+1),
     xlab="Anchovy biomass (metric tons)",
     ylab=expression('Anchovy eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='xy')
mtext('Core CalCOFI region')

plot((stock_assess$anch_2017),
     (eggs_sbc_yr$anch_eggs_10m2+1),
     xlab="Anchovy biomass (metric tons)",
     ylab=expression('Anchovy eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='xy')
mtext('Santa Barbara Channel')

plot((stock_assess$sard_best),
     (eggs_all_yr$sard_eggs_10m2+1),
     xlab="Sardine biomass (metric tons)",
     ylab=expression('Sardine eggs 10m'^-2),
     col='white',
     las=1, log='xy')
points((stock_assess$sard_best[1:13]),
       (eggs_all_yr$sard_eggs_10m2[1:13]+1),
       pch=24, cex=1.25, col='gray20',bg='gray100')
points((stock_assess$sard_best[31:54]),
       (eggs_all_yr$sard_eggs_10m2[31:54]+1),
       pch=23, cex=1.5, col='white',bg='gray50')
points((stock_assess$sard_best[55:68]),
       (eggs_all_yr$sard_eggs_10m2[55:68]+1),
       pch=21, cex=1.5, col='white',bg='gray20')
legend('topleft',c('1991','2009','2019'),pch=c(24,23,21),
       col=c('gray20','white','white'),
       pt.bg=c('gray100','gray50','gray20'),
       bty='n',pt.cex=1.5)

plot((stock_assess$sard_best),
     (eggs_sbc_yr$sard_eggs_10m2+1),
     xlab="Sardine biomass (metric tons)",
     ylab=expression('Sardine eggs 10m'^-2),
     col='white',
     las=1, log='xy')
points((stock_assess$sard_best[1:13]),
       (eggs_sbc_yr$sard_eggs_10m2[1:13]+1),
       pch=24, cex=1.25, col='gray20',bg='gray100')
points((stock_assess$sard_best[31:54]),
       (eggs_sbc_yr$sard_eggs_10m2[31:54]+1),
       pch=23, cex=1.5, col='white',bg='gray50')
points((stock_assess$sard_best[55:68]),
       (eggs_sbc_yr$sard_eggs_10m2[55:68]+1),
       pch=21, cex=1.5, col='white',bg='gray20')
dev.off()


### ratio analysis
### SBC to core region versus biomass

### anchovy
anch_ratio <- eggs_sbc_yr$anch_eggs_10m2/eggs_all_yr$anch_eggs_10m2
### sardine
sard_ratio <- eggs_sbc_yr$sard_eggs_10m2/eggs_all_yr$sard_eggs_10m2

setwd("~/Documents/R/Github/calcofi_spf/figures")
png('fig3_ratio.png',height=5,width=10,units='in',res=300)
par(mfrow=c(1,2),mar=c(5,4.5,2,1))
plot(stock_assess$anch_2017,anch_ratio,
     xlab="Anchovy biomass (metric tons)",
     ylab=expression('SBC:core anchovy eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='x')
plot(stock_assess$sard_best,sard_ratio,
     xlab="Sardine biomass (metric tons)",
     ylab=expression('SBC:core sardine eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='x')
dev.off()


setwd("~/Documents/R/Github/calcofi_spf/figures")
png('fig3_ratio_1.png',height=5,width=10,units='in',res=300)
par(mfrow=c(1,2),mar=c(5,4.5,2,1))
plot(stock_assess$anch_2017,anch_ratio,
     xlab="Anchovy biomass (metric tons)",
     ylab=expression('SBC:core anchovy eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1, log='x')
plot(stock_assess$sard_best,sard_ratio,
     xlab="Sardine biomass (metric tons)",
     ylab=expression('SBC:core sardine eggs 10m'^-2),
     col='white',
     las=1, log='x')
points(stock_assess$sard_best[1:13],sard_ratio[1:13],
       pch=24, cex=1.35, col='gray20',bg='gray100')
points(stock_assess$sard_best[31:54],sard_ratio[31:54],
       pch=23, cex=1.5, col='white',bg='gray50')
points(stock_assess$sard_best[55:68],sard_ratio[55:68],
       pch=21, cex=1.5, col='white',bg='gray20')
legend('topright',c('1991','2009','2019'),pch=c(24,23,21),
       col=c('gray20','white','white'),
       pt.bg=c('gray100','gray50','gray20'),
       bty='n',pt.cex=1.5)
dev.off()


setwd("~/Documents/R/Github/calcofi_spf/figures")
png('fig3_ratio_2.png',height=5,width=10,units='in',res=300)
par(mfrow=c(1,2),mar=c(5,4.5,2,1))
plot(stock_assess$anch_2017,anch_ratio,
     xlab="Anchovy biomass (metric tons)",
     ylab=expression('SBC:core anchovy eggs 10m'^-2),
     pch=21, cex=1.5, col='white',bg='gray50',
     las=1)
plot(stock_assess$sard_best,sard_ratio,
     xlab="Sardine biomass (metric tons)",
     ylab=expression('SBC:core sardine eggs 10m'^-2),
     col='white',
     las=1)
points(stock_assess$sard_best[1:13],sard_ratio[1:13],
       pch=24, cex=1.35, col='gray20',bg='gray100')
points(stock_assess$sard_best[31:54],sard_ratio[31:54],
       pch=23, cex=1.5, col='white',bg='gray50')
points(stock_assess$sard_best[55:68],sard_ratio[55:68],
       pch=21, cex=1.5, col='white',bg='gray20')
legend('topright',c('1991','2009','2019'),pch=c(24,23,21),
       col=c('gray20','white','white'),
       pt.bg=c('gray100','gray50','gray20'),
       bty='n',pt.cex=1.5)
dev.off()

cor.test(stock_assess$anch_2017,anch_ratio,method='spearman')
cor.test(stock_assess$sard_best,sard_ratio,method='spearman')

summary(lm(anch_ratio~log10(stock_assess$anch_2017)))
summary(lm(sard_ratio~log10(stock_assess$sard_best)))

summary(lm(anch_ratio~(stock_assess$anch_2017)))
summary(lm(sard_ratio~(stock_assess$sard_best)))
