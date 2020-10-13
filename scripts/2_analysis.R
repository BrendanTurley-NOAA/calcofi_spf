

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
anch_cor_w <- cor.test(stock_assess$anch_2017,eggs_all_yr$anch_eggs_10m2,method='spearman')
# sardine
sard_cor_w <- cor.test(stock_assess$sard_best,eggs_all_yr$sard_eggs_10m2,method='spearman')

### 2- stock versus Santa Barbara Channel (SBC)
# anchovy
anch_cor_sbc <- cor.test(stock_assess$anch_2017,eggs_sbc_yr$anch_eggs_10m2,method='spearman')
# sardine
sard_cor_sbc <- cor.test(stock_assess$sard_best,eggs_sbc_yr$sard_eggs_10m2,method='spearman')

### export results to table
cor_table <- matrix(NA,4,2)
cor_table[1,] <- cbind(anch_cor_w$estimate,anch_cor_w$p.value)
cor_table[2,] <- cbind(sard_cor_w$estimate,sard_cor_w$p.value)
cor_table[3,] <- cbind(anch_cor_sbc$estimate,anch_cor_sbc$p.value)
cor_table[4,] <- cbind(sard_cor_sbc$estimate,sard_cor_sbc$p.value)
colnames(cor_table) <- c('rho','p-value')
row.names(cor_table) <- c('anchovy whole','sardine whole','anchovy sbc','sardine sbc')
setwd("~/Documents/R/Github/calcofi_spf/results")
write.csv(cor_table,'correlation_results.csv')

### combined plot

setwd("~/Documents/R/Github/calcofi_spf/figures")
png('spf_corr_comparison.png',height=8,width=8,units='in',res=300)
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
