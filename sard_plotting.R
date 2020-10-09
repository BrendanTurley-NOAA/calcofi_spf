setwd('~/Desktop/professional/projects/side_projects/spf_eggs/update_2020/data')

sard_head <- read.csv('sardine_assessment_data.csv',nrows=1)
sard <- read.csv('sardine_assessment_data.csv',header=F,skip=2)

plot(sard$V1,sard[,2],typ='l',lwd=2)
for(i in 3:ncol(sard)){
  points(sard$V1,sard[,i],typ='l',col=i,lwd=2)
}
points(sard$V1,apply(sard,1,mean,na.rm=T),typ='l',col='purple',lwd=2,lty=2)

plot(sard$V1,sard[,2],typ='l',lwd=2)
points(sard$V1,sard[,7],typ='l',lwd=2,col=2)


assessment <- read.csv('assessment_output.csv',nrows=5)
spp_assess <- read.csv('assessment_output.csv',skip=5)

plot(spp_assess$year,spp_assess[,6],typ='l',lwd=2)
points(spp_assess$year,spp_assess[,2],typ='l',lwd=2,col=3)
