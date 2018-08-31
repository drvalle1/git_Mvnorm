rm(list=ls(all=TRUE))
set.seed(123120)

setwd('U:\\GIT_models\\git_Mvnorm')
source('gibbs functions.R')
source('MVnorm_gap function.R')

y=data.matrix(read.csv('fake data.csv',as.is=T))
impacted=paste0('water.level',1:3)
impacted.sites=which(colnames(y)%in%impacted)

#remove year and month columns
ind=which(colnames(y)%in%c('month','year'))
y1=y[,-ind]

#if needed, transform variables to ensure marginal normal distribution
z=transf.normal(y1)
hist(apply(z,2,mean,na.rm=T))
hist(apply(z,2,var,na.rm=T))
image(z)
for (i in 1:ncol(z)) hist(z[,i]);

ngibbs=1000
res=mvnorm.gap(y=z,ind.impact.sites=impacted.sites,ngibbs=ngibbs)

setwd('U:\\GIT_models\\git_Mvnorm\\fake data results')
seq1=1:ngibbs#seq(from=ngibbs/2,to=ngibbs,length.out=1000)
write.csv(res$betas[seq1,],'betas.csv',row.names=F)
write.csv(res$pred[seq1,],'predict.csv',row.names=F)
write.csv(res$Sigma[seq1,],'Sigma.csv',row.names=F)
