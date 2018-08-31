rm(list=ls(all=TRUE))
set.seed(123120)

setwd('U:\\GIT_models\\git_Mvnorm')
source('gibbs functions.R')
source('MVnorm_gap function.R')

setwd('U:/independent studies/kaplan amazon hydrology/gibbs no neg control')
y=data.matrix(read.csv('data for gibbs nnc.csv',as.is=T))
image(y)
impacted=c(2920, 2970, 2975, 2905, 2910)

#plug in NA's for impacted period of impacted sites
impacted.sites=which(colnames(y)%in%paste('w',impacted,sep=''))

#transform variables to ensure normality (look at "pseudo_data.R")
z=transf.normal(y)
hist(apply(z,2,mean,na.rm=T))
hist(apply(z,2,var,na.rm=T))
image(z)
hist(z[,1]); 
hist(z[,10]);

ngibbs=100000
res=mvnorm.gap(y=z,ind.impact.sites=impacted.sites,ngibbs=ngibbs)

#output results
setwd('U:\\independent studies\\kaplan amazon hydrology\\gibbs no neg control\\results mvnorm nnc')
seq1=seq(from=ngibbs/2,to=ngibbs,length.out=1000)
write.csv(res$betas[seq1,],'betas.csv',row.names=F)
write.csv(res$pred[seq1,],'predict.csv',row.names=F)
write.csv(res$Sigma[seq1,],'Sigma.csv',row.names=F)
