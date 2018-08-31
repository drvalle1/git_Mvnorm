rm(list=ls(all=TRUE))
set.seed(123120)

#------------------------------------------------------------
#compare estimated to true covariance matrix
setwd('U:\\GIT_models\\git_Mvnorm')
Sigma.true=data.matrix(read.csv('fake data Sigma.csv',as.is=T))

setwd('U:\\GIT_models\\git_Mvnorm\\fake data results')
Sigma=read.csv('Sigma.csv',as.is=T)
nvariables=10
Sigma1=matrix(apply(Sigma,2,median),nvariables,nvariables)

rango=range(c(Sigma.true,Sigma1))
plot(Sigma.true,Sigma1,xlim=rango,ylim=rango)
lines(rango,rango)

#------------------------------------------------------------
#compare predictions to true values
setwd('U:\\GIT_models\\git_Mvnorm')
source('gibbs functions.R')
y=data.matrix(read.csv('fake data.csv',as.is=T))
ntime=nrow(y)

impacted=paste0('water.level',1:3)
impacted.sites=which(colnames(y)%in%impacted)
nimpact.sites=length(impacted.sites)

#remove year and month columns
ind=which(colnames(y)%in%c('month','year'))
y1=y[,-ind]

#transform using empirical cdf
z=matrix(NA,ntime,nimpact.sites)
for (j in 1:nimpact.sites){
  tmp=y1[,j]
  for (i in 1:nrow(y1)){
    z[i,j]=mean(tmp <=tmp[i],na.rm=T)
  }
}

#get predictions on the z scale
setwd('U:\\GIT_models\\git_Mvnorm\\fake data results')
pred=read.csv('predict.csv',as.is=T)
quant=apply(pred,2,median)
med=matrix(quant,ntime,nimpact.sites)

#transform these results back into the original y units
pmed=pnorm(med)

yest.lo=yest.med=yest.hi=matrix(NA,ntime,nimpact.sites)
for (i in 1:ntime){
  for (j in 1:nimpact.sites){
    diff=abs(pmed[i,j]-z[,j])
    ind=which(diff==min(diff,na.rm=T))
    if (length(ind)>1) ind=sample(ind,size=1)
    yest.med[i,j]=y1[ind,j]
  }
}

setwd('U:\\GIT_models\\git_Mvnorm')
dat.true=read.csv('fake data complete.csv',as.is=T)
ind=which(is.na(y1[,1]))

for (i in 1:3){
  rango=range(c(dat.true[ind,i],yest.med[ind,i]))
  plot(dat.true[ind,i],yest.med[ind,i],xlim=rango,ylim=rango,main=i)
  lines(rango,rango)  
}
