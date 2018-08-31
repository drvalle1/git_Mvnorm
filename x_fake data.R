rm(list=ls(all=TRUE))
set.seed(1)
library('mvtnorm')

#generate time variable
year1=rep(1970:2015,each=12)
month=rep(1:12,times=46)
ntime=length(month)

#create variance-covariance matrix
nvariables=10
aux=outer(1:nvariables,1:nvariables,'-')
aux1=abs(aux)
rho=0.75
sigma2=2
cor1.true=cor1=rho^aux1
Sigma.true=Sigma=sigma2*cor1

#betas
betas.true=betas=runif(nvariables,min=-5,max=5)

#generate data
y=rmvnorm(ntime,mean=betas,sigma=Sigma)

#generate data where margins are gamma distributions
# a=runif(nvariables,min=0,max=10)
# b=runif(nvariables,min=0,max=10)
# y=matrix(NA,ntime,nvariables)
# seq1=seq(from=0.001,to=100,length.out=10000)
# for (i in 1:nvariables){
#   for (j in 1:ntime){
#     tmp=mean(z[j,i] <= z[,i])
#     y[j,i]=qgamma(tmp,a[i],b[i])
#   }
#   #look at marginal distributions
#   hist(y[,i],freq=F)
#   lines(seq1,dgamma(seq1,a[i],b[i]),col='red')
# }

#make holes in the data
ind=sample(1:(ntime*nvariables),size=0.2*ntime*nvariables)
y[ind]=NA
image(y)

#prepare final dataset
y1=cbind(y,year1,month)
colnames(y1)=c(paste0('water.level',1:4),
               paste0('precip',1:2),
               paste0('temperature',1:4),
               'year','month')

setwd('U:\\GIT_models\\git_Mvnorm')
write.csv(y1,'fake data.csv',row.names=F)
write.csv(betas.true,'fake data betas.csv',row.names=F)
write.csv(Sigma.true,'fake data Sigma.csv',row.names=F)