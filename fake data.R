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
rho=0.9
sigma2=1
cor1.true=cor1=rho^aux1
Sigma.true=Sigma=sigma2*cor1

#betas
betas.true=betas=runif(nvariables)

#generate latent z variables
z=rmvnorm(ntime,mean=betas,sigma=Sigma)
colnames(z)=c(paste0('water.level',1:4),
              paste0('precip',1:2),
              paste0('temperature',1:4))

#generate data where margins are gamma distributions
a=runif(nvariables,min=0,max=10)
b=runif(nvariables,min=0,max=10)
y=matrix(NA,ntime,nvariables)
seq1=seq(from=0.001,to=100,length.out=10000)
for (i in 1:nvariables){
  tmp=pnorm(z[,i],mean=betas.true[i],sd=sqrt(sigma2))
  y[,i]=qgamma(tmp,a[i],b[i])

  #look at marginal distributions
  hist(y[,i],freq=F)
  lines(seq1,dgamma(seq1,a[i],b[i]),col='red')
}

setwd('U:\\GIT_models\\git_Mvnorm')
write.csv(y,'fake data complete.csv',row.names=F)

#eliminate data from impacted sites
cond=year1>1984
y[cond,1:3]=NA
colnames(y)=colnames(z)

#make additional holes in the data
ind=sample(1:(ntime*nvariables),size=0.2*ntime*nvariables)
y[ind]=NA
image(y)

#prepare final dataset
tmp=data.frame(year=year1,month=month)
y1=cbind(y,tmp)

#export results
write.csv(y1,'fake data.csv',row.names=F)
write.csv(betas.true,'fake data betas.csv',row.names=F)
write.csv(Sigma.true,'fake data Sigma.csv',row.names=F)