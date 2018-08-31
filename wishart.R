#based on page 257 of Hoff
rwish=function(nu,M){
  p=nrow(M)
  z=matrix(rnorm(nu*p),nrow=nu,ncol=p)
  y=z%*%chol(M)
  t(y)%*%y
}

#mean nu*M
nsim=1000
nsites=10
M=diag(runif(nsites,min=0,max=10),nsites)
nu=6
res=matrix(NA,nsim,nsites*nsites)
for (i in 1:nsim){
  res[i,]=rwish(nu,M)
}
estim=colMeans(res)
plot(estim,nu*M)
