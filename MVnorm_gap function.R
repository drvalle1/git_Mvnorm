mvnorm.gap=function(y,ngibbs,ind.impact.sites){
  nsites=ncol(y)
  ntime=nrow(y)
  betas=rep(0,nsites)
  Sigma=diag(1,nsites)
  
  #priors
  S0=diag(1,nsites)
  v0=nsites+2
  
  #MCMC stuff
  vec.Sigma=matrix(NA,ngibbs,nsites*nsites)
  vec.betas=matrix(NA,ngibbs,nsites)
  vec.pred=matrix(NA,ngibbs,ntime*length(ind.impact.sites))

  param=list(Sigma=Sigma,betas=betas,y=y)
  options(warn=2) #this should be 2
  for (i in 1:ngibbs){
    print(i)
    param$y=make.predict(param=param,ntime=ntime,nsites=nsites,y.orig=y)
    param$Sigma=sample.Sigma(param=param,S0=S0,v0=v0,ntime=ntime,nsites=nsites)
    param$betas=sample.betas(param=param,ntime=ntime,nsites=nsites)

    vec.pred[i,]=param$y[,ind.impact.sites]
    vec.betas[i,]=param$betas
    vec.Sigma[i,]=param$Sigma
  }
  list(pred=vec.pred,betas=vec.betas,Sigma=vec.Sigma) 
}