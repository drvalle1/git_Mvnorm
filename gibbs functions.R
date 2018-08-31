transf.normal=function(mat){
  for (i in 1:ncol(mat)){
    tmp=mat[,i]
    tmp1=tmp[!is.na(tmp)]
    n=length(tmp1)
    tmp2=rep(NA,n)
    for (j in 1:n){
      tmp2[j]=mean(tmp1 <= tmp1[j])
    }
    mat[!is.na(tmp),i]=qnorm((n/(n+1))*tmp2)
  }
  mat
}
#-------------------------------
rwish=function(nu,M){
  p=nrow(M)
  z=matrix(rnorm(nu*p),nrow=nu,ncol=p)
  y=z%*%chol(M)
  t(y)%*%y
}
#-------------------------------
sample.Sigma=function(param=param,S0=S0,v0=v0,ntime=ntime,nsites=nsites){
  #based on page 113 of Hoff
  v1=v0+ntime
  err=param$y-matrix(param$betas,ntime,nsites,byrow=T)
  soma=t(err)%*%err
  
  # tmp=matrix(0,nsites,nsites)
  # for (i in 1:ntime) tmp=tmp+err[i,]%*%t(err[i,])
  # plot(soma,tmp)
  # unique(soma-tmp)
  
  S1=S0+soma
  prec=rwish(nu=v1,M=solve(S1))
  solve(prec)
}
#-------------------------------
rmvnorm1=function (n, sigma, pre0.9_9994 = FALSE) 
{
#   retval <- chol(sigma, pivot = TRUE)
#   o <- order(attr(retval, "pivot"))
#   retval <- retval[, o]
  s. <- svd(sigma)
  if (!all(s.$d >= -sqrt(.Machine$double.eps) * abs(s.$d[1]))) {
    warning("sigma is numerically not positive definite")
  }
  R = t(s.$v %*% (t(s.$u) * sqrt(s.$d)))
  retval <- matrix(rnorm(n * ncol(sigma)), nrow = n, byrow = !pre0.9_9994) %*% R
  retval
}
#----------------------------
#sample location specific mean parameters
sample.betas=function(param,ntime,nsites){
  invSigma=solve(param$Sigma)
  prec=ntime*invSigma+diag(1,nsites)
  var1=solve(prec)
  
  pmedia=invSigma%*%colSums(param$y)
  t(rmvnorm1(1,var1))+var1%*%pmedia
}
#-----------------------------------
make.predict=function(param,ntime,nsites,y.orig){
  Var1=param$Sigma
  media=matrix(param$betas,ntime,nsites,byrow=T)
  res=y1.orig=y.orig
  for (i in 1:ntime){
    cond=is.na(y1.orig[i,])
    if (!(sum(cond)%in%c(0,nsites))){ #either all data are present or all are missing
      mis=which(cond)
      obs=which(!cond)
      tmp=crmvnorm(media[i,],Var1,obs,mis,y1.orig[i,])      
      res[i,cond]=tmp
    }
  }
  res
}
#-----------------------------------
#conditional multivariate normal
crmvnorm=function(media,Var1,obs,mis,y){
  Sigma.obs=Var1[obs,obs]
  if (length(obs)==1) Sigma.obs=matrix(Sigma.obs,1,1)
  invSigma.obs=solve(Sigma.obs)
  
  Sigma.mis=Var1[mis,mis]
  if (length(mis)==1) Sigma.mis=matrix(Sigma.mis,1,1)

  Sigma.mis.obs=Var1[mis,obs]
  if (length(mis)==1 | length(obs)==1) Sigma.mis.obs=matrix(Sigma.mis.obs,length(mis),length(obs))
  
  media1=media[mis]+Sigma.mis.obs%*%invSigma.obs%*%(y[obs]-media[obs])
  var1=Sigma.mis-Sigma.mis.obs%*%invSigma.obs%*%t(Sigma.mis.obs)
  t(rmvnorm1(1,var1))+media1
}