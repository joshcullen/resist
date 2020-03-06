sample.betas=function(betas,xmat,y,jump,nparam,b.gamma,var.betas){
  betas.old=betas.new=betas
  betas.prop=rnorm(nparam,mean=betas,sd=jump)
  prior.old =dnorm(betas.old ,mean=0,sd=sqrt(var.betas),log=T)
  prior.prop=dnorm(betas.prop,mean=0,sd=sqrt(var.betas),log=T)
  accept=rep(0,nparam)
  
  for (i in 1:nparam){
    betas.new=betas.old
    betas.new[i]=betas.prop[i]
    pold=get.llk(betas=betas.old,xmat=xmat,y=y,b.gamma=b.gamma)+prior.old[i]
    pnew=get.llk(betas=betas.new,xmat=xmat,y=y,b.gamma=b.gamma)+prior.prop[i]
    pthresh=exp(pnew-pold)
    if (runif(1)<pthresh){
      accept[i]=1
      betas.old=betas.new
    }
  }
  list(accept=accept,betas=betas.old)
}
#---------------------------------------------------
get.llk=function(betas,xmat,y,b.gamma){
  media=exp(xmat%*%betas)
  a.gamma=b.gamma*media
  sum(dgamma(y,a.gamma,b.gamma,log=T))
}
#--------------------------------------------------
sample.b.gamma=function(betas,xmat,y,jump,b.gamma){
  b.old=b.gamma
  b.new=abs(rnorm(1,mean=b.gamma,sd=jump))
  
  pold=get.llk(betas=betas,xmat=xmat,y=y,b.gamma=b.old)
  pnew=get.llk(betas=betas,xmat=xmat,y=y,b.gamma=b.new)
  pthresh=exp(pnew-pold)
  accept=0
  if (runif(1)<pthresh){
    accept=1
    b.old=b.new
  }
  list(accept=accept,b.gamma=b.old)
}