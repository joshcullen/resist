sample.betas=function(betas,xmat,ysoma,jump,nparam,b.gamma,var.betas,seg.id){
  betas.old=betas.new=betas
  betas.prop=rnorm(nparam,mean=betas,sd=jump)
  prior.old =dnorm(betas.old ,mean=0,sd=sqrt(var.betas),log=T)
  prior.prop=dnorm(betas.prop,mean=0,sd=sqrt(var.betas),log=T)
  accept=rep(0,nparam)
  
  for (i in 1:nparam){
    betas.new=betas.old
    betas.new[i]=betas.prop[i]
    pold=get.llk(betas=betas.old,xmat=xmat,ysoma=ysoma,
                 b.gamma=b.gamma,seg.id=seg.id)+prior.old[i]
    pnew=get.llk(betas=betas.new,xmat=xmat,ysoma=ysoma,
                 b.gamma=b.gamma,seg.id=seg.id)+prior.prop[i]
    pthresh=exp(pnew-pold)
    if (runif(1)<pthresh){
      accept[i]=1
      betas.old=betas.new
    }
  }
  list(accept=accept,betas=betas.old)
}
#---------------------------------------------------
get.llk=function(betas,xmat,ysoma,b.gamma,seg.id){
  media=exp(xmat%*%betas)
  tmp=data.frame(media=media,seg.id=seg.id)
  soma.media=aggregate(media~seg.id,data=tmp,sum)
  a.gamma=b.gamma*soma.media$media
  sum(dgamma(ysoma,a.gamma,b.gamma,log=T))
}
#--------------------------------------------------
sample.b.gamma=function(betas,xmat,ysoma,jump,b.gamma,seg.id){
  b.old=b.gamma
  b.new=abs(rnorm(1,mean=b.gamma,sd=jump))
  
  pold=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.old,seg.id=seg.id)
  pnew=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.new,seg.id=seg.id)
  pthresh=exp(pnew-pold)
  accept=0
  if (runif(1)<pthresh){
    accept=1
    b.old=b.new
  }
  list(accept=accept,b.gamma=b.old)
}