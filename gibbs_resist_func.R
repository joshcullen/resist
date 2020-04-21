sample.betas=function(betas,xmat,ysoma,jump,nparam,b.gamma,var.betas,seg.id,
                      z,ngroup,nagg){
  betas.old=betas.new=betas
  betas.prop=matrix(rnorm(nparam,mean=betas,sd=jump),nparam,ngroup)
  accept=matrix(0,nparam,ngroup)
  
  for (i in 1:nparam){
    betas.new=betas.old
    betas.new[i,]=betas.prop[i,]
    pold=get.llk(betas=betas.old,xmat=xmat,ysoma=ysoma,
                 b.gamma=b.gamma,seg.id=seg.id,ngroup=ngroup,nagg=nagg)
    pnew=get.llk(betas=betas.new,xmat=xmat,ysoma=ysoma,
                 b.gamma=b.gamma,seg.id=seg.id,ngroup=ngroup,nagg=nagg)
    
    #get priors
    prior.old =dnorm(betas.old[i,] ,mean=0,sd=sqrt(var.betas[i]),log=T)
    prior.prop=dnorm(betas.prop[i,],mean=0,sd=sqrt(var.betas[i]),log=T)
  
    #sum the loglikel for the correct group
    pold1=pnew1=rep(NA,ngroup)
    for (j in 1:ngroup){
      cond=z==j
      pold1[j]=sum(pold[cond,j])
      pnew1[j]=sum(pnew[cond,j])
    }    
    
    #MH algorithm
    pold2=pold1+prior.old
    pnew2=pnew1+prior.prop
    pthresh=exp(pnew2-pold2)
    cond=runif(ngroup)<pthresh
    betas.old[i,cond]=betas.new[i,cond]
    accept[i,cond]=1
  }
  accept
  list(accept=accept,betas=betas.old)
}
#---------------------------------------------------
get.llk=function(betas,xmat,ysoma,b.gamma,seg.id,nagg,ngroup){
  media=exp(xmat%*%betas)
  soma.media=numeric()
  for (i in 1:ngroup){
    tmp=data.frame(media=media[,i],seg.id=seg.id)  
    tmp1=aggregate(media~seg.id,data=tmp,sum)
    soma.media=cbind(soma.media,tmp1$media)
  }
  a.gamma=b.gamma*soma.media
  ysoma.mat=matrix(ysoma,nagg,ngroup)
  dgamma(ysoma.mat,a.gamma,b.gamma,log=T)
}
#--------------------------------------------------
sample.b.gamma=function(betas,xmat,ysoma,jump,b.gamma,seg.id,z,
                        ngroup,nagg){
  b.old=b.gamma
  b.new=abs(rnorm(1,mean=b.gamma,sd=jump))
  
  pold=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.old,
               seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  pnew=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.new,
               seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  
  #sum the loglikel for the correct group
  pold1=pnew1=0
  for (j in 1:ngroup){
    cond=z==j
    pold1=pold1+sum(pold[cond,j])
    pnew1=pnew1+sum(pnew[cond,j])
  }    
  
  #MH algorithm
  pthresh=exp(pnew1-pold1)
  accept=0
  if (runif(1)<pthresh){
    accept=1
    b.old=b.new
  }
  list(accept=accept,b.gamma=b.old)
}