sample.betas=function(betas,xmat,ysoma,jump,nparam,b.gamma,var.betas,seg.id,nagg){
  betas.old=betas.new=betas
  betas.prop=matrix(rnorm(nparam,mean=betas,sd=jump),nparam,1)
  accept=matrix(0,nparam,1)

  for (i in 1:nparam){
    betas.new=betas.old
    betas.new[i,]=betas.prop[i,]
    pold=get.llk(betas=betas.old,xmat=xmat,ysoma=ysoma,
                 b.gamma=b.gamma,seg.id=seg.id,nagg=nagg)
    pnew=get.llk(betas=betas.new,xmat=xmat,ysoma=ysoma,
                 b.gamma=b.gamma,seg.id=seg.id,nagg=nagg)

    #get priors
    prior.old =dnorm(betas.old[i,] ,mean=0,sd=sqrt(var.betas[i]),log=T)
    prior.prop=dnorm(betas.prop[i,],mean=0,sd=sqrt(var.betas[i]),log=T)

    #sum the loglikel
    pold1=sum(pold)
    pnew1=sum(pnew)
    
    #MH algorithm
    pold2=pold1+prior.old
    pnew2=pnew1+prior.prop
    pthresh=exp(pnew2-pold2)
    cond=runif(1)<pthresh
    betas.old[i]=ifelse(cond,betas.new[i],betas.old[i])
    accept[i]=1
  }
  
  list(accept=accept,betas=betas.old)
}
#--------------------------------------------
sample.betas.joint=function(betas,xmat,ysoma,nparam,b.gamma,var.betas,seg.id,
                            nagg,var1){
  betas.old=betas.new=betas
  betas.new=t(rmvnorm(1,mean=betas.old,sigma=var1))

  pold=get.llk(betas=betas.old,xmat=xmat,ysoma=ysoma,
               b.gamma=b.gamma,seg.id=seg.id,nagg=nagg)
  pnew=get.llk(betas=betas.new,xmat=xmat,ysoma=ysoma,
               b.gamma=b.gamma,seg.id=seg.id,nagg=nagg)
    
  #get priors
  prior.old =apply(dnorm(betas.old,mean=0,sd=sqrt(var.betas),log=T),2,sum)
  prior.new=apply(dnorm(betas.new,mean=0,sd=sqrt(var.betas),log=T),2,sum)
    
  #sum the loglikel
  pold1=sum(pold)
  pnew1=sum(pnew)
    
  #MH algorithm
  pold2=pold1+prior.old
  pnew2=pnew1+prior.new
  pthresh=exp(pnew2-pold2)
  cond=runif(1)<pthresh
  accept=0
  if (cond) {
    betas.old=betas.new
    accept=1
  }

  list(accept=accept,betas=betas.old)
}
#--------------------------------------------
get.llk=function(betas,xmat,ysoma,b.gamma,seg.id,nagg){
  media=exp(xmat%*%betas)
  soma.media1=GetSomaMediaOneGroup(media=media, nagg=nagg,SegID=seg.id-1)
  a.gamma1=b.gamma*soma.media1
  dgamma(ysoma,a.gamma1,b.gamma,log=T)
}
#--------------------------------------------------
print.adapt = function(accept1z,jump1z,accept.output){
  accept1=accept1z; jump1=jump1z; 

  for (k in 1:length(accept1)){
    z=accept1[[k]]/accept.output
    print(names(accept1)[k])
    print(z); print(jump1[[k]])
  }

  for (k in 1:length(jump1)){
    cond=(accept1[[k]]/accept.output)>0.6 & jump1[[k]]<100
    jump1[[k]][cond] = jump1[[k]][cond]*2       
    cond=(accept1[[k]]/accept.output)<0.1 & jump1[[k]]>0.01
    jump1[[k]][cond] = jump1[[k]][cond]*0.5
    accept1[[k]][]=0
  }
  return(list(jump1=jump1,accept1=accept1))
}