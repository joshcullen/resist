sample.betas=function(betas,xmat,ysoma,jump,nparam,b.gamma,var.betas,seg.id,z,ngroup,nagg){
  betas.old=betas.new=betas
  betas.prop=matrix(rnorm(nparam*ngroup,mean=betas,sd=jump),nparam,ngroup)
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
    pold1=GetSomaLlkGroups(llk=pold, z=z-1, ngroups=ngroup)
    pnew1=GetSomaLlkGroups(llk=pnew, z=z-1, ngroups=ngroup)
    
    #MH algorithm
    pold2=pold1+prior.old
    pnew2=pnew1+prior.prop
    pthresh=exp(pnew2-pold2)
    cond=runif(ngroup)<pthresh
    betas.old[i,cond]=betas.new[i,cond]
    accept[i,cond]=1
  }
  
  list(accept=accept,betas=betas.old)
}
#--------------------------------------------
sample.betas.joint=function(betas,xmat,ysoma,nparam,b.gamma,var.betas,seg.id,
                            z,ngroup,nagg,var1){
  betas.old=betas.new=betas
  for (i in 1:ngroup){
    betas.new[,i]=rmvnorm(1,mean=betas.old[,i],sigma=var1[[i]])
  }
  
  pold=get.llk(betas=betas.old,xmat=xmat,ysoma=ysoma,
               b.gamma=b.gamma,seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  pnew=get.llk(betas=betas.new,xmat=xmat,ysoma=ysoma,
               b.gamma=b.gamma,seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  
  #get priors
  var.betas1=matrix(var.betas,nparam,ngroup)
  prior.old =apply(dnorm(betas.old,mean=0,sd=sqrt(var.betas1),log=T),2,sum)
  prior.new=apply(dnorm(betas.new,mean=0,sd=sqrt(var.betas1),log=T),2,sum)
  
  #sum the loglikel for the correct group
  pold1=GetSomaLlkGroups(llk=pold, z=z-1, ngroups=ngroup)
  pnew1=GetSomaLlkGroups(llk=pnew, z=z-1, ngroups=ngroup)
  
  #MH algorithm
  pold2=pold1+prior.old
  pnew2=pnew1+prior.new
  pthresh=exp(pnew2-pold2)
  cond=runif(ngroup)<pthresh
  betas.old[,cond]=betas.new[,cond]
  
  accept=rep(0,ngroup)
  accept[cond]=1
  
  list(accept=accept,betas=betas.old)
}
#--------------------------------------------
get.llk=function(betas,xmat,ysoma,b.gamma,seg.id,nagg,ngroup){
  media=exp(xmat%*%betas)
  soma.media1=GetSomaMediaAllGroups(media=media, ngroups=ngroup, nysoma=nagg,SegID=seg.id-1)
  a.gamma1=b.gamma*soma.media1
  ysoma.mat=matrix(ysoma,nagg,ngroup)
  dgamma(ysoma.mat,a.gamma1,b.gamma,log=T)
}
#--------------------------------------------------
sample.z=function(betas,xmat,ysoma,b.gamma,seg.id,ngroup,nagg,theta){
  llk=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
              seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  ltheta=matrix(log(theta),nagg,ngroup,byrow=T)
  
  llk=llk+ltheta
  prob=GetProbZ(llk=llk)
  # max1=apply(llk,1,max)
  # llk1=llk-max1
  # llk2=exp(llk1)
  # llk3=llk2/apply(llk2,1,sum)
  tmp=rmultinom1(prob=prob, runif1=runif(nrow(prob)))
  tmp+1
}
#----------------------------------
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