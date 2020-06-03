sample.betas=function(betas,xmat,ysoma,jump,nparam,b.gamma,var.betas,seg.id,
                      z,ngroup,nagg){
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
    # pold1=pnew1=rep(NA,ngroup)
    # for (j in 1:ngroup){
    #   cond=z==j
    #   pold1[j]=sum(pold[cond,j])
    #   pnew1[j]=sum(pnew[cond,j])
    # }
    
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
#---------------------------------------------------
get.llk=function(betas,xmat,ysoma,b.gamma,seg.id,nagg,ngroup){
  media=exp(xmat%*%betas)
  soma.media1=GetSomaMediaAllGroups(media=media, ngroups=ngroup, nysoma=length(ysoma),SegID=seg.id-1)
  b.gamma1=matrix(b.gamma,nagg,ngroup,byrow=T)
  a.gamma1=b.gamma1*soma.media1
  ysoma.mat=matrix(ysoma,nagg,ngroup)
  dgamma(ysoma.mat,a.gamma1,b.gamma1,log=T)
}
#--------------------------------------------------
sample.b.gamma=function(betas,xmat,ysoma,jump,b.gamma,seg.id,z,
                        ngroup,nagg){
  b.old=b.gamma
  b.new=abs(rnorm(ngroup,mean=b.gamma,sd=jump))

  pold=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.old,
               seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  pnew=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.new,
               seg.id=seg.id,ngroup=ngroup,nagg=nagg)

  #sum the loglikel for the correct group
  pold1=GetSomaLlkGroups(llk=pold, z=z-1, ngroups=ngroup)
  pnew1=GetSomaLlkGroups(llk=pnew, z=z-1, ngroups=ngroup)
  # pold2=pnew2=rep(NA,ngroup)
  # for (j in 1:ngroup){
  #   cond=z==j
  #   pold2[j]=sum(pold[cond,j])
  #   pnew2[j]=sum(pnew[cond,j])
  # }
  # unique(pold1-pold2)
  # unique(pnew1-pnew2)
  
  #MH algorithm
  pthresh=exp(pnew1-pold1)
  accept=rep(0,ngroup)
  cond=runif(ngroup)<pthresh
  accept[cond]=1
  b.old[cond]=b.new[cond]

  list(accept=accept,b.gamma=b.old)
}
#--------------------------------------------------
sample.z=function(betas,xmat,ysoma,b.gamma,seg.id,ngroup,nagg,theta){
  llk=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
              seg.id=seg.id,ngroup=ngroup,nagg=nagg)
  ltheta=matrix(log(theta),nrow(llk),ngroup,byrow=T)
  
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
#--------------------------------
sample.theta=function(z,gamma1,ngroup){
  #get summary statistics
  tab1=table(z)
  nk=rep(0,ngroup)
  nk[as.numeric(names(tab1))]=tab1
  
  tmp=(cumsum(nk[ngroup:1]))[ngroup:1]
  ngk=tmp[-1]
  
  #sample vk
  vk=rbeta(ngroup-1,nk[-ngroup]+1,ngk+gamma1)
  vk=c(vk,1)
  
  #calculate theta
  theta=vk
  prod1=1
  for (i in 2:ngroup){
    prod1=prod1*(1-vk[i-1])
    theta[i]=vk[i]*prod1
  }
  theta
}