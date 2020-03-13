sample.betas=function(betas,xmat,ysoma,jump,nparam,b.gamma,var.betas,seg.id,z,ngroups,n){
  betas.old=betas.new=betas
  betas.prop=matrix(rnorm(nparam*ngroups,mean=betas,sd=jump),nparam,ngroups)
  
  prior.old =matrix(dnorm(betas.old ,mean=0,sd=sqrt(var.betas),log=T),nparam,ngroups)
  prior.prop=matrix(dnorm(betas.prop,mean=0,sd=sqrt(var.betas),log=T),nparam,ngroups)
  accept=matrix(0,nparam,ngroups)
  
  for (i in 1:nparam){
    for (j in 1:ngroups){
      betas.new=betas.old
      betas.new[i,j]=betas.prop[i,j]
      pold=get.llk(betas=betas.old,xmat=xmat,ysoma=ysoma,n=n,
                   b.gamma=b.gamma,seg.id=seg.id,z=z)+prior.old[i,j]
      pnew=get.llk(betas=betas.new,xmat=xmat,ysoma=ysoma,n=n,
                   b.gamma=b.gamma,seg.id=seg.id,z=z)+prior.prop[i,j]
      pthresh=exp(pnew-pold)
      if (runif(1)<pthresh){
        accept[i,j]=1
        betas.old=betas.new
      }
    }
  }
  list(accept=accept,betas=betas.old)
}
#---------------------------------------------------
get.llk=function(betas,xmat,ysoma,b.gamma,seg.id,z,n){
  media=exp(xmat%*%betas)
  z1=z[seg.id]
  media1=rep(NA,n)
  for (i in 1:n){
    media1[i]=media[i,z1[i]]
  }
  tmp=data.frame(media=media1,seg.id=seg.id)
  soma.media=aggregate(media~seg.id,data=tmp,sum)
  a.gamma=b.gamma*soma.media$media
  sum(dgamma(ysoma,a.gamma,b.gamma,log=T))
}
#--------------------------------------------------
sample.b.gamma=function(betas,xmat,ysoma,jump,b.gamma,seg.id,z,n){
  b.old=b.gamma
  b.new=abs(rnorm(1,mean=b.gamma,sd=jump))
  
  pold=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.old,seg.id=seg.id,z=z,n=n)
  pnew=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.new,seg.id=seg.id,z=z,n=n)
  pthresh=exp(pnew-pold)
  accept=0
  if (runif(1)<pthresh){
    accept=1
    b.old=b.new
  }
  list(accept=accept,b.gamma=b.old)
}
#--------------------------------------------------
sample.z=function(xmat,betas,n.ysoma,ngroups,seg.id,b.gamma,ysoma,ltheta){
  media=exp(xmat%*%betas)
  media1=numeric()
  for (i in 1:ngroups){
    tmp=data.frame(media=media[,i],seg.id=seg.id)
    tmp1=aggregate(media~seg.id,data=tmp,sum)
    if (i==1) media1=tmp1[,2]
    if (i!=1) media1=cbind(media1,tmp1[,2])
  }
  
  #calculate probabilities
  a.gamma=b.gamma*media1
  ysoma.mat=matrix(ysoma,n.ysoma,ngroups)
  ltheta.mat=matrix(ltheta,n.ysoma,ngroups,byrow=T)
  prob=dgamma(ysoma,a.gamma,b.gamma,log=T)+ltheta.mat
  prob1=prob-apply(prob,1,max)
  prob2=exp(prob1)
  prob3=prob2/apply(prob2,1,sum)
  
  #sample z
  z=rep(NA,n.ysoma)
  for (i in 1:n.ysoma){
    ind=rmultinom(1,size=1,prob=prob3[i,])
    z[i]=which(ind==1)
  }
  z
}
#--------------------------------------------------
sample.theta=function(z,gamma1,ngroups){
  #summarize z's
  nk=rep(0,ngroups)
  tmp=table(z)
  nk[as.numeric(names(tmp))]=tmp
  
  #get v's
  v=rep(NA,ngroups)
  for (i in 1:(ngroups-1)){
    soma=sum(nk[(i+1):ngroups])
    v[i]=rbeta(1,nk[i]+1,soma+gamma1)
  }
  v[ngroups]=1
  
  #calculate theta
  theta=rep(NA,ngroups)
  theta[1]=v[1]
  prod=1
  for (i in 2:ngroups){
    prod=prod*(1-v[i-1])
    theta[i]=v[i]*prod
  }
  theta
}