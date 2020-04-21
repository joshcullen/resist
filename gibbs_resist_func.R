get.llk=function(betas,xmat,ysoma,b.gamma,seg.id,z,n,n.ysoma){
  media=exp(xmat%*%betas)
  soma.media1=GetSomaMedia(z=z-1,media=media,ngroups=ngroups,nysoma=n.ysoma,SegID=seg.id-1)
  a.gamma=b.gamma*soma.media1
  sum(dgamma(ysoma,a.gamma,b.gamma,log=T))
}
#--------------------------------------------------
# sample.b.gamma=function(betas,xmat,ysoma,jump,b.gamma,seg.id,z,n,n.ysoma){
#   b.old=b.gamma
#   b.new=abs(rnorm(1,mean=b.gamma,sd=jump))
#   
#   pold=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.old,seg.id=seg.id,z=z,n=n,n.ysoma=n.ysoma)
#   pnew=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.new,seg.id=seg.id,z=z,n=n,n.ysoma=n.ysoma)
#   pthresh=exp(pnew-pold)
#   accept=0
#   if (runif(1)<pthresh){
#     accept=1
#     b.old=b.new
#   }
#   list(accept=accept,b.gamma=b.old)
# }
#--------------------------------------------------
sample.z=function(xmat,betas,n.ysoma,ngroups,seg.id,b.gamma,ysoma,ltheta){
  media=exp(xmat%*%betas)
  media1=GetSomaMediaAllGroups(media=media,ngroups=ngroups,nysoma=n.ysoma,SegID=seg.id-1)

  #calculate probabilities
  a.gamma=b.gamma*media1
  ysoma.mat=matrix(ysoma,n.ysoma,ngroups)
  ltheta.mat=matrix(ltheta,n.ysoma,ngroups,byrow=T)
  prob=dgamma(ysoma,a.gamma,b.gamma,log=T)+ltheta.mat
  prob1=prob-apply(prob,1,max)
  prob2=exp(prob1)
  prob3=prob2/rowSums(prob2)
  
  #sample z
  rand1=runif(nrow(prob3))
  z1=rmultinom1(prob=prob3, runif1=rand1)+1
  z1
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