Doubling_Betas=function(xmat,betas1,target.beta,w,b.gamma,yslice,MaxIter,ysoma1,seg.id,z,target.group,n.ysoma){
  betasLo=betasHi=betas1
  betasLo[target.beta]=betasLo[target.beta]-w*runif(1)
  betasHi[target.beta]=betasLo[target.beta]+w
  
  #calculate llk
  ylo=llk_betas(xmat=xmat,ysoma1=ysoma1,betas1=betasLo,b.gamma=b.gamma,var.betas=var.betas,
                seg.id=seg.id,z=z,target.group=target.group,n.ysoma=n.ysoma)
  yhi=llk_betas(xmat=xmat,ysoma1=ysoma1,betas1=betasHi,b.gamma=b.gamma,var.betas=var.betas,
                seg.id=seg.id,z=z,target.group=target.group,n.ysoma=n.ysoma)
  
  #keep doubling until ylo<yslice and yhi<yslice
  oo=0
  while ((ylo>yslice) & (oo<MaxIter)){
    betasLo[target.beta]=betasLo[target.beta]-w
    ylo=llk_betas(xmat=xmat,ysoma1=ysoma1,betas1=betasLo,b.gamma=b.gamma,var.betas=var.betas,
                  seg.id=seg.id,z=z,target.group=target.group,n.ysoma=n.ysoma)
    oo=oo+1
  }
  oo=0
  while ((yhi>yslice) & (oo<MaxIter)){
    betasHi[target.beta]=betasHi[target.beta]+w
    yhi=llk_betas(xmat=xmat,ysoma1=ysoma1,betas1=betasHi,b.gamma=b.gamma,var.betas=var.betas,
                  seg.id=seg.id,z=z,target.group=target.group,n.ysoma=n.ysoma)
    oo=oo+1
  }
  c(betasLo[target.beta],betasHi[target.beta])
}
#-------------------------------------------
llk_betas=function(xmat,ysoma1,betas1,b.gamma,var.betas,seg.id,z,target.group,n.ysoma){
  n.ysoma=length(z)
  media=exp(xmat%*%betas1)
  soma.group=GetSomaMediaOneGroup(media=media,z=z-1,TargetGrp=target.group-1,nysoma=n.ysoma,SegID=seg.id-1)
  cond=z==target.group
  a1=b.gamma*soma.group[cond]
  sum(dgamma(ysoma1,a1,b.gamma,log=T))+
    sum(dnorm(betas1,mean=0,sd=sqrt(var.betas),log=T))
}
#-------------------------------------------
Shrink_Sample_Betas=function(rango1,yslice,MaxIter,betas1,target.beta,ysoma1,xmat,seg.id,z,target.group,n.ysoma,b.gamma){
  diff1=rango1[2]-rango1[1]
  yfim=-Inf
  oo=0
  while ((yfim<yslice) & (diff1 > 0.0001) & (oo<MaxIter)){
    x=runif(1,min=rango1[1],max=rango1[2])
    betas1[target.beta]=x
    yfim=llk_betas(xmat=xmat,ysoma1=ysoma1,betas1=betas1,b.gamma=b.gamma,var.betas=var.betas,
                   seg.id=seg.id,z=z,target.group=target.group,n.ysoma=n.ysoma)
    if (yfim<yslice){ #shrink the slice if x falls outside
      DistLo=abs(rango1[1]-x)
      DistHi=abs(rango1[2]-x)
      if (DistLo>DistHi) rango1[1]=x
      if (DistLo<DistHi) rango1[2]=x
      diff1=rango1[2]-rango1[1]
    }
    oo=oo+1
  }
  betas1
}
#-------------------------------------------
Sample_betas=function(ngroups,nparam,xmat,z,ysoma,betas,b.gamma,var.betas,w,MaxIter,seg.id,n.ysoma){
  for (i in 1:ngroups){
    for (j in 1:nparam){
      #subset relevant pieces
      ysoma1=ysoma[z==i]
      
      #define upper bound
      upper1=llk_betas(xmat=xmat,ysoma1=ysoma1,betas1=betas[,i],b.gamma=b.gamma,var.betas=var.betas,
                       seg.id=seg.id,z=z,target.group=i,n.ysoma=n.ysoma)
      yslice=upper1-rexp(1);
      
      #define slice
      rango1=Doubling_Betas(xmat=xmat,betas1=betas[,i],target.beta=j,w=w,
                            b.gamma=b.gamma,yslice=yslice,MaxIter=MaxIter,ysoma1=ysoma1,
                            seg.id=seg.id,z=z,target.group=i,n.ysoma=n.ysoma)
      
      #sample this particular parameter
      tmp=Shrink_Sample_Betas(rango1=rango1,yslice=yslice,MaxIter=MaxIter,
                              betas1=betas[,i],target.beta=j,ysoma1=ysoma1,xmat=xmat,
                              seg.id=seg.id,z=z,target.group=i,n.ysoma=n.ysoma,b.gamma=b.gamma) 
      betas[,i]=tmp
    }
  }
  betas
}