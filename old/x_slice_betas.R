Doubling_Betas=function(xmat,betas1,w,b.gamma,yslice,MaxIter,ysoma,seg.id,nagg,
                        target.g,target.p,z,ngroups){
  betasLo=betasHi=betas1
  betasLo[target.p,target.g]=betasLo[target.p,target.g]-w*runif(1)
  betasHi[target.p,target.g]=betasLo[target.p,target.g]+w
  
  #calculate llk
  ylo=llk_betas(xmat=xmat,ysoma=ysoma,betas1=betasLo,b.gamma=b.gamma,var.betas=var.betas,
                seg.id=seg.id,nagg=nagg,z=z,ngroups=ngroups)
  yhi=llk_betas(xmat=xmat,ysoma=ysoma,betas1=betasHi,b.gamma=b.gamma,var.betas=var.betas,
                seg.id=seg.id,nagg=nagg,z=z,ngroups=ngroups)
  
  #keep doubling until ylo<yslice and yhi<yslice
  oo=0
  while ((ylo>yslice) & (oo<MaxIter)){
    betasLo[target.p,target.g]=betasLo[target.p,target.g]-w
    ylo=llk_betas(xmat=xmat,ysoma=ysoma,betas1=betasLo,b.gamma=b.gamma,var.betas=var.betas,
                  seg.id=seg.id,nagg=nagg,z=z,ngroups=ngroups)
    oo=oo+1
  }
  oo=0
  while ((yhi>yslice) & (oo<MaxIter)){
    betasHi[target.p,target.g]=betasHi[target.p,target.g]+w
    yhi=llk_betas(xmat=xmat,ysoma=ysoma,betas1=betasHi,b.gamma=b.gamma,var.betas=var.betas,
                  seg.id=seg.id,nagg=nagg,z=z,ngroups=ngroups)
    oo=oo+1
  }
  c(betasLo[target.p,target.g],betasHi[target.p,target.g])
}
#-------------------------------------------
llk_betas=function(xmat,ysoma,betas1,b.gamma,var.betas,seg.id,nagg,z,ngroups){
  media=exp(xmat%*%betas1)
  soma.media=GetSomaMediaAllGroups(media=media, ngroups=ngroups, 
                                   nysoma=nagg,SegID=seg.id-1)
  soma.media1=rep(NA,nagg)
  for (i in 1:ngroups){
    cond=z==i
    soma.media1[cond]=soma.media[cond,i]
  }
  
  a1=b.gamma*soma.media1
  sum(dgamma(ysoma,a1,b.gamma,log=T))+
    sum(dnorm(betas1,mean=0,sd=sqrt(var.betas),log=T))
}
#-------------------------------------------
Shrink_Sample_Betas=function(rango1,yslice,MaxIter,betas1,ysoma,xmat,seg.id,z,
                             nagg,b.gamma,ngroups,target.p,target.g){
  diff1=rango1[2]-rango1[1]
  yfim=-Inf
  oo=0
  while ((yfim<yslice) & (diff1 > 0.0001) & (oo<MaxIter)){
    x=runif(1,min=rango1[1],max=rango1[2])
    betas1[target.p,target.g]=x
    yfim=llk_betas(xmat=xmat,ysoma=ysoma,betas1=betas1,b.gamma=b.gamma,var.betas=var.betas,
                   seg.id=seg.id,nagg=nagg,z=z,ngroups=ngroups)
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
Sample_betas=function(ngroups,nparam,xmat,z,ysoma,betas,b.gamma,var.betas,w,MaxIter,seg.id,nagg){
  for (i in 1:ngroups){
    for (j in 1:nparam){
       #define upper bound
      upper1=llk_betas(xmat=xmat,ysoma=ysoma,betas1=betas,b.gamma=b.gamma,z=z,
                       var.betas=var.betas,seg.id=seg.id,nagg=nagg,ngroups=ngroups)
      yslice=upper1-rexp(1);
      
      #define slice
      rango1=Doubling_Betas(xmat=xmat,betas1=betas,w=w,b.gamma=b.gamma,
                            yslice=yslice,MaxIter=MaxIter,ysoma=ysoma,
                            seg.id=seg.id,nagg=nagg,target.p=j,target.g=i,
                            ngroups=ngroups,z=z)
      
      #sample this particular parameter
      betas=Shrink_Sample_Betas(rango1=rango1,yslice=yslice,MaxIter=MaxIter,
                               betas1=betas,ysoma=ysoma,xmat=xmat,
                               seg.id=seg.id,nagg=nagg,b.gamma=b.gamma,
                               target.p=j,target.g=i,z=z,ngroups=ngroups) 
    }
  }
  betas
}