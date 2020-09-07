Doubling_Betas=function(xmat,betas1,w,b.gamma,yslice,MaxIter,ysoma,seg.id,
                        nagg,target.p,sd.betas){
  betasLo=betasHi=betas1
  betasLo[target.p]=betasLo[target.p]-w*runif(1)
  betasHi[target.p]=betasLo[target.p]+w
  
  #calculate llk
  ylo=sum(get.llk(betas=betasLo,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
                  seg.id=seg.id,nagg=nagg))+
    sum(dnorm(betasLo,mean=0,sd=sd.betas,log=T))
  yhi=sum(get.llk(betas=betasHi,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
                  seg.id=seg.id,nagg=nagg))+
    sum(dnorm(betasHi,mean=0,sd=sd.betas,log=T))
  cond=F
  
  #keep doubling until ylo<yslice and yhi<yslice
  oo=0
  while ((ylo>yslice) & (oo<MaxIter)){
    betasLo[target.p]=betasLo[target.p]-w
    ylo=sum(get.llk(betas=betasLo,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
                    seg.id=seg.id,nagg=nagg))+
      sum(dnorm(betasLo,mean=0,sd=sd.betas,log=T))
    oo=oo+1
  }
  if (oo >= MaxIter) cond=T
  oo=0
  while ((yhi>yslice) & (oo<MaxIter)){
    betasHi[target.p]=betasHi[target.p]+w
    yhi=sum(get.llk(betas=betasHi,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
                    seg.id=seg.id,nagg=nagg))+
      sum(dnorm(betasHi,mean=0,sd=sd.betas,log=T))
    oo=oo+1
  }
  if (oo >= MaxIter) cond=T
  if (cond) return(c(betas1[target.p],betas1[target.p]))
  
  c(betasLo[target.p],betasHi[target.p])
}
#-------------------------------------------
Shrink_Sample_Betas=function(rango1,yslice,MaxIter,betas1,ysoma,xmat,seg.id,
                             nagg,b.gamma,target.p,sd.betas){
  diff1=rango1[2]-rango1[1]
  betas1.orig=betas1
  yfim=-Inf
  oo=0
  while ((yfim<yslice) & (diff1 > 0.00000001) & (oo<MaxIter)){
    x=runif(1,min=rango1[1],max=rango1[2])
    betas1[target.p]=x
    yfim=sum(get.llk(betas=betas1,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
                     seg.id=seg.id,nagg=nagg))+
      sum(dnorm(betas1,mean=0,sd=sd.betas,log=T))
    if (yfim<yslice){ #shrink the slice if x falls outside
      DistLo=abs(rango1[1]-x)
      DistHi=abs(rango1[2]-x)
      if (DistLo>DistHi) rango1[1]=x
      if (DistLo<DistHi) rango1[2]=x
      diff1=rango1[2]-rango1[1]
    }
    oo=oo+1
  }
  if (oo >= MaxIter | diff1 <= 0.00000001) return(betas1.orig)
  betas1
}
#-------------------------------------------
Sample_betas=function(nparam,xmat,ysoma,betas,b.gamma,sd.betas,w,MaxIter,seg.id,
                      nagg){
  for (j in 1:nparam){
    #define upper bound
    upper1=sum(get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
                       seg.id=seg.id,nagg=nagg))+
      sum(dnorm(betas,mean=0,sd=sd.betas,log=T))
    yslice=upper1-rexp(1);
    
    #define slice
    rango1=Doubling_Betas(xmat=xmat,betas1=betas,w=w,b.gamma=b.gamma,
                          yslice=yslice,MaxIter=MaxIter,ysoma=ysoma,
                          seg.id=seg.id,nagg=nagg,target.p=j,sd.betas=sd.betas)
    
    #sample this particular parameter
    betas=Shrink_Sample_Betas(rango1=rango1,yslice=yslice,MaxIter=MaxIter,
                              betas1=betas,ysoma=ysoma,xmat=xmat,
                              seg.id=seg.id,nagg=nagg,b.gamma=b.gamma,
                              target.p=j,sd.betas=sd.betas) 
  }
  betas
}