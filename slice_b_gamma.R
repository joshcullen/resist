Doubling_bgamma=function(soma.media,w,b.gamma,yslice,MaxIter,ysoma){
  b.gammaLo=b.gamma-w*runif(1)
  b.gammaLo=ifelse(b.gammaLo<0.0000001,0.0000001,b.gammaLo)
  b.gammaHi=b.gammaLo+w
  
  #calculate llk
  ylo=llk_bgamma(soma.media=soma.media,ysoma=ysoma,b.gamma=b.gammaLo)
  yhi=llk_bgamma(soma.media=soma.media,ysoma=ysoma,b.gamma=b.gammaHi)
  
  #keep doubling until ylo<yslice and yhi<yslice
  oo=0
  while ((ylo>yslice) & (oo<MaxIter)){
    b.gammaLo=b.gammaLo-w
    if (b.gammaLo<0.0000001) { #avoid negative values
      b.gammaLo=0.0000001
      break;
    }
    ylo=llk_bgamma(soma.media=soma.media,ysoma=ysoma,b.gamma=b.gammaLo)
    oo=oo+1
  }
  oo=0
  while ((yhi>yslice) & (oo<MaxIter)){
    b.gammaHi=b.gammaHi+w
    yhi=llk_bgamma(soma.media=soma.media,ysoma=ysoma,b.gamma=b.gammaHi)
    oo=oo+1
  }
  c(b.gammaLo,b.gammaHi)
}
#-------------------------------------------
llk_bgamma=function(soma.media,ysoma,b.gamma){
  a1=b.gamma*soma.media
  sum(dgamma(ysoma,a1,b.gamma,log=T))
}
#-------------------------------------------
Shrink_Sample_bgamma=function(rango1,yslice,MaxIter,soma.media,ysoma,n.ysoma,b.gamma){
  diff1=rango1[2]-rango1[1]
  yfim=-Inf
  oo=0
  while ((yfim<yslice) & (diff1 > 0.0001) & (oo<MaxIter)){
    x=runif(1,min=rango1[1],max=rango1[2])
    yfim=llk_bgamma(soma.media=soma.media,ysoma=ysoma,b.gamma=x)
    if (yfim<yslice){ #shrink the slice if x falls outside
      DistLo=abs(rango1[1]-x)
      DistHi=abs(rango1[2]-x)
      if (DistLo>DistHi) rango1[1]=x
      if (DistLo<DistHi) rango1[2]=x
      diff1=rango1[2]-rango1[1]
    }
    oo=oo+1
  }
  x
}
#-------------------------------------------
Sample_bgamma=function(ngroups,nparam,xmat,z,ysoma,betas,b.gamma,w,MaxIter,seg.id,n.ysoma){
  media=exp(xmat%*%betas)
  soma.media=GetSomaMediaAllGroups(media=media, ngroups=ngroups, 
                                   nysoma=n.ysoma,SegID=seg.id-1)

  for (i in 1:ngroups){
    #define upper bound
    cond=z==i
    upper1=llk_bgamma(soma.media=soma.media[cond,i],ysoma=ysoma[cond],b.gamma=b.gamma[i])
    yslice=upper1-rexp(1);
    
    #define slice
    rango1=Doubling_bgamma(soma.media=soma.media,w=w,b.gamma=b.gamma,yslice=yslice,MaxIter=MaxIter,ysoma=ysoma)

    #sample this particular parameter
    tmp=Shrink_Sample_bgamma(rango1=rango1,yslice=yslice,MaxIter=MaxIter,
                             soma.media=soma.media,ysoma=ysoma,n.ysoma=n.ysoma,b.gamma=b.gamma)
    b.gamma[i]=tmp    
  }
  b.gamma
}