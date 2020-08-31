get.llk=function(betas,xmat,ysoma,b.gamma,seg.id,nagg){
  media=exp(xmat%*%betas)
  soma.media1=GetSomaMediaOneGroup(media=media, nagg=nagg,SegID=seg.id-1)
  a.gamma1=b.gamma*soma.media1
  dgamma(ysoma,a.gamma1,b.gamma,log=T)
}