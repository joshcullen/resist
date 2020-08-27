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