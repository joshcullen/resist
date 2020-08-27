gibbs_resist=function(ysoma,xmat,seg.id,ngibbs,nburn,var.betas,w,MaxIter){
  n=nrow(xmat)  
  nparam=ncol(xmat)
  nagg=length(ysoma)  
  
  #initial parameters
  betas=matrix(0,nparam,1)
  b.gamma=1

  #for joint sampling of betas
  var1=diag(1,nparam)

  #stuff for gibbs sampler
  store.betas=matrix(NA,ngibbs,nparam)
  store.b=matrix(NA,ngibbs,1)
  store.llk=matrix(NA,ngibbs,1)

  for (i in 1:ngibbs){
    print(i)
    
    #sample betas
    betas=Sample_betas(nparam=nparam,xmat=xmat,ysoma=ysoma,betas=betas,
                       b.gamma=b.gamma,var.betas=var1,w=w,MaxIter=MaxIter,seg.id=seg.id,nagg=nagg)
    # betas=betas.true
    
    #sample b.gamma
    b.gamma=Sample_bgamma(nparam=nparam,xmat=xmat,
                          ysoma=ysoma,betas=betas,b.gamma=b.gamma,
                          w=w,MaxIter=MaxIter,seg.id=seg.id,nagg=nagg)
    # b.gamma=b.true
    
    #get llk
    p=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
              seg.id=seg.id,nagg=nagg)
    llk1=sum(p)

    #store results
    store.betas[i,]=betas
    store.b[i]=b.gamma
    store.llk[i]=sum(llk1)
  }
  list(betas=store.betas,b.gamma=store.b,llk=store.llk)
}



