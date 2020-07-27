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
  jump1=list(betas=matrix(0.1,nparam,1),betas.joint=1)
  accept1=list(betas=matrix(0,nparam,1),betas.joint=0)
  accept.output=50  
  store.betas=matrix(NA,ngibbs,nparam)
  store.b=matrix(NA,ngibbs,1)
  store.llk=matrix(NA,ngibbs,1)
  
  #progress bar
  pb<- progress::progress_bar$new(
    format = " iteration (:current/:total) [:bar] :percent [Elapsed: :elapsed, Remaining: :eta]",
    total = ngibbs, clear = FALSE, width = 100)

  for (i in 1:ngibbs){
    pb$tick()  #create progress bar
    
    #sample betas    
    tmp=sample.betas(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$betas,
                     b.gamma=b.gamma,nparam=nparam,var.betas=var.betas,
                     seg.id=seg.id,nagg=nagg)
    betas=tmp$betas
    accept1$betas=accept1$betas+tmp$accept
    
    tmp=sample.betas.joint(betas=betas,xmat=xmat,ysoma=ysoma,
                           b.gamma=b.gamma,nparam=nparam,var.betas=var.betas,
                           seg.id=seg.id,nagg=nagg,var1=var1)
    betas=tmp$betas
    accept1$betas.joint=accept1$betas.joint+tmp$accept
    
    # betas=betas.true
    
    #sample b.gamma
    b.gamma=Sample_bgamma(nparam=nparam,xmat=xmat,
                          ysoma=ysoma,betas=betas,b.gamma=b.gamma,
                          w=w,MaxIter=100,seg.id=seg.id,nagg=nagg)
    # b.gamma=b.true
    
    #get llk
    p=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
              seg.id=seg.id,nagg=nagg)
    llk1=sum(p)

    #adaptation MH algorithm
    if (i<nburn & i%%accept.output==0){
      k=print.adapt(accept1z=accept1,jump1z=jump1,accept.output=accept.output)
      accept1=k$accept1
      jump1=k$jump1
      
      #get correlation structure from posterior samples
      seq1=(i-accept.output+1):(i-1)
      var1=var(store.betas[seq1,])  
    }
    
    #store results
    store.betas[i,]=betas
    store.b[i]=b.gamma
    store.llk[i]=sum(llk1)
  }
  list(betas=store.betas,b.gamma=store.b,llk=store.llk)
}



