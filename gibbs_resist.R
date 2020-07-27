gibbs_resist=function(ysoma,xmat,seg.id,ngroup,ngibbs,nburn,gamma1,var.betas,w,MaxIter){
  n=nrow(xmat)  
  nparam=ncol(xmat)
  nagg=length(ysoma)  
  
  #initial parameters
  betas=matrix(0,nparam,ngroup)
  b.gamma=1
  z=sample(1:ngroup,size=nagg,replace=T)
  theta=rep(1/ngroup,ngroup)
  tmp=diag(1,nparam)
  
  #for joint sampling of betas
  var1=list()
  for (jj in 1:ngroup){
    var1[[jj]]=tmp
  }
  ind.betas=matrix(1:(nparam*ngroup),nparam,ngroup)
  
  #stuff for gibbs sampler
  jump1=list(betas=matrix(0.1,nparam,ngroup),betas.joint=rep(1,ngroup))
  accept1=list(betas=matrix(0,nparam,ngroup),betas.joint=rep(0,ngroup))
  accept.output=50  
  store.betas=matrix(NA,ngibbs,nparam*ngroup)
  store.b=matrix(NA,ngibbs,1)
  store.theta=matrix(NA,ngibbs,ngroup)
  store.llk=matrix(NA,ngibbs,1)
  
  for (i in 1:ngibbs){
    print(i)
    
    #sample betas    
    tmp=sample.betas(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$betas,
                     b.gamma=b.gamma,nparam=nparam,var.betas=var.betas,
                     seg.id=seg.id,ngroup=ngroup,nagg=nagg,z=z)
    betas=tmp$betas
    accept1$betas=accept1$betas+tmp$accept
    
    tmp=sample.betas.joint(betas=betas,xmat=xmat,ysoma=ysoma,
                           b.gamma=b.gamma,nparam=nparam,var.betas=var.betas,
                           seg.id=seg.id,ngroup=ngroup,nagg=nagg,z=z,var1=var1)
    betas=tmp$betas
    accept1$betas.joint=accept1$betas.joint+tmp$accept
    
    # betas=betas.true
    
    #sample b.gamma
    b.gamma=Sample_bgamma(ngroups=ngroup,nparam=nparam,xmat=xmat,
                          z=z,ysoma=ysoma,betas=betas,b.gamma=b.gamma,
                          w=w,MaxIter=100,seg.id=seg.id,nagg=nagg)
    # b.gamma=b.true
    
    #sample z
    z=sample.z(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
               seg.id=seg.id,ngroup=ngroup,nagg=nagg,theta=theta)
    # z=z.true
    
    #sample theta
    theta=rep(1/ngroup,ngroup)
    
    #get llk
    p=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
              seg.id=seg.id,ngroup=ngroup,nagg=nagg)
    #sum the loglikel for the correct group
    llk1=GetSomaLlkGroups(llk=p, z=z-1, ngroups=ngroup)
    
    #adaptation MH algorithm
    if (i<nburn & i%%accept.output==0){
      k=print.adapt(accept1z=accept1,jump1z=jump1,accept.output=accept.output)
      accept1=k$accept1
      jump1=k$jump1
      
      #get correlation structure from posterior samples
      for (jj in 1:ngroup){
        seq1=(i-accept.output+1):(i-1)
        var1[[jj]]=var(store.betas[seq1,ind.betas[,jj]])  
      }
    }
    
    #store results
    store.betas[i,]=betas
    store.b[i]=b.gamma
    store.llk[i]=sum(llk1)
    store.theta[i,]=theta
    z.estim=z
  }
  list(betas=store.betas,b.gamma=store.b,llk=store.llk,
       z.estim=z,theta=store.theta)
}



