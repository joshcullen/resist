gibbs_resist=function(ysoma,xmat,seg.id,ngroup,ngibbs,nburn,gamma1,var.betas){
  n=nrow(xmat)  
  nparam=ncol(xmat)
  nagg=length(ysoma)  
  
  #initial parameters
  betas=matrix(0,nparam,ngroup)
  betas[1,]=log(mean(ysoma))
  b.gamma=rep(0.01,ngroup)
  z=sample(1:ngroup,size=nagg,replace=T)
  theta=rep(1/ngroup,ngroup)
  
  #stuff for gibbs sampler
  jump1=list(betas=matrix(0.1,nparam,ngroup),b.gamma=rep(0.1,ngroup))
  accept1=list(betas=matrix(0,nparam,ngroup),b.gamma=rep(0,ngroup))
  store.betas=matrix(NA,ngibbs,nparam*ngroup)
  store.b=matrix(NA,ngibbs,ngroup)
  store.theta=matrix(NA,ngibbs,ngroup)
  store.llk=matrix(NA,ngibbs,1)
  accept.output=50
  
  for (i in 1:ngibbs){
    print(i)
    
    #sample betas
    tmp=sample.betas(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$betas,
                     b.gamma=b.gamma,nparam=nparam,var.betas=var.betas,
                     seg.id=seg.id,ngroup=ngroup,nagg=nagg,z=z)
    betas=tmp$betas
    accept1$betas=accept1$betas+tmp$accept

    #sample b.gamma
    # b.gamma=Sample_bgamma(ngroups=ngroups,nparam=nparam,xmat=xmat,
    #                       z=z,ysoma=ysoma,betas=betas,b.gamma=b.gamma,
    #                       w=0.1,MaxIter=100,seg.id=seg.id,n.ysoma=length(ysoma))
    tmp=sample.b.gamma(betas=betas,xmat=xmat,ysoma=ysoma,jump=jump1$b.gamma,
                       b.gamma=b.gamma,seg.id=seg.id,ngroup=ngroup,nagg=nagg,z=z)
    b.gamma=tmp$b.gamma
    accept1$b.gamma=accept1$b.gamma+tmp$accept
    # b.gamma=b.true
    
    #sample z
    z=sample.z(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
               seg.id=seg.id,ngroup=ngroup,nagg=nagg,theta=theta)
    # z=z.true
    
    #sample theta
    # theta=sample.theta(z=z,gamma1=gamma1,ngroup=ngroup)
    theta=rep(1/ngroup,ngroup)
    
    #get llk
    p=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,
              seg.id=seg.id,ngroup=ngroup,nagg=nagg)
    #sum the loglikel for the correct group
    llk1=GetSomaLlkGroups(llk=p, z=z-1, ngroups=ngroup)
    # llk=0
    # for (j in 1:ngroup){
    #   cond=z==j
    #   llk=llk+sum(p[cond,j])
    # }    
    
    #re-order groups from time to time
    # if (i<nburn & i%%accept.output==0){
    #   ind=order(theta,decreasing=T)
    #   b.gamma=b.gamma[ind]
    #   theta=theta[ind]
    #   betas=betas[,ind]
    #   jump1$betas=jump1$betas[,ind]
    #   z1=rep(NA,length(z))
    #   for (k in 1:ngroup){
    #     cond=z==ind[k]
    #     z1[cond]=k
    #   }
    #   z=z1
    # }
    
    #adaptation MH algorithm
    if (i<nburn & i%%accept.output==0){
      k=print.adapt(accept1z=accept1,jump1z=jump1,accept.output=accept.output)
      accept1=k$accept1
      jump1=k$jump1
    }
    
    #store results
    store.betas[i,]=betas
    store.b[i,]=b.gamma
    store.llk[i]=sum(llk1)
    store.theta[i,]=theta
    z.estim=z
  }
  list(betas=store.betas,b.gamma=store.b,llk=store.llk,
       z.estim=z,theta=store.theta)
}



