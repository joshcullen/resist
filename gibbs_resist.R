gibbs_resist=function(dat,var.betas,gamma1,ngroups,w=0.1,MaxIter=100,ngibbs,nburn){
  n=nrow(dat)
  ind=grep('cov',colnames(dat))
  xmat=data.matrix(cbind(1,dat[,ind]))
  nparam=ncol(xmat)
  seg.id=dat$seg.id
  
  #get y soma
  tmp=unique(dat[,c('seg.id','ysoma')])
  cond=!is.na(tmp$ysoma)
  ysoma=tmp[cond,'ysoma']
  n.ysoma=length(ysoma)

  #initial parameters
  betas=matrix(0,nparam,ngroups)
  b.gamma=20
  z=sample(1:ngroups,size=n.ysoma,replace=T)
  theta=rep(1/ngroups,ngroups)
  
  #stuff for gibbs sampler
  store.betas=matrix(NA,ngibbs,nparam*ngroups)
  store.z=matrix(NA,ngibbs,n.ysoma)
  store.b=matrix(NA,ngibbs,1)
  store.llk=matrix(NA,ngibbs,1)
  store.theta=matrix(NA,ngibbs,ngroups)
  store.intercepts=matrix(NA,ngibbs,ngroups)
  
  for (i in 1:ngibbs){
    print(i)
    
    #sample betas 
    betas=Sample_betas(ngroups=ngroups,nparam=nparam,xmat=xmat,z=z,
                       ysoma=ysoma,betas=betas,b.gamma=b.gamma,var.betas=var.betas,
                       w=w,MaxIter=MaxIter,seg.id=seg.id)
    # betas=betas.true
    
    #sample b.gamma (implement slice sampler)
    b.gamma=Sample_bgamma(ngroups=ngroups,nparam=nparam,xmat=xmat,
                          z=z,ysoma=ysoma,betas=betas,b.gamma=b.gamma,
                          w=w,MaxIter=MaxIter,seg.id=seg.id,n.ysoma=n.ysoma)
    # b.gamma=b.true
    
    #sample z 
    # z=sample.z(xmat=xmat,betas=betas,n.ysoma=n.ysoma,ngroups=ngroups,seg.id=seg.id,
    #            b.gamma=b.gamma,ysoma=ysoma,ltheta=log(theta))
    z=aux.true$z
    
    #estimate theta
    # theta=sample.theta(z=z,gamma1=gamma1,ngroups=ngroups)
    theta=rep(1/ngroups,ngroups)
    
    #get llk
    llk=get.llk(betas=betas,xmat=xmat,ysoma=ysoma,b.gamma=b.gamma,seg.id=seg.id,z=z,n=n,n.ysoma=n.ysoma)
    
    #store results
    store.betas[i,]=betas
    store.b[i]=b.gamma
    store.llk[i]=llk
    store.z[i,]=z
    store.theta[i,]=theta
    store.intercepts[i,]=betas[1,]
  }
  list(betas=store.betas,
       b.gamma=store.b,
       llk=store.llk,
       z=store.z,
       theta=store.theta)
}
