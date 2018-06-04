
iterem=0

nbeta=beta=c(1,1)

repeat {   # 直到 EM 收斂為止

   iterem=iterem+1

   L=S=alpha=prod=rep(NA,n)

   Lz=Sz=Szd=matrix(NA,n,n)

   bz=Z%*%beta

   L=cumsum(dL)

   for(i in 1:n){
      Lz[i,]=L*exp(bz[i])
      }

   Sz=exp(-G.fn(Lz))

   for(i in 1:n){
      prod=rep(NA,n)
      Szd[i,1]=1-Sz[i,1]
      prod[1]=est_Vnt[1]*Szd[i,1]
      for(j in 2:n){
         Szd[i,j]=Sz[i,j-1]-Sz[i,j]
         prod[j]=est_Vnt[j]*Szd[i,j]
         }
      alpha[i]=sum(prod)
      }   
   

   EO=matrix(NA,n,n)
   p=matrix(NA,n,n)

   for(i in 1:n){
      p[i,]=dL*exp(bz[i])*g.fn(cumsum(dL)*exp(bz[i]))*exp(-G.fn(cumsum(dL)*exp(bz[i])))
      }

   for(i in 1:n){
      QQQ=rep(0,n)
      for(l in 1:n){   #<--------------------------------------------------------------------------
         QQQ[l]=p[i,l]*ifelse(nX[i]<=nX[l],1,0)
         }
      for(j in 1:n){      
         EO[i,j]=ifelse(sum(QQQ)!=0,ndelta[i]*ifelse(nX[i]==nX[j],1,0)+(1-ndelta[i])*(p[i,j]*ifelse(nX[i]<=nX[j],1,0))/sum(QQQ),ndelta[i]*ifelse(nX[i]==nX[j],1,0))
         }
      }   

   Em=matrix(NA,n,n)
   
   for(i in 1:n){
      Em[i,]=p[i,]*(1-est_Vnt)/alpha[i]
      }

   w=EO+Em

   wpj=colSums(w)
   wpi=rowSums(w)

   iter=0

   #################
   #     M-step    # 
   #################

   repeat{

   	bz=Z%*%beta
   	iter=iter+1
   	for(l in 1:n) {
      		tmpl=matrix(NA,n,n)

      		for(j in 1:n){
         		LT=(exp(bz))%*%sum(dL[1:j])
         		Ai=g.dn(LT)/g.fn(LT)
         		tmpl[,j]=w[,j]*exp(bz)*(g.fn(LT)-Ai)*ifelse(nX[j]>=nX[l],1,0)
         		} # end: for(j in 1:n)

      		dL[l]=wpj[l]/sum(tmpl)
      		} # end: for(l in 1:n)

   	plot(nX, L)
   	lines(nX, 5*nX^2, col=2)
   	title(main=paste("R=", R, ",  iterem=", iterem))



   	SB=function(x) {
      		sb=matrix(NA,n,2)   
      		for(i in 1:n) {
         		LT=cumsum(dL)*exp(Z[i,]%*%x)
         		sb[i,]=Z[i,]*sum((1+g.dn(LT)/g.fn(LT)*cumsum(dL)*exp(Z[i,]%*%x)-g.fn(LT)*cumsum(dL)*exp(Z[i,]%*%x))*w[i,])
         		} # end: for(i in 1:n)
      		return(colSums(sb))
      		} # end: SB=function(x)


   	tmpp=nleqslv(x=beta,SB)$x
   	db=max(abs(beta-tmpp))
   	beta=tmpp
	# cat("beta=",beta,"db=",db,"\n")
   
   	#if(iter==200|db<=0.001) break
   	if (iter==200|db>=0) break
      
   	} # end: repeat


   db2=max(abs(beta-nbeta))  
   # cat("db2=", db2, "\n")
   nbeta=beta
   #bz=Z%*%beta
   #dL=dL/sum(dL*exp(-cumsum(dL)))
   #cat("nbeta=",nbeta,"\n")
   if(iterem==max.loops|db2<=tol) break

   } # end: repeat {   # 直到 EM 收斂為止




cat("average estimated untruncated rate =",mean(alpha),"\n")
# cat("beta=",beta,"db=",db,"\n")
B[R,]=beta
 
   I[R]=iterem
   BZ=c(1,1)%*%beta
   #DL[R,]=dL
   #NX[R,]=nT
   BZ=(c(1,1)%*%beta)[1,1]
   t0.8=R.in(G.in(-log(1-0.2))/exp(BZ));t0.5=R.in(G.in(-log(1-0.5))/exp(BZ));t0.2=R.in(G.in(-log(1-0.8))/exp(BZ)); 
   id0.8=sum(ifelse(nX<=t0.8,1,0));id0.5=sum(ifelse(nX<=t0.5,1,0));id0.2=sum(ifelse(nX<=t0.2,1,0))
   ST0.2[R]=exp(-G.fn(sum(dL[1:id0.2])*exp(BZ)))
   ST0.5[R]=exp(-G.fn(sum(dL[1:id0.5])*exp(BZ)))
   ST0.8[R]=exp(-G.fn(sum(dL[1:id0.8])*exp(BZ)))
  