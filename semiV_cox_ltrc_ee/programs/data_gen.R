


      thetaV=sthetaV
      thetaD=sthetaD

      beta=sbeta
      N=0
      z1=rbinom(nn,1,0.5)
      z2=rnorm(n,0,1) 
      Z=matrix(c(z1,z2),n,length_beta)
      C=T=V=X=D=U=rep(NA,n)
      bz=Z%*%beta
      CCC=NULL
      for(i in 1:n){

         repeat{
 
            N=N+1
            u=runif(1)
            TT=R.in(G.in(-log(1-u))/exp(bz[i]))
            DD=rexp(1,thetaD)
            VV=rexp(1,thetaV)
            #VV=runif(1,0,0.5)
            CC=VV+DD
            CCC[N]=CC
            if(TT>=VV) break
            
         }

         T[i]=TT
         V[i]=VV
         D[i]=DD
         C[i]=CC
      }         
      

      
      delta=ifelse(T<=C,1,0)
      X=delta*T+(1-delta)*C
      pc=sum(delta==0)/n
      q=(N-n)/N  

      PC[R]=pc
      Q[R]=q

      cat("the truncated rate is",q,"\n")
      cat("the censored rate is",pc,"\n")

      iter=0

      Order=order(X)
      Xp=sort(X)
      nX=X[Order]
      nV=V[Order]
      nD=D[Order]
      ndelta=delta[Order]
      Z=Z[Order,]
      bz=Z%*%beta
      k=n

      cdL=dL=rep(NA,n)
      dL[1]=nX[1]/10
      for(i in 2:n){
         dL[i]=(nX[i]-nX[i-1])/10
      }
      cdL=5*nX^2
      dL[1]=5*nX[1]^2
      for(i in 2:n){
         dL[i]=cdL[i]-cdL[i-1]
      }
      