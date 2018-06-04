   cat("estimating theta of Vnt ","\n")
   theta=sthetaV
   thetad=sthetaD
   G.ff=function(x,theta){ ifelse(x>=0,1-exp(-theta*x),0)}

   qn=qo=rep(1/n,n)
   iter=0
      
   repeat{  

   iter=iter+1
  
   for(j in 1:n){
       
      tmpq2=tmpq3=rep(0,n)
      tmpq1=(1/G.ff(nX[j],thetad)*qo[j])
      tmpq3=rep(0,n)

      for(l in 1:j){
         tmpq3=rep(0,n)
         for(i in l:n){     
            tmpq3[i]=1/(G.ff(nX[i],thetad))*qo[i]
         }
         tmpq2[l]=(1-ndelta[l])/sum(tmpq3)
      }
   qn[j]=(ndelta[j]+tmpq1*sum(tmpq2))/n
   }

   dq=max(abs(qo-qn))
   qo=qn
      
   if(iter==300|dq<=1e-5) break
   #cat("iter=",iter,"\n")
   }

   qn
       
   Lp=function(x,nx,nv){  

      PL1=-(exp(-x * nx) * nx/(1 - exp(-x * nx))^2)
      PL2=1/(1-exp(-x*nx))
      PR1=(exp(-x * nv) - x * (exp(-x * nv) * nv))/(x * exp(-x * nv))
      PR2=exp(-x * nx) * nx/(1 - exp(-x * nx))

      tmp4=tmp3=rep(NA,n)

      for(i in 1:n){
         tmp11=tmp1=rep(0,n)
         for(j in i:n){
            tmp1[j]=PL1[j]*qn[j]
            tmp11[j]=PL2[j]*qn[j]
         }
      
         tmp3[i]=sum(tmp1)/sum(tmp11)*(1-ndelta[i])
         tmp4[i]=-PR2[i]*ndelta[i]+PR1[i]
      }
      return(sum(tmp3)+sum(tmp4))
   }
   
   ntheta=nleqslv(x=thetad,Lp,nx=nX,nv=nV)$x

   dt=abs(ntheta-theta)
   theta=ntheta
   cat("est_theta= ",theta,"\n")
      
   Vnt=pexp(nX,theta)
   ln=n*0.8
   plot(Vnt,type="l",ylab="CDFs",xlab="times")
   lines(pexp(nX,thetaV),col=2)
   legend(ln,0.8,c("est_Vnt","true_Vnt"),col=c(1:2),lty=1)
   est_Vnt=Vnt