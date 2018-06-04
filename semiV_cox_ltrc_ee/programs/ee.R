nnD=sort(nD)
orderD=order(nD)
nndelta=ndelta[orderD]
thetaV=theta
S_Dx=function(x){
   N_Du=R_Du=NULL
   length_x=length(x)
   for(i in 1:length_x){
      N_Du[i]=sum(1*(nnD==x[i])*1*(nndelta==0))
      R_Du[i]=sum(nnD>=x[i])/n
   }
   return(cumprod(1-N_Du/R_Du/n))
}

EE_beta_score2=function(x){
   bz=Z%*%x
   ns=max(which(ndelta==1))
   sb=matrix(NA,ns,2)
   tmp1=matrix(NA,n,2)
   tmp2=NULL
   for(i in 1:ns){
      for(j in 1:n){
         tmp1[j,]=(Z[j,]*(1*(nX[j]>=nX[i])*ndelta[j])/K_Dx_hat[j]*exp(bz)[j])
         tmp2[j]=(1*(nX[j]>=nX[i])*ndelta[j])/K_Dx_hat[j]*exp(bz[j])
      }
   sb[i,]=(Z[i,]-colSums(tmp1)/sum(tmp2))*ndelta[i]
   }
   return(colSums(sb))
}
plot(S_Dx((nnD)),xlab="time",ylab="S_D(t)")
plot(nnD,xlab="i-th obs",ylab="time points")
lines(nX,col=2)
ln=max(nD)*0.8
legend(ln,ln,c("Di","Xi"),col=c(1:2),lty=1)
dS_Dx_hat=S_Dx_hat=S_Dx((nnD))
dS_Dx_hat[1]=0
for(i in 2:n){
   dS_Dx_hat[i]=S_Dx_hat[i-1]-S_Dx_hat[i]
}
plot(dS_Dx_hat,xlab="time")
K_Dx_hat=NULL
for(i in 1:n){
   if(min(nnD-nX[i])<0){
      K_Dx_hat[i]=pexp(nX[i],thetaV)-sum(dS_Dx_hat[1:max(which(nnD<=nX[i]))]*pexp(nnD[1:max(which(nnD<=nX[i]))],thetaV))
   }
   else{
      K_Dx_hat[i]=pexp(nX[i],thetaV)-sum(dS_Dx_hat*pexp(nnD,thetaV))
      cat("ww","\n")
   }
}
for(i in 1:n){
   if(min(nnD-nX[i])<0){
      K_Dx_hat[i]=pexp(nX[i],thetaV)-sum(dS_Dx_hat[1:max(which(nnD<=nX[i]))]*pexp(nX[i]-nnD[1:max(which(nnD<=nX[i]))],thetaV))
   }
   else{
      K_Dx_hat[i]=pexp(nX[i],thetaV)-sum(dS_Dx_hat*pexp(nnD,thetaV))
      cat("ww","\n")
   }         
}
beta=nleqslv(x=beta,EE_beta_score2)$x
cat("beta_hat is : ",beta,"\n\n") 
B[R,]=beta