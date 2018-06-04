
##  Breslow's estimator  ##

bz=Z%*%beta

t0.2=R.in(-log(1-0.8));t0.5=R.in(-log(1-0.5));t0.8=R.in(-log(1-0.2))

Breslow_est_fn=function(t,v=0,x,bz,type="cen"){
   tmp1=NULL
   if(length(x)<=1) {
      stop("data is too small")
   }
   if(type=="ltrc"){
      for(i in 1:length(x)){
         tmp1[i]=1-ndelta[i]/sum(exp(bz)*1*(v<=x[i])*(x[i]<=x))
      }
   }
   if(type=="cen"){
      for(i in 1:length(x)){
         tmp1[i]=1-ndelta[i]/sum(exp(bz)*1*(x[i]<=x))
      }   
   }
   surv=cumprod(tmp1)[sum(1*(x<=t))]      
   return(surv)
}

ST0.2[R]=Breslow_est_fn(t0.2,v=nV,x=nX,bz,type="ltrc")
ST0.5[R]=Breslow_est_fn(t0.5,v=nV,x=nX,bz,type="ltrc")
ST0.8[R]=Breslow_est_fn(t0.8,v=nV,x=nX,bz,type="ltrc")