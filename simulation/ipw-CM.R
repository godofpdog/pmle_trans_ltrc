#   theta=thetaV 
#   for(j in 1:n){
#      Rj=sum(1*(nV<=nV[j])*(nV[j]<=nX))   # number of at risk set at time cV[j]
#      }
#   Fnv=cumprod(1-ndelta/Rj)

##################
   

#   Vh=function(Fnv, ndelta){
#      vh=rep(NA,n)
#      #Fnv=Fnv-0.001
#      Fnv[Fnv==1]=Fnv[Fnv==1]-0.01
#      Fnv=Fnv[-n]
#      ndelta=ndelta[-n]
#      for(i in 1:n){
#         id=ifelse(nV<=nX[i],1,0)
#         id=id[-n]
#         vh[i]=1/sum(1/(1-Fnv),na.rm=TRUE)*sum(id/(1-Fnv),na.rm=TRUE)
#         }
#      return(vh)
#      }

#   plot(Vnt,type="l",ylab="CDFs",xlab="times")
#   lines(pexp(nX,theta),col=2,lty=2)

###########################

   indata=cbind(nX, ndelta, nV)

   event=indata[indata[,"ndelta"]==1,]
   event.time=as.numeric(row.names(table(factor(event[,"nX"]))))
   K=length(event.time)    # ¦@ K­Ó distinct failure times
   event.names=rownames(1:K, do.NULL = FALSE, prefix = "tilde.T")
   event.time.table=matrix(data.frame(table(factor(event[,"nX"])))$Freq,ncol=1,dimnames=list(event.names,c("freq")))
   dk=as.numeric(event.time.table[,"freq"])


   Y.nX=matrix(0, n, K)
   for( k in 1:K){
	Y.nX[, k]=(indata[,"nX"]>=event.time[k]-10^(-10))*(indata[,"nV"]<=event.time[k]+10^(-10))
	}

   R.nX=rep(0, K) 
   Snx=cumprod(1-dk/ apply(Y.nX, 2, sum))  # SDF of T at uncensored nX, CDF:F=1-SDF
   Fnv=rep(0, n)   # F at V[i]
   for (i in 1:n) {
	Fnv[i]=c(0, 1-Snx)[sum(nV[i]>=c(0, event.time)-10^(-10))]
	}


   Vh=function(Fnv, ndelta){   # ¨D K(t) at time nX
      Fnv0=Fnv
      vh=rep(NA,n)
      Fnv0[Fnv0==1]=Fnv0[Fnv0==1]-0.01
      Fnv0=Fnv0[-n]
      ndelta=ndelta[-n]
      for(i in 1:n){
         id=ifelse(nV<=nX[i],1,0)
         id=id[-n]
         vh[i]=1/sum(1/(1-Fnv0), na.rm=TRUE)*sum(id/(1-Fnv0), na.rm=TRUE)
         }
      return(vh)
      }

   Vnt=Vh(Fnv, ndelta) 

   plot(nX[order(nX)], Vnt[order(nX)],type="l",ylab="CDFs",xlab="times", ylim=c(0, 1))
   lines(sort(nX), pexp(sort(nX),thetaV),col=2,lty=3)

   legend(0.6,0.65,c("est_Vnt","true_Vnt"),col=c(1:2),lty=c(1:2))
   est_Vnt=Vnt  # est_Vnt: K(t) at time nX 

