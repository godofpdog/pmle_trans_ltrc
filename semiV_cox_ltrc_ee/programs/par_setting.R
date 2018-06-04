
##  setting  ##

n=n=100
sbeta=c(1,1)
thetaV=1/0.18
thetaD=1/0.84
rho=0
REP=1000

## inverse of baseline hazard  ##

R.in=function(x) (x/5)^0.5

## model selection ( ph only )  ##

md="ph"

source(file.path("programs/models.r"))
source(file.path("programs/space.r"))

##  data_gen  ##

for(R in 1:1000){

   cat(R,"\n")
   source(file.path("programs/data_gen.r"))


}


cat("\n\n")
cat("the average trancated rate is : ",mean(Q),"\n")
cat("the average censored rate is : ",mean(PC),"\n")