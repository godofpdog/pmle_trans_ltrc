
## main program ##

R=1

while(R<=REP){

cat("replication : ",R,"\n")

iter_rep=data.frame(iter=R)

write.table(iter_rep,file.path("output/replications.txt"),quote=F)

source(file.path("programs/data_gen.r"))

source(file.path("programs/est_thetaV.r"))

source(file.path("programs/ee.r"))

source(file.path("programs/Breslow.r"))

result_tmp=data.frame(beta1=B[,1],beta2=B[,2],st0.2=ST0.2,st0.5=ST0.5,st0.8=ST0.8)

write.table(result_tmp,file.path("output/result_tmp.txt"),quote=F)

R=R+1

}

if(R==(REP+1)) source(file.path("programs/result.r"))