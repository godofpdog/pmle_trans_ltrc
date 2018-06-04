## main program ##


R=1
seed.mrep=seed.ini
while(R<=REP){

	seed.mrep=seed.mrep+1
	set.seed(seed.mrep)
	cat("seed.mrep=", seed.mrep, "\n")


	cat("replication : ",R,"\n")

	iter_rep=data.frame(iter=R)

	write.table(iter_rep, file.path("replications.txt"),quote=F)

	source("data_gen.r")


	if(Vnt_est=="IPW") source("ipw-CM.R") 

	source("em-CM.r")


	result_tmp=data.frame(beta1=B[,1],beta2=B[,2],st0.2=ST0.2,st0.5=ST0.5,st0.8=ST0.8)

	write.table(result_tmp, file.path("result_tmp.txt"),quote=F)

 	if ( R>=2) {
		cat("Average beta estimates=", apply(B[1:R,], 2, mean), "\n")
		cat("sd of beta estimates=", apply(B[1:R,], 2, sd), "\n")
		cat("MSE of beta estimates=", apply((B[1:R,]-1)^2, 2, mean), "\n")
		cat("the average truncated rate is : ", mean(Q[1:R]),"\n")
		cat("the average censoring rate is : ", mean(PC[1:R]),"\n")
		}
 

	R=R+1

	}

	if(R==(REP+1)) source("result.r")




