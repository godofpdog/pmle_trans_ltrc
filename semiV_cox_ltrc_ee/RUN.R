###################################################################
# coef est for Cox model by EE with LTRC data when V~G(x,thetaV)  #
###################################################################

rm(list = ls())
library(nleqslv)

## setting working directory

setwd("G:/semiV_cox_ltrc_ee")

## setting parameters

nn=n=100
REP=50
sbeta=c(1,1)
sthetaV=1/0.18
sthetaD=1/0.84
rho=0

## inverse of baseline hazard

R.in=function(x) (x/5)^0.5

## model selection ( ph only )

md="ph"

## main programs

par(mfrow=c(4,1))
source(file.path("programs/models.r"))
source(file.path("programs/space.r"))
source(file.path("programs/main_pgm.r"))