
################################################################################
#                                                                              #
#  MLE of semiparametric transformation models with LTRC data by EM algorithm  #
#                                                                              #       
################################################################################

rm(list = ls())
library(nleqslv)
seed.ini=2000
tol=0.0001
max.loops=300
            

#############   setup   ####################  ##�ѼƳ]�w                                            
# ��ܼҫ�  
# �i��H�U�o3�� md
# md = "ph"; rho=0   
# md = "prop odds"; rho=1   
# md = "log trans"; rho=0.5  


# ���P�� md �Ptruncation rate �H�� censoring rate�A�t���P�� thetaV �P thetaD (�ѦҤ峹 Table 1)                              
# ���n�]�w���˼ơA�Ҧp�A���� q=0.3, pc=0.45���]�w�� 
# md = "log trans"; rho=0.5; thetaV=1/0.22; thetaD=1/0.47

                         
                                           
 md = "ph"; rho=0; thetaV=1/0.18; thetaD=1/0.82    
    

 md = "ph"; rho=0; thetaV=1/0.50; thetaD=1/0.42    
 md = "ph"; rho=0; thetaV=1/0.50; thetaD=1/0.76
 md = "ph"; rho=0; thetaV=1/0.18; thetaD=1/0.40


                                         
 n=200                                


############################################

  REP=500
  jack=n                                   
  sbeta=c(1,1)                             
  coef=1.96                                
                  

                         
##  inverse function of baseline harzard  ##  

R.in=function(x) {(x/5)^0.5}


##  method for est the dist of V  ##  ##IPW or profile

Vnt_est = "IPW"


## main program  ##

options(digits=6)

source("models.r")  

source("space.r")  # �x�s output

source("main_pgm-CM.r")
