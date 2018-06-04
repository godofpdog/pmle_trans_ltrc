
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
            

#############   setup   ####################  ##參數設定                                            
# 選擇模型  
# 可選以下這3種 md
# md = "ph"; rho=0   
# md = "prop odds"; rho=1   
# md = "log trans"; rho=0.5  


# 不同的 md 與truncation rate 以及 censoring rate，配不同的 thetaV 與 thetaD (參考文章 Table 1)                              
# 但要設定為倒數，例如，對應 q=0.3, pc=0.45的設定為 
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

source("space.r")  # 儲存 output

source("main_pgm-CM.r")
