#Set working directory
setwd("")

#Import packages
library(foreign)
library(optimx)
library(sandwich)

#Import data

#For Diet
#myData=read.dta("dataForMLE_ABD_WithoutHealth.dta")
#myData=read.dta("dataForMLE_ABD_WithoutCarcin.dta")
#myData=read.dta("dataForMLE_ABD_WithoutCO2.dta")
#myData=read.dta("dataForMLE_ABD_WithoutSoy.dta")
#myData=read.dta("dataForMLE_ABD_WithoutMilk.dta")
#myData=read.dta("dataForMLE_ABD_WithoutEgg.dta")
#myData=read.dta("dataForMLE_ThreeFirstQuestions.dta")

#For Alcohol
#myData=read.dta("dataForMLE_Alcohol_WithoutHeart.dta")
#myData=read.dta("dataForMLE_Alcohol_WithoutEarn.dta")
#myData=read.dta("dataForMLE_Alcohol_WithoutDepend.dta")
#myData=read.dta("dataForMLE_Alcohol_WithoutCarcin.dta")
#myData=read.dta("dataForMLE_Alcohol_WithoutAge.dta")
#myData=read.dta("dataForMLE_Alcohol_WithoutTraffic.dta")
#myData=read.dta("dataForMLE_Alochol_ThreeFirstQuestions.dta")

#For Immigration
#myData=read.dta("dataForMLE_Immigration_WithoutVisa.dta")
#myData=read.dta("dataForMLE_Immigration_WithoutBaby.dta")
#myData=read.dta("dataForMLE_Immigration_WithoutConstruct.dta")
#myData=read.dta("dataForMLE_Immigration_WithoutMoney.dta")
#myData=read.dta("dataForMLE_Immigration_WithoutJob.dta")
#myData=read.dta("dataForMLE_Immigration_WithoutAsylum.dta")
#myData=read.dta("dataForMLE_Immigration_ThreeFirstQuestions.dta")

#For the discussion
#myData=read.dta("dataForMLE_ABD_AnimalWelfare.dta")
#myData=read.dta("dataForMLE_ABD_Environment.dta")
#myData=read.dta("dataForMLE_ABD_Health.dta")

myData$percent_incorrect=ifelse(myData$myTreat=="HNL3",1-myData$percent_correct-myData$percent_dk,1-myData$percent_correct)

#LL function
LL <- function(v) {
  AK=v[1]
  infoSeeking=v[2]
  infoPassive=v[3]
  fakeIDK=v[4]
  fakeIncorrect=v[5]
  selective=v[6]
  unconscious=v[7]
  s=v[8]
  sprime=v[9]
  #trueIncorrect=selective+unconscious
  #trueIDK=trueIDK_LC+trueIDK_HC
  
  mu_HNL3=AK
  mu_HNL3_DK=infoSeeking+infoPassive+fakeIDK
  mu_HNL=AK+s*(infoSeeking+infoPassive)+sprime*fakeIDK
  mu_HL=AK+s*infoPassive+infoSeeking+sprime*fakeIDK
  mu_INL=AK+s*(infoSeeking+infoPassive)+fakeIDK+fakeIncorrect
  mu_IL=AK+infoPassive+infoSeeking+fakeIDK+fakeIncorrect+selective
  mu_HNL_Incor=selective+unconscious+fakeIncorrect+(1-s)*(infoSeeking+infoPassive)+(1-sprime)*fakeIDK
  mu_HNL3_Incor=selective+unconscious+fakeIncorrect
  mu_HL_Incor=selective+unconscious+fakeIncorrect+(1-s)*infoPassive+(1-sprime)*fakeIDK
  mu_INL_Incor=selective+unconscious+(1-s)*(infoSeeking+infoPassive)
  mu_IL_Incor=unconscious
  
  sigma_HNL=sigma_HNL_Incor=v[10]
  sigma_HNL3=v[11]
  sigma_HL=sigma_HL_Incor=v[12]
  sigma_INL=sigma_INL_Incor=v[13]
  sigma_IL=sigma_IL_Incor=v[14]
  sigma_HNL3_DK=v[15]
  sigma_HNL3_Incor=v[16]
  
  ll_HNL = log(dnorm(workingDataHNL$percent_correct, mu_HNL, sigma_HNL))
  ll_HNL=ifelse(ll_HNL==-Inf,-1000,ll_HNL)
  ll_HNL3 = log(dnorm(workingDataHNL3$percent_correct, mu_HNL3, sigma_HNL3))
  ll_HNL3=ifelse(ll_HNL3==-Inf,-1000,ll_HNL3)
  ll_HL = log(dnorm(workingDataHL$percent_correct, mu_HL, sigma_HL))
  ll_HL=ifelse(ll_HL==-Inf,-1000,ll_HL)
  ll_INL = log(dnorm(workingDataINL$percent_correct, mu_INL, sigma_INL))
  ll_INL=ifelse(ll_INL==-Inf,-1000,ll_INL)
  ll_IL = log(dnorm(workingDataIL$percent_correct, mu_IL, sigma_IL))
  ll_IL=ifelse(ll_IL==-Inf,-1000,ll_IL)
  
  ll_HNL3_DK = log(dnorm(workingDataHNL3$percent_dk, mu_HNL3_DK, sigma_HNL3_DK))
  ll_HNL3_DK =ifelse(ll_HNL3_DK==-Inf,-1000,ll_HNL3_DK)
  
  ll_HNL_Incor = log(dnorm(workingDataHNL$percent_incorrect, mu_HNL_Incor, sigma_HNL_Incor))
  ll_HNL_Incor=ifelse(ll_HNL_Incor==-Inf,-1000,ll_HNL_Incor)
  ll_HNL3_Incor = log(dnorm(workingDataHNL3$percent_incorrect, mu_HNL3_Incor, sigma_HNL3_Incor))
  ll_HNL3_Incor=ifelse(ll_HNL3_Incor==-Inf,-1000,ll_HNL3_Incor)
  ll_HL_Incor = log(dnorm(workingDataHL$percent_incorrect, mu_HL_Incor, sigma_HL_Incor))
  ll_HL_Incor=ifelse(ll_HL_Incor==-Inf,-1000,ll_HL_Incor)
  ll_INL_Incor = log(dnorm(workingDataINL$percent_incorrect, mu_INL_Incor, sigma_INL_Incor))
  ll_INL_Incor=ifelse(ll_INL_Incor==-Inf,-1000,ll_INL_Incor)
  ll_IL_Incor = log(dnorm(workingDataIL$percent_incorrect, mu_IL_Incor, sigma_IL_Incor))
  ll_IL_Incor=ifelse(ll_IL_Incor==-Inf,-1000,ll_IL_Incor)
  
  weight_HNL=1/2
  weight_HL=dim(workingDataHL)[1]/dim(workingDataHNL)[1]/2
  weight_IL=dim(workingDataIL)[1]/dim(workingDataHNL)[1]/2
  weight_INL=dim(workingDataINL)[1]/dim(workingDataHNL)[1]/2
  weight_HNL3=dim(workingDataHNL3)[1]/dim(workingDataHNL)[1]/3
  
  return(weight_HNL*(sum(ll_HNL)+sum(ll_HL_Incor))+weight_HL*(sum(ll_HL)+sum(ll_HNL_Incor))+weight_IL*(sum(ll_IL)+sum(ll_IL_Incor))+weight_INL*(sum(ll_INL)+sum(ll_INL_Incor))+weight_HNL3*(sum(ll_HNL3)+sum(ll_HNL3_DK)+sum(ll_HNL3_Incor)))
}

#Optimization parameters
startVal=c(rep(0.1,9),rep(1,7))
lower=rep(0,16)
upper=c(rep(1,9),rep(+Inf,7))

#generate subdata
dataHNL=myData[myData$myTreat=="HNL",]
dataHNL3=myData[myData$myTreat=="HNL3",]
dataHL=myData[myData$myTreat=="HL",]
dataINL=myData[myData$myTreat=="INL",]
dataIL=myData[myData$myTreat=="IL",]

#Bootstrap Data
workingDataHNL=dataHNL
workingDataHNL3=dataHNL3
workingDataHL=dataHL
workingDataINL=dataINL
workingDataIL=dataIL

myOptim=optim(par = startVal, LL, method="L-BFGS-B", lower=lower, upper=upper, hessian=FALSE, control=list("fnscale"=-1, maxit=5000))

myOptim

round(myOptim$par,3)

#Scores
print(paste0("Accepted Knowledge:", round(myOptim$par[1],3)))
print(paste0("infoSeeking:", round(myOptim$par[2],3)))
print(paste0("infoPassive:", round(myOptim$par[3],3)))
print(paste0("Fake IDK:", round(myOptim$par[4],3)))
print(paste0("Fake Incorrect:", round(myOptim$par[5],3)))
print(paste0("Selective ignorance:", round(myOptim$par[6],3)))
print(paste0("Unconscious ignorance:", round(myOptim$par[7],3)))

CD=round(myOptim$par[4]+myOptim$par[5]+myOptim$par[6],3)
print(paste0("CD:", CD))

ICEI=round(myOptim$par[2]+myOptim$par[3]+myOptim$par[7],3)
print(paste0("ICEI:", ICEI))


