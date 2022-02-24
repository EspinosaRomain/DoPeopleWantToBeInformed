#Set working directory
setwd("")

#Import libraries
library(pastecs)

#Topic
#topic="ABD"
#topic="Alcohol"
#topic="Immigration"
#topic="heavyDrinkers"
#topic="heavyMeatEaters"
#topic="extremeRight"

#Import bootstrap Data
myBootRes=read.table(paste0("BootstrapResults/resultsBootstrap",topic,"_10000.csv"), sep = ";", header=TRUE)
myBootRes=na.omit(myBootRes)

#Compute the ICEI
myBootRes$ICEI=myBootRes$infoSeeking+myBootRes$infoPassive+myBootRes$Unconscious
myBootRes$TotalCD=myBootRes$FakeIDK+myBootRes$FakeIncorrect+myBootRes$Selective

#Prepare Matrix for results
resBoot=matrix(nrow=11,ncol=11,data=NA)
rownames(resBoot)=c("AcceptedKnowledge","infoSeeking", "infoPassive","FakeIDK", "FakeIncorrect","Selective","Unconscious","s","sprime","ICEI","TotalCD")
colnames(resBoot)=c("Mean","Standard Error","Lower 90%-CI","Upper 90%-CI","Reject at 10%","Lower 95%-CI","Upper 95%-CI","Reject at 5%","Lower 99%-CI","Upper 99%-CI","Reject at 1%")

#Get Means
resBoot[1,1]=round(mean(myBootRes$AcceptedKnowledge),3)
resBoot[2,1]=round(mean(myBootRes$infoSeeking),3)
resBoot[3,1]=round(mean(myBootRes$infoPassive),3)
resBoot[4,1]=round(mean(myBootRes$FakeIDK),3)
resBoot[5,1]=round(mean(myBootRes$FakeIncorrect),3)
resBoot[6,1]=round(mean(myBootRes$Selective),3)
resBoot[7,1]=round(mean(myBootRes$Unconscious),3)
resBoot[8,1]=round(mean(myBootRes$s),3)
resBoot[9,1]=round(mean(myBootRes$sprime),3)
resBoot[10,1]=round(mean(myBootRes$ICEI),3)
resBoot[11,1]=round(mean(myBootRes$TotalCD),3)

#Get standard errors
resBoot[1,2]=round(sqrt(sum((myBootRes$AcceptedKnowledge-resBoot[1,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[2,2]=round(sqrt(sum((myBootRes$infoSeeking-resBoot[2,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[3,2]=round(sqrt(sum((myBootRes$infoPassive-resBoot[3,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[4,2]=round(sqrt(sum((myBootRes$FakeIDK-resBoot[4,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[5,2]=round(sqrt(sum((myBootRes$FakeIncorrect-resBoot[5,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[6,2]=round(sqrt(sum((myBootRes$Selective-resBoot[6,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[7,2]=round(sqrt(sum((myBootRes$Unconscious-resBoot[7,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[8,2]=round(sqrt(sum((myBootRes$s-resBoot[8,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[9,2]=round(sqrt(sum((myBootRes$sprime-resBoot[9,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[10,2]=round(sqrt(sum((myBootRes$ICEI-resBoot[10,1])^2)/(dim(myBootRes)[1]-1)),3)
resBoot[11,2]=round(sqrt(sum((myBootRes$TotalCD-resBoot[11,1])^2)/(dim(myBootRes)[1]-1)),3)

#Get p-values
#for(i in 1:9){
#  resBoot[i,3]=2*pnorm(-abs(resBoot[i,1]/resBoot[i,2]))  
#}

#Intervals

#90%
resBoot[1,3]=round(quantile(myBootRes$AcceptedKnowledge, probs = c(0.05)),3)
resBoot[2,3]=round(quantile(myBootRes$infoSeeking, probs = c(0.05)),3)
resBoot[3,3]=round(quantile(myBootRes$infoPassive, probs = c(0.05)),3)
resBoot[4,3]=round(quantile(myBootRes$FakeIDK, probs = c(0.05)),3)
resBoot[5,3]=round(quantile(myBootRes$FakeIncorrect, probs = c(0.05)),3)
resBoot[6,3]=round(quantile(myBootRes$Selective, probs = c(0.05)),3)
resBoot[7,3]=round(quantile(myBootRes$Unconscious, probs = c(0.05)),3)
resBoot[8,3]=round(quantile(myBootRes$s, probs = c(0.05)),3)
resBoot[9,3]=round(quantile(myBootRes$sprime, probs = c(0.05)),3)
resBoot[10,3]=round(quantile(myBootRes$ICEI, probs = c(0.05)),3)

resBoot[1,4]=round(quantile(myBootRes$AcceptedKnowledge, probs = c(0.95)),3)
resBoot[2,4]=round(quantile(myBootRes$infoSeeking, probs = c(0.95)),3)
resBoot[3,4]=round(quantile(myBootRes$infoPassive, probs = c(0.95)),3)
resBoot[4,4]=round(quantile(myBootRes$FakeIDK, probs = c(0.95)),3)
resBoot[5,4]=round(quantile(myBootRes$FakeIncorrect, probs = c(0.95)),3)
resBoot[6,4]=round(quantile(myBootRes$Selective, probs = c(0.95)),3)
resBoot[7,4]=round(quantile(myBootRes$Unconscious, probs = c(0.95)),3)
resBoot[8,4]=round(quantile(myBootRes$s, probs = c(0.95)),3)
resBoot[9,4]=round(quantile(myBootRes$sprime, probs = c(0.95)),3)
resBoot[10,4]=round(quantile(myBootRes$ICEI, probs = c(0.95)),3)

resBoot[1,5]=ifelse(resBoot[1,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[2,5]=ifelse(resBoot[2,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[3,5]=ifelse(resBoot[3,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[4,5]=ifelse(resBoot[4,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[5,5]=ifelse(resBoot[5,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[6,5]=ifelse(resBoot[6,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[7,5]=ifelse(resBoot[7,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[8,5]=ifelse(resBoot[8,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[9,5]=ifelse(resBoot[9,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")
resBoot[10,5]=ifelse(resBoot[10,3]>0,"Reject H0 at 10%","Don't reject H0 at 10%")

#95%
resBoot[1,6]=round(quantile(myBootRes$AcceptedKnowledge, probs = c(0.025)),3)
resBoot[2,6]=round(quantile(myBootRes$infoSeeking, probs = c(0.025)),3)
resBoot[3,6]=round(quantile(myBootRes$infoPassive, probs = c(0.025)),3)
resBoot[4,6]=round(quantile(myBootRes$FakeIDK, probs = c(0.025)),3)
resBoot[5,6]=round(quantile(myBootRes$FakeIncorrect, probs = c(0.025)),3)
resBoot[6,6]=round(quantile(myBootRes$Selective, probs = c(0.025)),3)
resBoot[7,6]=round(quantile(myBootRes$Unconscious, probs = c(0.025)),3)
resBoot[8,6]=round(quantile(myBootRes$s, probs = c(0.025)),3)
resBoot[9,6]=round(quantile(myBootRes$sprime, probs = c(0.025)),3)
resBoot[10,6]=round(quantile(myBootRes$ICEI, probs = c(0.025)),3)

resBoot[1,7]=round(quantile(myBootRes$AcceptedKnowledge, probs = c(0.975)),3)
resBoot[2,7]=round(quantile(myBootRes$infoSeeking, probs = c(0.975)),3)
resBoot[3,7]=round(quantile(myBootRes$infoPassive, probs = c(0.975)),3)
resBoot[4,7]=round(quantile(myBootRes$FakeIDK, probs = c(0.975)),3)
resBoot[5,7]=round(quantile(myBootRes$FakeIncorrect, probs = c(0.975)),3)
resBoot[6,7]=round(quantile(myBootRes$Selective, probs = c(0.975)),3)
resBoot[7,7]=round(quantile(myBootRes$Unconscious, probs = c(0.975)),3)
resBoot[8,7]=round(quantile(myBootRes$s, probs = c(0.975)),3)
resBoot[9,7]=round(quantile(myBootRes$sprime, probs = c(0.975)),3)
resBoot[10,7]=round(quantile(myBootRes$ICEI, probs = c(0.975)),3)

resBoot[1,8]=ifelse(resBoot[1,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[2,8]=ifelse(resBoot[2,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[3,8]=ifelse(resBoot[3,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[4,8]=ifelse(resBoot[4,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[5,8]=ifelse(resBoot[5,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[6,8]=ifelse(resBoot[6,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[7,8]=ifelse(resBoot[7,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[8,8]=ifelse(resBoot[8,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[9,8]=ifelse(resBoot[9,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")
resBoot[10,8]=ifelse(resBoot[10,6]>0,"Reject H0 at 5%","Don't reject H0 at 5%")

#99%
resBoot[1,9]=round(quantile(myBootRes$AcceptedKnowledge, probs = c(0.005)),3)
resBoot[2,9]=round(quantile(myBootRes$infoSeeking, probs = c(0.005)),3)
resBoot[3,9]=round(quantile(myBootRes$infoPassive, probs = c(0.005)),3)
resBoot[4,9]=round(quantile(myBootRes$FakeIDK, probs = c(0.005)),3)
resBoot[5,9]=round(quantile(myBootRes$FakeIncorrect, probs = c(0.005)),3)
resBoot[6,9]=round(quantile(myBootRes$Selective, probs = c(0.005)),3)
resBoot[7,9]=round(quantile(myBootRes$Unconscious, probs = c(0.005)),3)
resBoot[8,9]=round(quantile(myBootRes$s, probs = c(0.005)),3)
resBoot[9,9]=round(quantile(myBootRes$sprime, probs = c(0.005)),3)
resBoot[10,9]=round(quantile(myBootRes$ICEI, probs = c(0.005)),3)

resBoot[1,10]=round(quantile(myBootRes$AcceptedKnowledge, probs = c(0.995)),3)
resBoot[2,10]=round(quantile(myBootRes$infoSeeking, probs = c(0.995)),3)
resBoot[3,10]=round(quantile(myBootRes$infoPassive, probs = c(0.995)),3)
resBoot[4,10]=round(quantile(myBootRes$FakeIDK, probs = c(0.995)),3)
resBoot[5,10]=round(quantile(myBootRes$FakeIncorrect, probs = c(0.995)),3)
resBoot[6,10]=round(quantile(myBootRes$Selective, probs = c(0.995)),3)
resBoot[7,10]=round(quantile(myBootRes$Unconscious, probs = c(0.995)),3)
resBoot[8,10]=round(quantile(myBootRes$s, probs = c(0.995)),3)
resBoot[9,10]=round(quantile(myBootRes$sprime, probs = c(0.995)),3)
resBoot[10,10]=round(quantile(myBootRes$ICEI, probs = c(0.995)),3)

resBoot[1,11]=ifelse(resBoot[1,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[2,11]=ifelse(resBoot[2,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[3,11]=ifelse(resBoot[3,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[4,11]=ifelse(resBoot[4,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[5,11]=ifelse(resBoot[5,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[6,11]=ifelse(resBoot[6,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[7,11]=ifelse(resBoot[7,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[8,11]=ifelse(resBoot[8,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[9,11]=ifelse(resBoot[9,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")
resBoot[10,11]=ifelse(resBoot[10,9]>0,"Reject H0 at 1%","Don't reject H0 at 1%")


#Summary Results
sumResults=matrix(ncol=3,nrow=11,data=NA)
rownames(sumResults)=c("AcceptedKnowledge","infoSeeking", "infoPassive","FakeIDK", "FakeIncorrect","Selective","Unconscious","s","sprime","ICEI","TotalCD")
colnames(sumResults)=c("Mean","Standard Error","Decision")

sumResults[,1:2]=resBoot[,1:2]
for(i in 1:10){
  if(resBoot[i,3]==0){
    sumResults[i,3]="Don't reject H0"
  }
  if(resBoot[i,3]>0){
    sumResults[i,3]="Reject at 10%"
  }
  if(resBoot[i,6]>0){
    sumResults[i,3]="Reject at 5%"
  }
  if(resBoot[i,9]>0){
    sumResults[i,3]="Reject at 1%"
  }
}
sumResults



