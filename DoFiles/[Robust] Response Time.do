clear all
set more off

cd ""

use "Data/Data_forResponseTime.dta"

//For Robustness Check #1
su timeQuestions if link==0 & treat!="HNL3"
su timeQuestions if link==1 & treat!="HNL3"
ranksum timeQuestions if treat!="HNL3", by(link)

su timeQuestions if incentivized==0 & treat!="HNL3"
su timeQuestions if incentivized==1 & treat!="HNL3"
ranksum timeQuestions if treat!="HNL3", by(incentivized)
