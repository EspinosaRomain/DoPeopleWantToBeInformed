clear 
set more off

cd ""

use "Data/Data_Alcohol.dta"

//Summary statistics by treatment
//Table C3: columns 3 and 4
//Table C2: column 2
mat sumStat=J(9,5,.)
 
local k=1
foreach var in hypo_nolink hypo_idk hypo_link  inc10_nolink inc10_link   inc30_nolink inc30_link  inc50_nolink inc50_link{
	su percent_correct if treatment=="`var'"
	mat sumStat[`k',1]=round(`r(mean)',0.001)
	mat sumStat[`k',2]=round(`r(sd)',0.001)
	mat sumStat[`k',5]=round(`r(N)',0.001)
	local k=`k'+1
}
su percent_dk if treatment=="hypo_idk"
mat sumStat[2,3]=round(`r(mean)',0.001)
mat sumStat[2,4]=round(`r(sd)',0.001)

matrix rownames sumStat="hypo_nolink" "hypo_idk" "hypo_link"  "inc10_nolink" "inc10_link"   "inc30_nolink" "inc30_link"  "inc50_nolink" "inc50_link"

mat list sumStat

//Regressions
//Table 3: Columns 3 and 4
reg percent_correct linkTreat incent10Treat incent30Treat incent50Treat dkTreat, robust
test incent10Treat=incent30Treat
test incent10Treat=incent50Treat
test incent30Treat=incent50Treat
reg percent_correct linkTreat incent10Treat incent30Treat incent50Treat dkTreat i.use age_with_missing missing_age i.stud_num i.sex_num i.employment_num, robust
test incent10Treat=incent30Treat
test incent10Treat=incent50Treat
test incent30Treat=incent50Treat

//Table for demographics
//Table C1: column 2
mat Demographics=J(15,1,.)
su stud_num if studentstatus=="Yes"
mat Demographics[1,1]=round(`r(N)'/1800,0.001)*100
su stud_num if studentstatus=="No"
mat Demographics[2,1]=round(`r(N)'/1800,0.001)*100
su stud_num if studentstatus=="Missing"
mat Demographics[3,1]=round(`r(N)'/1800,0.001)*100
su sex_num if sex=="Male"
mat Demographics[4,1]=round(`r(N)'/1800,0.001)*100
su sex_num if sex=="Female"
mat Demographics[5,1]=round(`r(N)'/1800,0.001)*100
su sex_num if sex=="Missing"
mat Demographics[6,1]=round(`r(N)'/1800,0.001)*100
su age
mat Demographics[7,1]=round(`r(mean)',0.1)
su missing_age
mat Demographics[8,1]=round(`r(mean)',0.001)*100
su employment_num if employmentstatus=="Due to start a new job within the next month"
mat Demographics[9,1]=round(`r(N)'/1800,0.001)*100
su employment_num if employmentstatus=="Full-Time"
mat Demographics[10,1]=round(`r(N)'/1800,0.001)*100
su employment_num if employmentstatus=="Not in paid work (e.g. homemaker, retired or disabled)"
mat Demographics[11,1]=round(`r(N)'/1800,0.001)*100
su employment_num if employmentstatus=="Other"
mat Demographics[12,1]=round(`r(N)'/1800,0.001)*100
su employment_num if employmentstatus=="Part-Time"
mat Demographics[13,1]=round(`r(N)'/1800,0.001)*100
su employment_num if employmentstatus=="Unemployed (and job seeking)"
mat Demographics[14,1]=round(`r(N)'/1800,0.001)*100
su employment_num if employmentstatus=="Missing"
mat Demographics[15,1]=round(`r(N)'/1800,0.001)*100
mat list Demographics

//Kruskal-Wallist test
//Note small error in the paper: p=0.543 and not p=0.802
kwallis use, by(treatment)

//Time to complete the questionnaire
gen timeQuestions= timingpagesubmitq1+ timingpagesubmitq2+timingpagesubmitq3+timingpagesubmitq4+timingpagesubmitq5+timingpagesubmitq6
gen minutes=timeQuestions/60

//Regressions
//Table 7: Columns 3 and 4
reg percent_correct linkTreat incentiveTreat dkTreat
reg percent_correct linkTreat incentiveTreat dkTreat minutes


