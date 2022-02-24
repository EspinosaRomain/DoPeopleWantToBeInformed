clear 
set more off

cd ""

use  "Data/Data_InfoImmigration.dta"


//Table for demographics
mat Demographics=J(15,1,.)
su stud_num if studentstatus=="Yes"
mat Demographics[1,1]=round(`r(N)'/198,0.001)*100
su stud_num if studentstatus=="No"
mat Demographics[2,1]=round(`r(N)'/198,0.001)*100
su stud_num if studentstatus=="Missing"
mat Demographics[3,1]=round(`r(N)'/198,0.001)*100
su sex_num if sex=="Male"
mat Demographics[4,1]=round(`r(N)'/198,0.001)*100
su sex_num if sex=="Female"
mat Demographics[5,1]=round(`r(N)'/198,0.001)*100
su sex_num if sex=="Missing"
mat Demographics[6,1]=round(`r(N)'/198,0.001)*100
su age
mat Demographics[7,1]=round(`r(mean)',0.1)
su missing_age
mat Demographics[8,1]=round(`r(mean)',0.001)*100
su employment_num if employmentstatus=="Due to start a new job within the next month"
mat Demographics[9,1]=round(`r(N)'/198,0.001)*100
su employment_num if employmentstatus=="Full-Time"
mat Demographics[10,1]=round(`r(N)'/198,0.001)*100
su employment_num if employmentstatus=="Not in paid work (e.g. homemaker, retired or disabled)"
mat Demographics[11,1]=round(`r(N)'/198,0.001)*100
su employment_num if employmentstatus=="Other"
mat Demographics[12,1]=round(`r(N)'/198,0.001)*100
su employment_num if employmentstatus=="Part-Time"
mat Demographics[13,1]=round(`r(N)'/198,0.001)*100
su employment_num if employmentstatus=="Unemployed (and job seeking)"
mat Demographics[14,1]=round(`r(N)'/198,0.001)*100
su employment_num if employmentstatus=="Missing"
mat Demographics[15,1]=round(`r(N)'/198,0.001)*100
mat list Demographics

//Append data from main experiment
append using  "Data//Data_Immigration.dta", force

keep if treatment=="immigrant" | treatment=="Hypo_NoLink"
replace Political=political if Political==.
ranksum Political, by(treatment)
