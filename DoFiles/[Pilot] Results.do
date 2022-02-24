clear all
set more off

cd ""

use "Data/Pilot_ABD.dta"

//Table C4
reg percent_correct linkTreat incentiveTreat primeTreat, robust
reg percent_correct linkTreat incentiveTreat primeTreat i.diet, robust

//Table C2
tab linkTreat incentiveTreat
