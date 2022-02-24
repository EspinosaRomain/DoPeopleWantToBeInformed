clear 
set more off

cd ""

use  "Data/Data_DiD.dta"

reg percent_correct ABD Alcohol Immigrants ABDinfo Alcoholinfo Immigrantsinfo, nocons
test ABDinfo=Alcoholinfo
test ABDinfo=Immigrantsinfo
test Alcoholinfo=Immigrantsinfo
