clear all
set more off

cd ""

use "Data/Placebo_Data.dta"

gen percent_correct=total_correct/6
su percent_correct if treatment=="hypo"
su percent_correct if treatment=="incentive"
ranksum percent_correct, by(treatment)
