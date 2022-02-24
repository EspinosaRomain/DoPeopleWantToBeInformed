clear all
set more off

cd ""

import excel "Data/Regression impact.xlsx", sheet("Feuil1") firstrow

foreach var in impact AK InfoSeeking InfoPassive FakeIDK Incorrect Selective Unconscious ICEI CD s sprime incorrectHNL{
	destring `var', replace
}


replace impact=impact/100

//Correlation
pwcorr ICEI impact, sig

//Look at declared ignorance
reg impact incorrectHNL //Adjusted R2: 48.5%, R2=51,5

//Look at the clicks
reg impact shareClickHL

//Regress with ICEI
reg impact ICEI //Adjusted-Rsquared: 56.0% // R2=58.6%

//Adjusted ICEI
gen ICEIadj=ICEI-s*(InfoSeeking+InfoPassive)

//Compare the new ICEIbis
reg impact ICEIadj //Adjusted R2: 66.1% //R2=68.1%

//Look at declared ignorance corrected for CD
reg impact ICEIadj CD //ADjusted 2: 75.9% //R2=78.7%

