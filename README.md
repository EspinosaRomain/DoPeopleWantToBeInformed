# DoPeopleWantToBeInformed

This repository contains the data and scripts to reproduce Espinosa & Stoop, Experimental Economics, 2021: Do people really want to be informed? 

The project contains folders wit data, Stata codes (do-files), and R scripts.

DoFiles:
1) [ABD] Results: computes the estimates shown in the paper for Diet treatment
	- Table C1: column 1
	- Table C2: column 1
	- Table C3: columns 1 & 2 
2) [Alcohol] Results: computes the estimates shown in the paper for Alcohol treatment
	- Table C1: column 2
	- Table C2: column 2
	- Table C3: columns 3 & 4
3) [Immigration] Results: computes the estimates shown in the paper for Immigration treatment
	- Table C1: column 3
	- Table C2: column 3
	- Table C3: columns 5 & 6 
4) [InfoABD] Results: computes the estimates shown in the paper for Immigration treatment
	- Table C1: column 4
5) [InfoAlcohol] Results: computes the estimates shown in the paper for Immigration treatment
	- Table C1: column 5
6) [InfoImmigration] Results: computes the estimates shown in the paper for Immigration treatment
	- Table C1: column 6
7) [DiD] InfoImpact: difference-in-difference estimation
	- Table 5
8) [HeavyDrinkers] Results: Impact of incentives on heavy drinkers
	- Robustness check #6
9) [Impact] Results: regression of the information campaigns' impact
	- Table 6
10) [Placebo] Results: Look at incentive effect in placebo treatment
	- Robustness check #2
11) [Pilot] Results: Look at the order effect in the pilot
	- Robustness check #5

R Scripts:
1) MLE on full sample: computes the estimates of Table 4 (columns: 1-3-5)
2) Robustness checks MLE.
	- Robustness check #4 (focusing on the three first questions)
	- Robustness check #3 (omitting one question each time)
	- Table 8
3) Bootstrap Process: bootstraps the MLEs
4) Analyze Bootstrap Results: Analyze the results of the bootstrap process
	- Table 4: columns 2-4-6
	- Table C5


