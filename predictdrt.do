//This do file generates the regressions for Table II in the article.

clear
clear matrix
set mem 10m
cd f:\budesktopbackup\data\drtge\predictdrt
cap log close
log using predictdrt.log, replace

use predictdata_drtge, replace

keep if ut==0 & tag==1 | tag==-1
snapspan sid year workdrt, generate(year0) replace
stset year, failure(workdrt) id(sid) origin(time y(1993))

ereturn clear
estimates clear

eststo: stcox meanatbc, nohr
eststo: stcox assetspretrend, nohr
eststo: stcox profitspretrend, nohr

eststo: stcox meanatbc, tvc(gr pccredit share sharegrowth casespc judgepc congress janata communist regional ally) texp(1) nohr

eststo: stcox assetspretrend, tvc(gr pccredit share sharegrowth casespc judgepc congress janata communist regional ally) texp(1) nohr

eststo: stcox profitspretrend, tvc(gr pccredit share sharegrowth casespc judgepc congress janata communist regional ally) texp(1) nohr

esttab using predicting_drtge, b(3) t(3) label title("Survival Analysis of DRT Adoption") /*
*/ addnote("A Cox proportional hazards model is fitted to the time taken to establish a DRT in a state. As indicated, explanatory variables include the 1990-92 averages of total bank credit, firm assets, and firm profits in this state, state GDP, its growth rate, per capita total bank credit, the share of small scale industries in total bank credit, the growth rate of this share, per capita pending high court cases, number of high court judges per capita, dummies for political party in the state government, and a dummy for whether the political party in state government was allied with the party in the national government. In results not shown, each of these variables is also entered separately, without any significant effects. ") tex replace

ereturn clear
estimates clear

log close
