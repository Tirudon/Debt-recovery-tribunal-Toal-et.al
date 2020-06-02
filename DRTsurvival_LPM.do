import delimited "C:\Users\51950005\Desktop\Empirical Corp Finance\class3_bankruptcy laws and credit access\Assignment1\Final1\predictdata_drtge.csv"
keep if ut==0 & tag==1 | tag==-1
snapspan sid year workdrt, generate(year0) replace
stset year, failure(workdrt) id(sid) origin(time y(1993))
generate early=0
replace early=1 if drtyear>=1995
eststo: reg early meanatbc
eststo: reg early assetspretrend
eststo: reg early profitspretrend
esttab using LPM1, b(3) t(3) label title("DRT adoption using LPM")
estimates clear
eststo: reg early meanatbc gr pccredit share sharegrowth casespc judgepc congress janata communist regional
eststo: reg early assetspretrend gr pccredit share sharegrowth casespc judgepc congress janata communist regional
eststo: reg early profitspretrend gr pccredit share sharegrowth casespc judgepc congress janata communist regional
esttab using LPM2, b(3) t(3) label title("DRT adoption using LPM - time varying variables")
