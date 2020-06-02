use "C:\Users\51950005\Desktop\Empirical Corp Finance\class3_bankruptcy laws and credit access\Assignment1\Final1\predictdata_drtge.dta"
keep if ut==0 & tag==1 | tag==-1
snapspan sid year workdrt, generate(year0) replace
stset year, failure(workdrt) id(sid) origin(time y(1993))
eststo: stcox meanatbc, nohr
eststo: stcox assetspretrend, nohr
eststo: stcox profitspretrend, nohr
esttab
esttab using predicting_drtge, b(3) t(3) label title("Survival Analysis of DRT Adoption")
estimates clear
eststo: stcox meanatbc, tvc(gr pccredit share sharegrowth casespc judgepc congress janata communist regional ally) texp(1) nohr
eststo: stcox assetspretrend, tvc(gr pccredit share sharegrowth casespc judgepc congress janata communist regional ally) texp(1) nohr
eststo: stcox profitspretrend, tvc(gr pccredit share sharegrowth casespc judgepc congress janata communist regional ally) texp(1) nohr
esttab using predicting_drtge1, b(3) t(3) label title("Survival Analysis of DRT Adoption")
