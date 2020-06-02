log using table7.log
import delimited "C:\Users\51950005\Desktop\Empirical Corp Finance\class3_bankruptcy laws and credit access\Assignment1\Final\PanelData_table7.csv"
gen early=1 if borrcluster<=5
replace early=0 if borrcluster>5 & borrcluster<.
gen nodrtyears = drtyear-1993
gen drtyears = 2003 - nodrtyears - 1994
sort early
rename year t
rename tangibleassets Lassets
encode companyname, gen(company)
xtset company t
tab t, gen(y)
foreach i of numlist 1/5 {
gen ys`i'=y`i'*Lassets
}
gen drtsize=drtown*Lassets
gen earlytrend=early*t
gen earlyassettrend=early*t*Lassets
gen tdrtyear=drtyears*t
gen tdrtyearass= drtyears*t*Lassets
xtreg borrowing earlytrend i.t  if  t>=1989 &  t<=1993 , fe cluster(state)
eststo
xtreg borrowing  earlytrend  earlyassettrend y1-y5 ys1-ys5 if t>=1989 & t<=1993 , fe cluster(state)
eststo
xtreg borrowing tdrtyear y1-y5   if t>=1989 & t<=1993, fe cluster(state)
eststo
xtreg borrowing tdrtyear tdrtyearass y1-y5 ys1-ys5 if t>=1989 & t<=1993, fe cluster(state)
eststo
esttab using Table7.doc
esttab, keep(earlytrend earlyassettrend tdrtyear tdrtyearass)
log close
