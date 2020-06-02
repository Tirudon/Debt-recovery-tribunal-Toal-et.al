/* import the PanelData_table5.csv file 
import delimited "C:\Users\51950005\Desktop\Empirical Corp Finance\class3_bankruptcy laws and credit access\Assignment1\Final1\PanelData_table5.csv" */

gen early=1 if borrcluster<=5
replace early=0 if borrcluster>5 & borrcluster<.
gen nodrtyears = drtyear-1993
gen drtyears = 2003 - nodrtyears - 1994
rename year t
rename tangibleassets Lassets
gen firstregressionsyear = 1992
gen lastregressionsyear = 2003
encode companyname, gen(company)
xtset company t
tab t, gen(y)
tab state, gen(s)
foreach i of numlist 1/20 {
gen st`i'=s`i'*(t-1991)
}
foreach i of numlist 1/12 {
gen ys`i'=y`i'*Lassets
}
foreach i of numlist 1/20 {
gen sts`i'=st`i'*Lassets
}
gen drtsize=drtown*Lassets
eststo clear
xtreg borrowing drtown y1-y12 if t>=firstregressionsyear & t<=lastregressionsyear, fe cluster(state)
estadd scalar numberofgroups = e(N_g)
eststo
xtreg borrowing drtown y1-y12 st1-st20 if t>=firstregressionsyear & t<=lastregressionsyear, fe cluster(state)
estadd scalar numberofgroups = e(N_g)
eststo
xtreg borrowing drtown drtsize y1-y12 ys1-ys12 if t>=firstregressionsyear & t<=lastregressionsyear, fe cluster(state)
estadd scalar numberofgroups = e(N_g)
eststo
xtreg borrowing drtown drtsize y1-y12 ys1-ys12 st1-st20 sts1-sts20 if t>=firstregressionsyear & t<=lastregressionsyear, fe cluster(state)
estadd scalar numberofgroups = e(N_g)
eststo
xtreg borrowing drtown drtsize y1-y12 ys1-ys12 st1-st20 if t>=firstregressionsyear & t<=lastregressionsyear, fe cluster(state)
estadd scalar numberofgroups = e(N_g)
eststo
esttab, keep(drtown drtsize)
/* specify the location for storing results; cd "C:\Users\51950005\Desktop\Empirical Corp Finance\class3_bankruptcy laws and credit access\Assignment1\Final1"*/
esttab using Table5.doc
esttab using Table5.xls

