/*
import delimited "C:\Users\51950005\Desktop\Empirical Corp Finance\class3_bankruptcy laws and credit access\Assignment1\Final1\PanelData_table5.csv
*/
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
gen Y1992=drtown*y1
gen Y1993=drtown*y2
gen Y1994=drtown*y3
gen Y1995=drtown*y4
gen Y1996=drtown*y5
gen Y1997=drtown*y6
gen Y1998=drtown*y7
gen Y1999=drtown*y8
gen Y2000=drtown*y9
gen Y2001=drtown*y10
gen Y2002=drtown*y11
gen Y2003=drtown*y12
xtreg borrowing Y1992-Y2003, fe cluster(state)
coefplot, drop(_cons) xline(70) vertical
/*
graph export "C:\Users\51950005\Desktop\Empirical Corp Finance\class3_bankruptcy laws and credit access\Assignment1\Final1\Preexisting_trends.png", as(png) replace
*/
