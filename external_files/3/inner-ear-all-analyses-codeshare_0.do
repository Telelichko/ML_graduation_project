*--------------------------------------------------------------------------------
*                        Inner Ear Study: All Graphs and Figures
*
* Programming:
*   Stata Version:  Stata 15.0 MP.
*   Original Authors: Annika Jessen and Andrew Ho
*   Last Modified: June 11, 2018
* 	Notes: Sharing code for teaching introductory measurement methods. Example
* 		from Jessen et al. (2018)
* 		Ungated link: http://journals.sagepub.com/eprint/eeyzKdAtSUVdMWyZNZxH/full
*--------------------------------------------------------------------------------
*--------------------------------------------------------------------------------
* Stata Version:
    version 15.0
  
* Clearing
    clear all
	set more off
  
* Local directory:

* Local directory:
if c(username) == "adh796" {
	cd "C:\Users\adh796\Dropbox\AJ Advising Docs\IRT Paper"
}
if c(username) == "Annika" {
	cd "/Users/Annika/Desktop/FinalProj"
}

*--------------------------------------------------------------------------------
* Loading dataset, Rescale variables to the same scale in new variables 
*--------------------------------------------------------------------------------

    import excel "Raw Data\Export for AH AJ 11232016.xls", sheet("Sheet1") firstrow clear
	
	rename innerear11 ear11
	
	recode innereararray234innerear2a (1=0) (2=25) (3=50) (4=75) (5=100), gen(ear2a)
	recode innereararray234innerear2b (1=0) (2=25) (3=50) (4=75) (5=100), gen(ear2b)
	recode innereararray234innerear2c (1=0) (2=25) (3=50) (4=75) (5=100), gen(ear2c)
	recode innereararray234innerear2d (1=0) (2=25) (3=50) (4=75) (5=100), gen(ear2d)
	recode innereararray234innerear2e (1=0) (2=25) (3=50) (4=75) (5=100), gen(ear2e)
	recode innereararray234innerear3a (1=0) (2=25) (3=50) (4=75) (5=100), gen(ear3a)
	recode innereararray234innerear3b (1=0) (2=25) (3=50) (4=75) (5=100), gen(ear3b)
	recode innereararray234innerear4 (1=0) (2=25) (3=50) (4=75) (5=100), gen(ear4)
	recode innereararray5innerear5a (1=0) (2=33) (3=67) (4=100), gen(ear5a)
	recode innereararray5innerear5b (1=0) (2=33) (3=67) (4=100), gen(ear5b)
	
	destring leftwrs-rightptaair, replace

	keep ear11 ear2a-ear5b leftwrs-rightptaair
	order ear11 ear2a-ear5b leftwrs-rightptaair
	
	gen dataset = 2
	
	save "Stata Data Files\Dataset2.dta", replace
	
*--------------------------------------------------------------------------------
* dataset (appending additional dataset)
*-------------------------------------------------------------------------------- 
	use "Raw Data\Dataset1.dta", clear
	append using "Stata Data Files\Dataset2.dta"
	replace id = _n
	replace dataset = 1 if dataset == .	

*--------------------------------------------------------------------------------
* Rescale variables to the same scale (just ear11)
*--------------------------------------------------------------------------------

	gen ear11a = .
	replace ear11a = 0 if ear11 == 0
	replace ear11a = 10 if ear11 == 1
	replace ear11a = 20 if ear11 == 2
	replace ear11a = 30 if ear11 == 3
	replace ear11a = 40 if ear11 == 4
	replace ear11a = 50 if ear11 == 5
	replace ear11a = 60 if ear11 == 6
	replace ear11a = 70 if ear11 == 7
	replace ear11a = 80 if ear11 == 8
	replace ear11a = 90 if ear11 == 9
	replace ear11a = 100 if ear11 == 10
	
	order id ear11a ear2a-ear5b 
	
	save "Stata Data Files\DatasetAppended.dta", replace

*--------------------------------------------------------------------------------
* Duplicate check
*--------------------------------------------------------------------------------

	duplicates tag ear11a-ear5b, generate(dups)  
	*yes lots of duplicates 
	duplicates tag ear11a-ear5b if dataset==1, generate(dups1)
	*few 
	duplicates tag ear11a-ear5b if dataset == 2, generate(dups2)
	*few
	* Note first 46 IDs are unique to dataset 1.
	generate include = _n < 47 | dataset == 2
	duplicates tag ear11a-ear5b if include, generate(dupsfinal)
	tab dupsfinal // These are real-data duplicates
	drop if !include
	
	save "Stata Data Files\DatasetClean.dta", replace

*--------------------------------------------------------------------------------
* Examining and exploring the data
*--------------------------------------------------------------------------------

	use "Stata Data Files\DatasetClean.dta", clear
	tabstat ear11a-ear5b, stats (mean sd n min max) col(statistics)
	
	egen nmiss = rowmiss(ear11a-ear5b)
	tab nmiss
	
	* We can't do sum scores due to missing data. 
		*Creating composite scores. 
		* gen sumscore = ear2a+ear2b+ear2c+ear2d+ear2e+ear3a+ear3b+ear4+ear5a+ear5b
		* gen finalscore = sumscore/10
		* hist finalscore, discrete percent
		* graph save HistogramFinalScore
		
	egen mean11 = rowmean(ear11a-ear5b)
	hist mean11, percent yla(0(2)12)

*--------------------------------------------------------------------------------
* Classical test theory
*-------------------------------------------------------------------------------- 

* All items: ear11 ear2a ear2b ear2c ear2d ear2e ear3a ear3b ear4 ear5a ear5b
* 8 Items: ear2a-ear4 so items 23456789
* 3 Items: ear2d ear2a ear3a 457

	* Cronbach's alpha on all items. 
	alpha ear11a-ear5b, item asis
	*Note: total reliability is .93.
	
	alpha ear2a-ear4, item asis // Still .93
	alpha ear2d ear2a ear3a, item asis // .89 for 3 items
	alpha ear2a-ear3a, item asis // .92 now for 8 items
	
	egen mean3 = rowmean(ear2d ear2a ear3a)
	egen mean6 = rowmean(ear2a-ear3a)
	egen mean8 = rowmean(ear2a-ear4)
	
	*Showing Item-rest correlations to examine possible items to drop.
	matrix define itemrest = r(ItemRestCorr)'
	svmat itemrest
	gen index=_n if _n<=11
	graph twoway (scatter itemrest index, mlabel(index)) (scatter itemrest index, connect(1)), ///
	xlabel (1(1)11) ylabel (0(.2)1) legend(off) xtitle (Item Number) ytitle (Item-Rest Correlation) yline (.7) yline(1) yline(0) 

	*Correlations
	pwcorr ear11a-ear5b, sig star(.05)
	
	
*--------------------------------------------------------------------------------
* VALUES USED IN TABLE 1
*-------------------------------------------------------------------------------- 

	pwcorr ear11a-ear5b, sig star(.05)
	tabstat ear11a-ear5b, stats (mean sd n min max) col(statistics)
	
*-------------------------------------------------------------------------------- 
* Exploratory Factor analysis to see how many factors might emerge, 
* 	as everything seems to be correlated with each other. 

	mean (ear11a ear2a ear2b ear2c ear2d ear2e ear3a ear3b ear4 ear5a ear5b)
	factor ear11a-ear5b // , blanks (.3)
	** Exploratory Factor Analytic Scree Plot
	screeplot, lcolor(black) mcolor(black) yline(1, lcolor(black)) ///
			title("Scree Plot of Eigenvalues", color(black)) graphregion(color(white)) ///
			bgcolor(white) yla(0(1)6) xla(0(1)11) xtitle(Dimension Number) 
	factor ear11a-ear5b, factors(1)

** PCA Scree Plot	
	pca ear11a-ear5b
	screeplot, lcolor(black) mcolor(black) ///
		title("") graphregion(color(white)) ///
		bgcolor(white) yla(0(1)6, nogrid) xla(0(1)11) xtitle(Dimension Number) saving("Figures and Tables\Figure1.gph", replace)
	graph export "Figures and Tables\Figure1v2.png", replace width(1100) height(800)
	
** PCA Scree Plot, Unstandardized
	pca ear11a-ear5b, covar
	screeplot, lcolor(black) mcolor(black) ///
		title("") graphregion(color(white)) ///
		bgcolor(white) yla(, nogrid) xla(0(1)11) xtitle(Dimension Number)
		
***FIGURE USED IN APPENDIX
** histograms showing distributions of scores from all six polytomously scored items. Used in Appendix

	foreach var of varlist ear11 ear2a ear2b ear2c ear2d ear2e ear3a ear3b ear4 ear5a ear5b {
		hist `var', discrete percent addlabels fcolor(gs8) lcolor(gs15) ///
			graphregion(color(white)) bgcolor(white) name(`var'gph, replace) ///
			addlabopts(yvarformat(%4.0f)) xtitle(`var')
	}
	graph combine ear11gph ear2agph ear2bgph ear2cgph ear2dgph ear2egph ear3agph ///
		ear3bgph ear4gph ear5agph ear5bgph, graphregion(color(white)) saving("Figures and Tables\FigureA1.gph", replace)
	graph export "Figures and Tables\FigureA1v2.png", replace width(2200) height(1600)

	
*--------------------------------------------------------------------------------
* GRM Polytmous IRT for inner ear 
*-------------------------------------------------------------------------------- 	
set more off
irt grm ear11a-ear5b
predict theta, latent // Theta estimates

ereturn list
matrix list e(b)
*spit out all coefficients
matrix define COEFS = e(b)'
*store in a matrix called coefficients, aposotrophe transposes 60 columns single row. 
svmat COEFS

putexcel set "Figures and Tables\Inner Ear Raw Tables", replace

* Column 1, item labels
putexcel A1=("Item Code"), border(bottom)
local row = 4
putexcel A2 = "ear11 (b1-b5)"
putexcel A3 = "ear11 (b6-b10)"
foreach var of varlist ear2a ear2b ear2c ear2d ear2e ear3a ear3b ear4 ear5a ear5b {
	putexcel A`row' = ("`var'")
	local row = `row'+1
}

* Column 2, discrimination estimates
putexcel B1 = ("Discrimination Parameter Estimates"), border(bottom)
putexcel B2 = matrix(COEFS[1,1]), nformat(number_d2)
putexcel B4 = matrix(COEFS[2..11,1]), nformat(number_d2)

* Columns 3-7, location estimates 10x1, 4x8, 3x2
putexcel C1 = ("Location Parameter Estimates"), border(bottom)
putexcel C2 = matrix(COEFS[12..16,1]'/COEFS[1]), nformat(number_d2)
putexcel C3 = matrix(COEFS[17..21,1]'/COEFS[1]), nformat(number_d2)

*difference betwen slope intercept form and discrimination/difficulty form. 
*IRT spits our discrimination and difficulty, but actually spitting out slope intercept. 
*each location paramater BY its descrimination parameter. 


* For example:
* putexcel C4 = matrix(COEFS[22..25,1]'/COEFS[2]), nformat(number_d2)

forvalues i = 4/11 {
	putexcel C`i' = matrix(COEFS[4*`i'+6..4*`i'+9,1]'/COEFS[`i'-2]), nformat(number_d2)
}

* For example:
* putexcel C4 = matrix(COEFS[54..56,1]'/COEFS[10]), nformat(number_d2)

forvalues i = 12/13 {
	putexcel C`i' = matrix(COEFS[3*`i'+18..3*`i'+20,1]'/COEFS[`i'-2]), nformat(number_d2)
}


**FIGURE 2
**Combined Category characteristic curves. Note, couldn't get ear11 to fill in the full space
irtgraph icc ear11a, lcolor(black) title("ear11a", color(black)) legend(off) graphregion(color(white)) name(ear11,replace)
irtgraph icc ear2a, lcolor(black) title("ear2a", color(black)) legend(off) graphregion(color(white)) name(ear2a,replace)
irtgraph icc ear2b, lcolor(black) title("ear2b", color(black)) legend(off) graphregion(color(white)) name(ear2b,replace)
irtgraph icc ear2c, lcolor(black) title("ear2c", color(black)) legend(off) graphregion(color(white)) name(ear2c,replace)
irtgraph icc ear2d, lcolor(black) title("ear2d", color(black)) legend(off) graphregion(color(white)) name(ear2d,replace)
irtgraph icc ear2e, lcolor(black) title("ear2e", color(black)) legend(off) graphregion(color(white)) name(ear2e,replace)
irtgraph icc ear3a, lcolor(black) title("ear3a", color(black)) legend(off) graphregion(color(white)) name(ear3a,replace)
irtgraph icc ear3b, lcolor(black) title("ear3b", color(black)) legend(off) graphregion(color(white)) name(ear3b,replace)
irtgraph icc ear4, lcolor(black) title("ear4", color(black)) legend(off)  graphregion(color(white))  name(ear4,replace)
irtgraph icc ear5a, lcolor(black) title("ear5a", color(black)) legend(off) graphregion(color(white)) name(ear5a,replace)
irtgraph icc ear5b, lcolor(black) title("ear5b", color(black)) legend(off) graphregion(color(white)) name(ear5b,replace)


gr combine ear11 ear2a ear2b ear2c ear2d ear2e ear3a ear3b ear4 ear5a ear5b, graphregion(color(white)) ///
	saving("Figures and Tables\Figure2.gph", replace)
graph export "Figures and Tables\temp.png", replace width(3300) height(2400)

**FIGURE 2
**Combined Boundary characteristic curves. Note, couldn't get ear11 to fill in the full space
irtgraph icc ear11a, bcc lcolor(black) title("ear11a", color(black)) legend(off) graphregion(color(white)) name(ear11,replace)
irtgraph icc ear2a, bcc lcolor(black) title("ear2a", color(black)) legend(off) graphregion(color(white)) name(ear2a,replace)
irtgraph icc ear2b, bcc lcolor(black) title("ear2b", color(black)) legend(off) graphregion(color(white)) name(ear2b,replace)
irtgraph icc ear2c, bcc lcolor(black) title("ear2c", color(black)) legend(off) graphregion(color(white)) name(ear2c,replace)
irtgraph icc ear2d, bcc lcolor(black) title("ear2d", color(black)) legend(off) graphregion(color(white)) name(ear2d,replace)
irtgraph icc ear2e, bcc lcolor(black) title("ear2e", color(black)) legend(off) graphregion(color(white)) name(ear2e,replace)
irtgraph icc ear3a, bcc lcolor(black) title("ear3a", color(black)) legend(off) graphregion(color(white)) name(ear3a,replace)
irtgraph icc ear3b, bcc lcolor(black) title("ear3b", color(black)) legend(off) graphregion(color(white)) name(ear3b,replace)
irtgraph icc ear4, bcc lcolor(black) title("ear4", color(black)) legend(off)  graphregion(color(white))  name(ear4,replace)
irtgraph icc ear5a, bcc lcolor(black) title("ear5a", color(black)) legend(off) graphregion(color(white)) name(ear5a,replace)
irtgraph icc ear5b, bcc lcolor(black) title("ear5b", color(black)) legend(off) graphregion(color(white)) name(ear5b,replace)


gr combine ear11 ear2a ear2b ear2c ear2d ear2e ear3a ear3b ear4 ear5a ear5b, graphregion(color(white)) ///
	saving("Figures and Tables\Figure2.gph", replace)
graph export "Figures and Tables\Figure2bv2.png", replace width(3300) height(2400)
	
*Examining Item information function across values of theta. 
irtgraph iif, data("Stata Data Files\AnalysisStage2.ItemInformationFunctions.dta",replace)

save "Stata Data Files\AnalysisStage1.CTTandIRT.dta", replace

use "Stata Data Files\AnalysisStage2.ItemInformationFunctions.dta", clear

* All items: ear11 ear2a ear2b ear2c ear2d ear2e ear3a ear3b ear4 ear5a ear5b
* 8 Items: ear2a-ear4 so items 23456789
* 3 Items: ear2d ear2a ear3a 457

egen tifall = rowtotal(iif1-iif11)
egen tif8 = rowtotal(iif2-iif9)
egen tif3 = rowtotal(iif4 iif5 iif7)

gen csemall = 1/sqrt(tifall)
gen csem8 = 1/sqrt(tif8)
gen csem3 = 1/sqrt(tif3)

*--------------------------------------------------------------------------------
* FIGURE 3a and 3b used in paper: TIFs and IIFs labeling lowest discrim and two highest
*-------------------------------------------------------------------------------- 

* Combined

#delimit ;
graph twoway 
	(scatter iif1 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif2 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif3 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif4 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif5 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif6 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif7 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif8 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif9 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif10 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter iif11 theta, msymbol(i) connect(l) lcolor(gs14))
	(scatter tifall theta, msymbol(i) connect(l) lcolor(black))
	(scatter tif8 theta, msymbol(i) connect(l) lcolor(gs8))
	(scatter tif3 theta, msymbol(i) connect(l) lcolor(gs10)),
	legend(order(12 13 14) label(12 "11 Items") label(13 "8 Items") label(14 "3 Items") row(1)) graphregion(color(white))
	text(5 2 "ear3a")
	text(3 -.5 "ear2a")
	text(0.4 -1 "ear11")
	text(1.4 0 "ear5b")
	text(1.2 1.3 "ear5a")
	yline(0,lcolor(black) lwidth(vthin)) ylabel(,nogrid)
	ytitle(Item and Test Information) xtitle(Theta (Score Scale))
	;
#delimit cr

* Items Figure 3a

#delimit ;
graph twoway 
	(scatter iif1 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif2 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif3 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif4 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif5 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif6 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif7 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif8 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif9 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif10 theta, msymbol(i) connect(l) lcolor(gs4))
	(scatter iif11 theta, msymbol(i) connect(l) lcolor(gs4)),
	legend(off) graphregion(color(white)) plotregion(margin(none))
	text(4.5 2.6 "ear3a")
	text(3.65 -.5 "ear2a")
	text(1 1 "ear11")
	text(1.15 -.5 "ear5b")
	text(.6 1.5 "ear5a")
	text(2.7 1.5 "ear2d")
	ylabel(0(1)5,nogrid)
	ytitle(Item Information Functions) xtitle(Theta (Score Scale)) 
	saving("Figures and Tables\Figure3a.gph", replace)
	;
#delimit cr

graph export "Figures and Tables\Figure3av2.png", replace width(2200) height(1600) 

#delimit ;
graph twoway 
	(scatter tifall theta, msymbol(i) connect(l) lcolor(black))
	(scatter tif8 theta, msymbol(i) connect(l) lcolor(gs8))
	(scatter tif3 theta, msymbol(i) connect(l) lcolor(gs10)),
	legend(order(1 2 3) label(1 "11 Items") label(2 "8 Items") label(3 "3 Items") row(1)) graphregion(color(white))
	plotregion(margin(none))
	ylabel(,nogrid)
	ytitle(Test Information Functions) xtitle(Theta (Score Scale))
	saving("Figures and Tables\Figure3b.gph", replace)
	;
#delimit cr

graph export "Figures and Tables\Figure3bv2.png", replace width(2200) height(1600) 


#delimit ;
graph twoway 
	(scatter csemall theta if csemall < 1 & theta > -3 & theta < 3, msymbol(i) connect(l) lcolor(black))
	(scatter csem8 theta if csem8 < 1 & theta > -3 & theta < 3, msymbol(i) connect(l) lcolor(gs8))
	(scatter csem3 theta if csem3 < 1 & theta > -3 & theta < 3, msymbol(i) connect(l) lcolor(gs10)),
	legend(order(1 2 3) label(1 "11 Items") label(2 "8 Items") label(3 "3 Items") row(1)) graphregion(color(white))
	plotregion(margin(none))
	ylabel(0(.2)1,nogrid) xla(-3(1)3)
	ytitle(Conditional Standard Error of Measurement) xtitle(Theta (Score Scale))
	saving("Figures and Tables\Figure3c.gph", replace)
	;
#delimit cr

graph export "Figures and Tables\Figure3cv2.png", replace width(2200) height(1600) 

save "Stata Data Files\AnalysisStage2.ItemInformationFunctions.dta", replace

*--------------------------------------------------------------------------------
* Correlational analysis
*-------------------------------------------------------------------------------- 

use "Stata Data Files\AnalysisStage1.CTTandIRT.dta", clear

	egen ymiss = rowmiss(leftwrs rightwrs leftptaair rightptaair)
	tab ymiss
	drop if ymiss == 4
	
*******Histograms and boxplots to identify outliers	
	hist leftwrs, freq name(gph1, replace)
	graph box leftwrs
	hist rightwrs, freq name(gph2, replace)
	graph box rightwrs
	hist leftptaair, freq name(gph3, replace)
	graph box leftptaair
	hist rightptaair, freq name(gph4, replace)
	graph box rightptaair
	
	graph combine gph1 gph2 gph3 gph4
	
	gen negleftpta = -leftptaair
	gen negrightpta = -rightptaair
	
	pwcorr theta mean3 mean6 mean8 mean11 leftwrs rightwrs negleftpta negrightpta, star(.05)

*--------------------------------------------------------------------------------
* Log transformations of variables 
*-------------------------------------------------------------------------------- 
	gen log_rightwrs= -log(102-rightwrs)
	gen log_leftwrs= -log(102-leftwrs)
	gen log_rightptaair= -log(rightptaair)
	gen log_leftptaair= -log(leftptaair)
	
	hist log_rightwrs, freq name(lgph1, replace)
	hist log_leftwrs, freq name(lgph2, replace)
	hist log_rightptaair, freq name(lgph3, replace)
	hist log_leftptaair, freq name(lgph4, replace)
	
	graph combine lgph1 lgph2 lgph3 lgph4
		
	pwcorr theta mean3 mean6 mean8 mean11 log_leftwrs log_rightwrs log_leftptaair log_rightptaair, obs star(.05)

	corrci theta mean3 mean6 mean8 mean11 log_leftwrs log_rightwrs log_leftptaair log_rightptaair
		*Note, Corrci uses listwise deletion. Hence the following foreach loop to calculate correlations one by one

	foreach x of varlist mean3 mean6 mean8 mean11 log_leftwrs log_rightwrs log_leftptaair log_rightptaair {
	foreach y of varlist mean3 mean6 mean8 mean11 log_leftwrs log_rightwrs log_leftptaair log_rightptaair {
	corrci `x' `y'
	} 
}

foreach x of varlist mean3 mean6 mean8 mean11 {
	foreach y of varlist log_leftwrs log_rightwrs log_leftptaair log_rightptaair {
	corrci `x' `y'
	} 
}
		
save "Stata Data Files\AnalysisStage3.Correlations.dta", replace

graph drop _all


*--------------------------------------------------------------------------------
* Graded response model threshold map
*-------------------------------------------------------------------------------- 

import excel using "Figures and Tables\Inner Ear Raw Tables.xlsx", firstrow clear

drop if _n < 3 | _n > 10
rename LocationParameterEstimates cp1
rename D cp2
rename E cp3
rename F cp4 

keep Item cp1-cp4
xpose, clear varname
drop if _n == 1

forvalues i=1/8 {
	gen num`i' = `i'
}

gen labels = ""
replace labels = "Fair" if _n == 1
replace labels = "Good" if _n == 2
replace labels = "VGood" if _n == 3
replace labels = "Exclnt" if _n == 4

#delimit ;
graph twoway
	(scatter num8 v1, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num7 v2, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num6 v3, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num5 v4, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num4 v5, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num3 v6, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num2 v7, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5))
	(scatter num1 v8, msize(vsmall) mla(labels) mlabsize(small) mlabcolor(gs0) mcolor(gs0) mlabgap(*.5)),
	ylabel(1 "Mood based on hearing" 
	2 "Hear soft household sounds"
	3 "Hear in different situations"
	4 "Understand telephone"
	5 "Understand and filter"
	6 "Understand crowded restaurant"
	7 "Understand quiet room"
	8 "Understand family and friends", tlength(0) ang(h) labsize(small))
	xlabel(-2(1)3.5) legend(off) xtitle(Theta) graphregion(color(white))
	saving("Figures and Tables\Figure4.gph", replace)
;
#delimit cr

graph export "Figures and Tables\Figure4.png", replace width(2200) height(1600) 
