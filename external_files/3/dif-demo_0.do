*--------------------------------------------------------------------------------
*                         S-061: Educational Measurement
*                        Data-Analytic Handout - Class 09
*
* Class 09: Differential Item Functioning
*
* Programming:
*   Stata Version:  Stata 14.1 SE.
*   Original Author: Andrew Ho
*   Last Modified:  October 2, 2017.
*--------------------------------------------------------------------------------

*--------------------------------------------------------------------------------
* Set the critical parameters of the computing environment.
*--------------------------------------------------------------------------------
* Specify the version of Stata for this analysis [Need Stata 14 for IRT!]:
    version 14.1
  
* Clear all computer memory and delete any existing stored graphs and matrices:
    clear all
	set more off
  
* Define the local directory [CHANGE THIS]:
    cd "C:\Users\adh796\Dropbox\S-061\S-061 2017\"	

*-----------------------------------------------------------------------------------
*
* Differential Item Functioning by the Cochran Maentel Haenszel Test
*
*-----------------------------------------------------------------------------------

use http://www.stata-press.com/data/r14/masc2, clear

egen sumscore = rowtotal(q1-q9)

tab(female), summarize(sumscore)

difmh q1-q9, group(female) noyates // Group variable is 0/1 where 1 indicates the focal

display invchi2(1,.95)  			// Critical value of chi-sq on 1 df.

difmh q1-q9, group(female) total(sumscore) noyates // Same thing
gen sumscoreX = sumscore
replace sumscoreX = 99 if sumscore==3
replace sumscoreX = 43 if sumscore==7
difmh q1-q9, group(female) total(sumscoreX) noyates // Same thing, demonstrating
													// categorical nature of CMH test
											
* Finally, note equivalence of difmh and mhodds command, with mhodds command
* including a nonuniform DIF test of homogeneity of odds ratios.  For q1:

mhodds q1 female, by(sumscore)
difmh q1, group(female) total(sumscore) noyates  

*-----------------------------------------------------------------------------------
*
* Differential Item Functioning by Logistic Regression
*
*-----------------------------------------------------------------------------------

* For reference, logistic regression approach on all 
foreach var of varlist q1-q9 {
	egen sumscore_n`var' = rowtotal(q1-q9)
	replace sumscore_n`var' = sumscore_n`var' - `var'
	logit `var' sumscore_n`var' female, nolog // Uniform DIF
	logit `var' c.sumscore_n`var'##i.female, nolog // Nonuniform DIF
}

 * Just focus on Item q4
 capture egen sumscore_nq4 = rowtotal(q1-q3 q5-q9)
 // Uniform DIF
 logit q4 sumscore_nq4 female
 // Probability space
graph twoway (function y = 1/(1+exp(-(_b[_cons]+_b[sumscore_nq4]*x))), ///
	clcolor(red) range(0 10)) ///
	(function y = 1/(1+exp(-(_b[_cons]+_b[sumscore_nq4]*x+_b[female]))), ///
	clcolor(blue) range(0 10)), ///
	xtitle(Score (0-8) not including Item 4) ytitle(Estimated P(Correct)) ///
	legend(label(1 Male) label(2 Female)) ylabel(0(.2)1) yline(0) yline(1)
 // Logit space
graph twoway (function y = _b[_cons]+_b[sumscore_nq4]*x, ///
	clcolor(red) range(0 10)) ///
	(function y = _b[_cons]+_b[sumscore_nq4]*x+_b[female], ///
	clcolor(blue) range(0 10)), ///
	xtitle(Score (0-8) not including Item 4) ytitle(Estimated Logits) ///
	legend(label(1 Male) label(2 Female))
 
 // Nonuniform DIF
 logit q4 c.sumscore_nq4##i.female
 // Probability space
 graph twoway (function y = 1/(1+exp(-(_b[_cons]+_b[sumscore_nq4]*x))), ///
	clcolor(red) range(0 10)) ///
	(function y = 1/(1+exp(-(_b[_cons]+_b[sumscore_nq4]*x+_b[1.female]+ ///
	_b[1.female#c.sumscore_nq4]*x))), ///
	clcolor(blue) range(0 10)), ///
	xtitle(Score (0-8) not including Item 4) ytitle(Estimated P(Correct)) ///
	legend(label(1 Male) label(2 Female)) ylabel(0(.2)1) yline(0) yline(1)
	
// Logit space
graph twoway (function y = _b[_cons]+_b[sumscore_nq4]*x, ///
	clcolor(red) range(0 10)) ///
	(function y = _b[_cons]+_b[sumscore_nq4]*x+_b[1.female]+ ///
	_b[1.female#c.sumscore_nq4]*x, ///
	clcolor(blue) range(0 10)), ///
	xtitle(Score (0-8) not including Item 4) ytitle(Estimated Logits) ///
	legend(label(1 Male) label(2 Female))
 

