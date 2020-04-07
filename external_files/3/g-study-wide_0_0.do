*--------------------------------------------------------------------------------
*                         S-061: Educational Measurement
*                        Data-Analytic Handout - Class 04
*
* Class 04: Generalizability Theory
*
* Programming:
*   Stata Version:  Stata 14.1 SE.
*   Original Author: Andrew Ho
*   Last Modified:  September 12, 2017.
*--------------------------------------------------------------------------------

*--------------------------------------------------------------------------------
* Set the critical parameters of the computing environment.
*--------------------------------------------------------------------------------
* Specify the version of Stata to be used in the analysis:
    version 14.1
  
* Clear all computer memory and delete any existing stored graphs and matrices:
    clear all
	set more off
  
* Define the local directory [CHANGE THIS]:
    cd "C:\Users\adh796\Dropbox\S-061A2\S-061 2017"	
 	
*-----------------------------------------------------------------------------------
*
* Input the raw pxixr dataset
*
*-----------------------------------------------------------------------------------	
	
	use "pxixr.dta", clear
	
*-----------------------------------------------------------------------------------
* Reshape from double-wide to double-long
* See: 	http://www.ats.ucla.edu/stat/stata/faq/doublewide.htm
*-----------------------------------------------------------------------------------
	
	local i = 1
	foreach var of varlist r1item1-r3item5 {
		rename `var' score`i'
		local i = `i'+1
	}
	
	reshape long score, i(person) j(seq)
	recode seq (1 6 11=1) (2 7 12=2) (3 8 13=3) (4 9 14=4) (5 10 15=5), gen(item)
	recode seq (1/5=1) (6/10=2) (11/15=3), gen(rater)
	drop seq
	order person item rater score
	label variable item ""
	label variable rater ""
	table person item rater, c(mean score)
	
	save pxixr_long.dta, replace

*-----------------------------------------------------------------------------------
*
* Multilevel modeling approach to a pxixr Generalizability Study
*
*-----------------------------------------------------------------------------------

	* capture the error (if you haven't done this already, which we have...)
	* mixed can't do factor "#"notation for random effects, so we need this.
	capture egen pXi = group(person item)  
	capture egen pXr = group(person rater)
	capture egen iXr = group(item rater)

	mixed score || _all:R.person || _all:R.item || _all:R.rater || _all:R.pXi ///
		|| _all:R.pXr || _all:R.iXr, variance reml
	
	* This strange looking code grabs each estimated variance component 
	* See: http://www.ats.ucla.edu/stat/stata/faq/diparm.htm 
	_diparm lns1_1_1, f(exp(@)^2) d(2*exp(@)^2)
	local sig2p = r(est)

	_diparm lns1_2_1, f(exp(@)^2) d(2*exp(@)^2)
	local sig2i = r(est)

	_diparm lns1_3_1, f(exp(@)^2) d(2*exp(@)^2)
	local sig2r = r(est)
	
	_diparm lns1_4_1, f(exp(@)^2) d(2*exp(@)^2)
	local sig2pi = r(est)
	
	_diparm lns1_5_1, f(exp(@)^2) d(2*exp(@)^2)
	local sig2pr = r(est)
	
	_diparm lns1_6_1, f(exp(@)^2) d(2*exp(@)^2)
	local sig2ir = r(est)
	
	_diparm lnsig_e, f(exp(@)^2) d(2*exp(@)^2)
	local sig2e = r(est)
	
	di `sig2p'
	di `sig2i'
	di `sig2r'
	di `sig2pi'
	di `sig2pr'
	di `sig2ir'
	di `sig2e'	
	
*-----------------------------------------------------------------------------------
*
* Visualizing the marginal means with the egen command
*
*-----------------------------------------------------------------------------------	
	
	* First, what is the distribution of scores
	hist score, discrete percent name(scorhist, replace)
	egen allmean = mean(score)
	
	* For every person, calculate the average score over all items
	egen pmean = mean(score), by(person)
	
	* Pick one row from each person
	egen ptag = tag(person)
	
	* For every item, calculate the average score over all items
	egen imean = mean(score), by(item)
	
	* Pick one row from each item
	egen itag = tag(item)
	
	* For every rater, calculate the average score over all raters
	egen rmean = mean(score), by(rater)
	
	* Pick one row from each item
	egen rtag = tag(rater)
	
	* Plots a histogram of person mean scores
	hist pmean if ptag, discrete freq name(pmean, replace)
	
	* Plots a histogram of item percent correct
	hist imean if itag, discrete freq name(imean, replace)
	
	* Plots a histogram of rater average scores
	hist rmean if rtag, discrete freq name(rmean, replace)
	
*-----------------------------------------------------------------------------------
*
* Visualizing interactions with the egen command
*
*-----------------------------------------------------------------------------------	
	
	* Give each person-item combination its own dummy variable
	egen pXi = group(person item)  
	
	* Then take the average score for each person-item combination
	egen pXi_mean = mean(score), by(pXi)
	replace pXi_mean = pXi_mean - pmean - imean + allmean
	
	* Then show the variance of these person-by-item averages.
	egen pXitag = tag(pXi)
	hist pXi_mean if pXitag, discrete freq
	
	* Repeat for others.
	egen pXr = group(person rater)
	egen pXr_mean = mean(score), by(pXr)
	replace pXr_mean = pXr_mean - pmean - rmean + allmean
	egen pXrtag = tag(pXr)
	hist pXr_mean if pXrtag, discrete freq
	
	egen iXr = group(item rater)
	egen iXr_mean = mean(score), by(iXr)
	replace iXr_mean = iXr_mean - imean - rmean + allmean
	egen iXrtag = tag(iXr)
	hist iXr_mean if iXrtag, discrete freq

	
*--------------------------------------------------------------------------------
* Close the log.
*--------------------------------------------------------------------------------
    log close


