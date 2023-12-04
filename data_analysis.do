***
use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\dataset_71_Nov2021.dta", clear

encode method, generate(metho)
encode farmid, generate(farm)
encode province, generate(provinces)
encode housing_name, generate(housing)


correlate farmid dtoa province
pwcorr pielou_evenness shannon_entropy faith_pd observed_features
******************************************************************************
**ALPHA DIVERSITY 

*Shannon 

histogram shannon_entropy
graph box shannon_entropy, over(method)

anova shannon_entropy method farmid i.housing
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

/* transform  for normality */ 
xi: boxcox shannon_entropy i.method i.farmid 


graph box shannon_entropy, over(method)
mixed shannon_entropy i.method|| farmid:

** Bonferroni's Pairwise comparisons  
pwcompare method , pv mcomp(bon) 

 
margins method, asbalanced
marginsplot
*------------------------------------------------------------------------------
*Evenness

histogram pielou_evenness
graph box pielou_evenness, over(method)
mixed pielou_evenness i.method|| farmid:
** Bonferroni's Pairwise comparisons  
pwcompare method , pv mcomp(bon)




anova p2 method farmid
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

/* transform  for normality */ 

xi: boxcox pielou_evenness i.method i.farmid 

gen evenness_bc = pielou_evenness ^   13.45

rename evenness_bc P_evenness

anova P_evenness method farmid
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res

graph box P_evenness, over(method)
mixed P_evenness i.method|| farmid:

** Bonferroni's Pairwise comparisons  
pwcompare method , pv mcomp(bon)


margins method
marginsplot
**Back trasform margins
*0
di(0.4758756  ^(1/13.45)) 
=.95
*3
di(.3456883 ^(1/13.45))
=.92
*7
di( .241375^(1/13.45))
=.90
*E
di(.383879^(1/13.45))
=.93

**Back trasform margins contrast
*0 vs 3
di(0.1301873  ^(1/13.45)) 

*0 vs 7
di(-.2345006 ^(1/13.45))
=.92
*E vs 7
di( .142504 ^(1/13.45))

**------------------------------------------------------------------------------

*faith_pd


graph box faith_pd, over(method)
tab faith_pd method

anova faith_pd method farmid
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

/* transform  for normality */ 

xi: boxcox faith_pd i.method i.farmid 

/* generate normalized faith_pd */

gen faith_pd_bc = faith_pd ^ 0.5
qnorm faith_pd_bc
hist faith_pd_bc

/* evaluate normality of residuals in faith_pd_bc vs days_4c and herd id */

anova faith_pd_bc method farmid
predict res, rstandard
qnorm res
hist res
rvfplot
drop res

mixed faith_pd_bc i.method|| farmid:


graph box faith_pd_bc, over(method)

** Bonferroni's Pairwise comparisons  
pwcompare method , pv mcomp(bon)


margins method, asbalanced
marginsplot

**Back trasform margins
*0
di 6.324489^(1/0.5)
=40
*3
di 6.224584 ^(1/0.5)
=38.7
*7
di 6.244396^(1/0.5)
=39
*E
di   5.792532^(1/0.5)
=33.5

di  1.02^(1/0.62)
*---------------------------------------------------------------------------

*Observed Features 

graph box observed_features, over(method)

anova observed_features method farmid
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

* transform  for normality 
xi: boxcox observed_features i.method i.farmid 


* evaluate normality of residuals 
gen lnof = ln(observed_features)
anova lnof method farmid
predict res, rstandard
qnorm res
hist res
rvfplot
drop res


graph box lnof, over(method)
mixed lnof i.method || farmid:


** Bonferroni's Pairwise comparisons  
pwcompare method , pv mcomp(bon) 

margins method, asbalanced
marginsplot

**Back trasform margins
*0
di exp(6.263483)
=525
*3
di exp( 6.22427 )
=504.9
*7
di exp( 6.157578  )
=472.3
*E
di  exp( 5.908699 )
=368.2

di  exp( -0.315 )
=368.2
*******************************************************************************

**BETA DIVERSITY 

**BRAY-CURTIS

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\1.braycurtismatrixfull.dta", clear

foreach v of varlist sample100-sample99 {
	replace `v'=. if `v'==0
}

*Mean distances per method 
gen meandist= -1
capture drop temp


egen temp=rmean ( sample100 sample104 sample108 sample111 sample114 sample118 sample122 sample126 sample46 sample50 sample54 sample58 sample70 sample86) if method == "E"
replace meandist = temp if method == "E"

capture drop temp
egen temp=rmean ( sample101 sample105 sample109 sample112 sample115 sample119 sample123 sample127 sample47 sample51 sample55 sample59 sample63 sample71 sample75 sample79 sample83 sample87 sample91 sample95 sample97) if method == "0"
replace meandist = temp if method == "0"

capture drop temp
egen temp=rmean (sample102 sample106 sample110 sample116 sample120 sample124 sample128 sample48 sample52 sample56 sample60 sample68 sample72 sample76 sample80 sample84 sample92 sample96 sample98) if method == "3"
replace meandist = temp if method == "3"

capture drop temp
egen temp=rmean (sample103 sample107 sample113 sample125 sample129 sample49 sample53 sample57 sample61 sample65 sample69 sample73 sample77 sample81 sample85 sample89 sample99) if method == "7"
replace meandist = temp if method == "7"

drop temp
save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\1.braycurtismatrixfull.dta", replace

***MIXED MODEL meandist by method 
use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\1.braycurtismatrixfull.dta", clear

*Checking normality of residuals 
anova meandist metho farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

*mixed model 
mixed meandist i.metho|| farm:

** Bonferroni's Pairwise comparisons  
pwcompare metho , pv mcomp(bon) 

graph box meandist, over(metho)
 
margins metho, asbalanced
marginsplot

save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\1.braycurtismatrixfull.dta", replace

***-------------------------------------------------------------------------

*Master dataset with id for second component of the pairs by farmid 
        **same master dataset for all the indices 

		use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\1.braycurtismatrixfull.dta" , clear
order farmid v1 method
rename v1 sample_1
gen sample_22 = sample_1
rename method method_1
gen method_2 = method_1
gen farmid_2 = farmid

*keep farmid sample_1 method_1 farmid_2 method_2
gen sample_2 = substr(sample_22, 8,3)
keep farmid sample_2 method_2
destring sample_2, replace

save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\farm_method_master.dta", replace

*------------------------------------------------------------------------------

*Reshape dataset to organize second component of the pairs//// Bray-Curtis

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\1.braycurtismatrixfull.dta" , clear

rename v1 sample_1
rename method method_1
order farmid sample_1 method_1

reshape long sample, i(sample_1) j(sample_2)
rename sample dist

gen sample_11=substr(sample_1, 8,3)
destring sample_11, replace

*create boundaries to select samples that belong to a given farm
egen sample_11_min=min(sample_11), by(farmid)
egen sample_11_max=max(sample_11), by(farmid)
bysort farmid:keep if sample_2>=sample_11_min & sample_2<=sample_11_max

*drop records with no distance (eg same sample)
drop if dist==.

*remove duplicates (eg the reverse order of the pair)
sort farmid dist sample_11 sample_2
by farmid dist : keep if _n==1

*bring master data to crate variable with the pairs
mmerge farmid sample_2 using "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\farm_method_master.dta"
drop _merge
drop if sample_1==""
egen str10 methd_pair=concat( method_1 method_2), p(-)

drop sample_11 sample_11_min sample_11_max


save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\1.farm_pair_dist_bray.dta", replace

***MIXED MODEL meandist by Farmid pairs

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\1.farm_pair_dist_bray.dta", clear

encode methd_pair, generate(methd_pair1)
encode farmid, generate(farm)

*Checking normality of residuals 
anova dist i.methd_pair1 farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

*mixed model 
graph box dist, over(methd_pair1)

mixed dist i.methd_pair1 || farmid:

pwcompare methd_pair1 , pv mcomp(bon) groups
 
margins i.methd_pair1, asbalanced
marginsplot

*matrix imput from margins table
matrix input m = (0,.66,.77,.75\.66,0,.66,.76\.77,.66,0,.84\.75,.76,.84,0)
matrix list m

**classical multidimensional scaling
mdsmat m, method(classical) names(0 3 7 E) config


**Modern multidimensional scaling
mdsmat m, method(modern) names(0 3 7 E) config


**-----------------------------------------------------------------------------
*-------------------------------------------------------------------------------

**W Index

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\2.wmatrixfull.dta", clear

foreach v of varlist sample100-sample99 {
	replace `v'=. if `v'==0
}

*Mean distances per method 
gen meandist= -1
capture drop temp


egen temp=rmean ( sample100 sample104 sample108 sample111 sample114 sample118 sample122 sample126 sample46 sample50 sample54 sample58 sample70 sample86) if method == "E"
replace meandist = temp if method == "E"

capture drop temp
egen temp=rmean ( sample101 sample105 sample109 sample112 sample115 sample119 sample123 sample127 sample47 sample51 sample55 sample59 sample63 sample71 sample75 sample79 sample83 sample87 sample91 sample95 sample97) if method == "0"
replace meandist = temp if method == "0"

capture drop temp
egen temp=rmean (sample102 sample106 sample110 sample116 sample120 sample124 sample128 sample48 sample52 sample56 sample60 sample68 sample72 sample76 sample80 sample84 sample92 sample96 sample98) if method == "3"
replace meandist = temp if method == "3"

capture drop temp
egen temp=rmean (sample103 sample107 sample113 sample125 sample129 sample49 sample53 sample57 sample61 sample65 sample69 sample73 sample77 sample81 sample85 sample89 sample99) if method == "7"
replace meandist = temp if method == "7"

drop temp

***MIXED MODEL meandist by method 

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\2.wmatrixfull.dta", clear

*Checking normality of residuals 
anova meandist metho farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

*mixed model 
mixed meandist i.metho|| farm:

** Bonferroni's Pairwise comparisons  
pwcompare metho , pv mcomp(bon) 

graph box meandist, over(metho)
 
margins metho, asbalanced
marginsplot

save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\2.wmatrixfull.dta", replace

*reshape dataset to organize second component of the pairs//// 
use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\2.wmatrixfull.dta" , clear
rename v1 sample_1
rename method method_1
order farmid sample_1 method_1

reshape long sample, i(sample_1) j(sample_2)
rename sample dist

gen sample_11=substr(sample_1, 8,3)
destring sample_11, replace

*create boundaries to select samples that belong to a given farm
egen sample_11_min=min(sample_11), by(farmid)
egen sample_11_max=max(sample_11), by(farmid)
bysort farmid:keep if sample_2>=sample_11_min & sample_2<=sample_11_max

*drop records with no distance (eg same sample)
drop if dist==.

*remove duplicates (eg the reverse order of the pair)
sort farmid dist sample_11 sample_2
by farmid dist : keep if _n==1

*bring master data to crate variable with the pairs
mmerge farmid sample_2 using "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\farm_method_master.dta"
drop _merge
drop if sample_1==""
egen str10 methd_pair=concat( method_1 method_2), p(-)

drop sample_11 sample_11_min sample_11_max

save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\2.wmatrix_farm_pair_dist.dta", replace

***MIXED MODEL meandist by Farmid pairs /// W index

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\2.wmatrix_farm_pair_dist.dta", clear

encode methd_pair, generate(methd_pair1)
encode farmid, generate(farm)

*Checking normality of residuals 
anova dist i.methd_pair1 farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

* transform  for normality 
xi: boxcox dist i.methd_pair1 i.farm 

* evaluate normality of residuals 
gen distbc = dist ^  .3659418
anova distbc i.methd_pair1 farm 
predict res, rstandard
qnorm res
hist res
rvfplot
drop res

*mixed model 
graph box distbc, over(methd_pair1)

mixed distbc i.methd_pair1 || farmid:

pwcompare methd_pair1 , pv mcomp(bon) groups
 
margins i.methd_pair1, asbalanced
marginsplot

*matrix imput from margins table
matrix input m = (0,.84,.88,.90\.84,0,.86,.89\.88,.86,0,.92\.90,.89,.92,0)
matrix list m

**classical multidimensional scaling
mdsmat m, method(classical) names(0 3 7 E) config


**Modern multidimensional scaling
mdsmat m, method(modern) names(0 3 7 E) config

save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\2.wmatrix_farm_pair_dist.dta", replace
*----------------------------------------------------------------------------
*-------------------------------------------------------------------------------
** mn Index

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\3.mnmatrixfull.dta", clear

foreach v of varlist sample100-sample99 {
	replace `v'=. if `v'==0
}

*Mean distances per method 
gen meandist= -1
capture drop temp


egen temp=rmean ( sample100 sample104 sample108 sample111 sample114 sample118 sample122 sample126 sample46 sample50 sample54 sample58 sample70 sample86) if method == "E"
replace meandist = temp if method == "E"

capture drop temp
egen temp=rmean ( sample101 sample105 sample109 sample112 sample115 sample119 sample123 sample127 sample47 sample51 sample55 sample59 sample63 sample71 sample75 sample79 sample83 sample87 sample91 sample95 sample97) if method == "0"
replace meandist = temp if method == "0"

capture drop temp
egen temp=rmean (sample102 sample106 sample110 sample116 sample120 sample124 sample128 sample48 sample52 sample56 sample60 sample68 sample72 sample76 sample80 sample84 sample92 sample96 sample98) if method == "3"
replace meandist = temp if method == "3"

capture drop temp
egen temp=rmean (sample103 sample107 sample113 sample125 sample129 sample49 sample53 sample57 sample61 sample65 sample69 sample73 sample77 sample81 sample85 sample89 sample99) if method == "7"
replace meandist = temp if method == "7"

drop temp

save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\3.mnmatrixfull.dta", replace
***MIXED MODEL meandist by method 

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\3.mnmatrixfull.dta", clear

*Checking normality of residuals 
anova meandist metho farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

* transform  for normality 
xi: boxcox meandist i.metho i.farm 

gen distmn2= 1-meandist

* transform  for normality 
xi: boxcox distmn2 i.metho i.farm  

* evaluate normality of residuals 
gen meandistbc = distmn2 ^ 1.471954 
anova meandistbc metho farm 
predict res, rstandard
qnorm res
hist res
rvfplot
drop res

*mixed model 
mixed meandistbc i.metho|| farm:

** Bonferroni's Pairwise comparisons  
pwcompare metho , pv mcomp(bon) 

graph box meandistbc, over(metho)
graph box meandist, over (metho) 
margins metho, asbalanced
marginsplot

save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\3.mnmatrixfull.dta", replace


*reshape dataset to organize second component of the pairs
use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\3.mnmatrixfull.dta" , clear
rename v1 sample_1
rename method method_1
order farmid sample_1 method_1

reshape long sample, i(sample_1) j(sample_2)
rename sample dist

gen sample_11=substr(sample_1, 8,3)
destring sample_11, replace

*create boundaries to select samples that belong to a given farm
egen sample_11_min=min(sample_11), by(farmid)
egen sample_11_max=max(sample_11), by(farmid)
bysort farmid:keep if sample_2>=sample_11_min & sample_2<=sample_11_max

*drop records with no distance (eg same sample)
drop if dist==.
drop if dist==0

*remove duplicates (eg the reverse order of the pair)
sort farmid dist sample_11 sample_2
by farmid dist : keep if _n==1

*bring master data to crate variable with the pairs
mmerge farmid sample_2 using "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\farm_method_master.dta"
drop _merge
drop if sample_1==""
egen str10 methd_pair=concat( method_1 method_2), p(-)

drop sample_11 sample_11_min sample_11_max


save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\3.mnmatrixfull_farm_pair_dist.dta", replace

***MIXED MODEL meandist by Farmid pairs /// mn index

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\3.mnmatrixfull_farm_pair_dist.dta", clear

encode methd_pair, generate(methd_pair1)
encode farmid, generate(farm)

*Checking normality of residuals 
gen distmn2= 1-dist
anova distmn2 i.methd_pair1 farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

* transform  for normality 
xi: boxcox distmn2 i.methd_pair1 i.farm 


* evaluate normality of residuals 
gen distbc = distmn2 ^  .8315511 
anova distbc i.methd_pair1 farm 
predict res, rstandard
qnorm res
hist res
rvfplot
drop res

*mixed model 
graph box distbc, over(methd_pair1)

mixed distbc i.methd_pair1 || farmid:

pwcompare methd_pair1 , pv mcomp(bon) groups
 
margins i.methd_pair1, asbalanced
marginsplot

**Back trasform margins
*0-3
di 1-( 0.0404341^(1/0.8315511)) 
.97888875
*0-7
di 1-(0.0413216^(1/0.8315511))
.97833027
*3-7
di 1- (0.0448233^(1/0.8315511))
.97610339
*E-0
di 1-(0.0262251^(1/0.8315511))
.98745722
*E-3
di 1-(0.026388 ^(1/0.8315511))
.98736347
*E-7
di 1-(0.0250477  ^(1/0.8315511))
.9881313


*matrix imput from margins table
matrix input m = (0,.97888875,.97833027,.98745722\.97888875,0,.97610339,.98736347\.97833027,.97610339,0,.9881313\.98745722,.98736347,.9881313,0)
matrix list m

**classical multidimensional scaling
mdsmat m, method(classical) names(0 3 7 E) config


**Modern multidimensional scaling
mdsmat m, method(modern) names(0 3 7 E) config
*-------------------------------------------------------------------------------

*-2 Index

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\4.B2matrixfull.dta", clear

foreach v of varlist sample100-sample99 {
	replace `v'=. if `v'==0
}

*Mean distances per method 
gen meandist= -1
capture drop temp


egen temp=rmean ( sample100 sample104 sample108 sample111 sample114 sample118 sample122 sample126 sample46 sample50 sample54 sample58 sample70 sample86) if method == "E"
replace meandist = temp if method == "E"

capture drop temp
egen temp=rmean ( sample101 sample105 sample109 sample112 sample115 sample119 sample123 sample127 sample47 sample51 sample55 sample59 sample63 sample71 sample75 sample79 sample83 sample87 sample91 sample95 sample97) if method == "0"
replace meandist = temp if method == "0"

capture drop temp
egen temp=rmean (sample102 sample106 sample110 sample116 sample120 sample124 sample128 sample48 sample52 sample56 sample60 sample68 sample72 sample76 sample80 sample84 sample92 sample96 sample98) if method == "3"
replace meandist = temp if method == "3"

capture drop temp
egen temp=rmean (sample103 sample107 sample113 sample125 sample129 sample49 sample53 sample57 sample61 sample65 sample69 sample73 sample77 sample81 sample85 sample89 sample99) if method == "7"
replace meandist = temp if method == "7"

drop temp

***MIXED MODEL meandist by method 
use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\4.B2matrixfull.dta", clear

*Checking normality of residuals 
encode method, generate(metho)
encode farmid, generate(farm)

anova meandist metho farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

* transform  for normality 
xi: boxcox meandist i.metho i.farm 
gen meandistbc = meandist^3.757089 

anova meandistbc metho farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

*mixed model 
mixed meandistbc i.metho|| farm:

** Bonferroni's Pairwise comparisons  
pwcompare metho , pv mcomp(bon) 

graph box meandistbc, over(metho)
 
margins metho, asbalanced
marginsplot

save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\4.B2matrixfull.dta", replace

*reshape dataset to organize second component of the pairs 
use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\4.B2matrixfull.dta" , clear
rename v1 sample_1
rename method method_1
order farmid sample_1 method_1

reshape long sample, i(sample_1) j(sample_2)
rename sample dist

gen sample_11=substr(sample_1, 8,3)
destring sample_11, replace

*create boundaries to select samples that belong to a given farm
egen sample_11_min=min(sample_11), by(farmid)
egen sample_11_max=max(sample_11), by(farmid)
bysort farmid:keep if sample_2>=sample_11_min & sample_2<=sample_11_max

*drop records with no distance (eg same sample)
drop if dist==.
drop if dist==0

*remove duplicates (eg the reverse order of the pair)
sort farmid dist sample_11 sample_2
by farmid dist : keep if _n==1

*bring master data to crate variable with the pairs
mmerge farmid sample_2 using "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\farm_method_master.dta"
drop _merge
drop if sample_1==""
egen str10 methd_pair=concat( method_1 method_2), p(-)

drop sample_11 sample_11_min sample_11_max


save "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\4.B2matrixfull_farm_pair_dist.dta", replace

***MIXED MODEL meandist by Farmid pairs /// -2 index

use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\4.B2matrixfull_farm_pair_dist.dta", clear

encode methd_pair, generate(methd_pair1)
encode farmid, generate(farm)

*Checking normality of residuals 
anova dist i.methd_pair1 farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

* transform  for normality 
xi: boxcox dist i.methd_pair1 i.farm 


* evaluate normality of residuals 
gen distbc = dist^ .8604889
anova distbc i.methd_pair1 farm 
predict res, rstandard
qnorm res
hist res
rvfplot
drop res

*mixed model 
graph box distbc, over(methd_pair1)

mixed distbc i.methd_pair1 || farmid:


 
margins i.methd_pair1, asbalanced
marginsplot


**Back trasform margins
*0-3
di 1-( 0.52^(1/0.8604889)) 
.532
*0-7
di 1-(0.54^(1/0.8604889))
.511
*3-7
di 1- (0.53^(1/0.8604889))
.521
*E-0
di 1-(0.49^(1/0.8604889))
.563
*E-3
di 1-(0.50^(1/0.8315511))
.565
*E-7
di 1-(0.56^(1/0.8315511))
.502

*matrix imput from margins table
matrix input m = (0,.532,.511,.563\.532,0,.521,.565\.511,.521,0,.502\.563,.565,.502,0)
matrix list m

**classical multidimensional scaling
mdsmat m, method(classical) names(0 3 7 E) config


**Modern multidimensional scaling
mdsmat m, method(modern) names(0 3 7 E) config





*-----------------------------------------------------------------------------
 use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\beta_W.dta", clear 
encode method, generate (methd)

*Checking normality of residuals
anova diversity methd
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

/* transform  for normality */ 
xi: boxcox diversity i.methd

gen diver_bc = diversity ^  .2542507 
anova diver_bc methd
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res

*mixed model 
graph box diversity, over(methd)

mixed diversity i.methd

pwcompare methd , pv mcomp(bon) groups
 
margins i.methd, asbalanced
marginsplot












*ABUNDANCE PHYLA COMPARISON
**----------------------------------------------------------------------
use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\taxonomy_phylum_sum.dta", clear 

**Bar Graphs
*phylum
tab phylum method, summ(abundance) nostandard nofreq 
graph bar abund if abundance,  over(phylum) over(method) stack asyvars
graph bar (sum) abund if abundance,  over(phylum) over(method) stack asyvars

graph box Actinobacteria, over(method)
graph box Firmicutes, over(method)
graph box Proteobacteria, over(method)
graph box method, over(Proteobacteria)

graph bar abundance,  over(phylum) over (herdid) stack asyvars


use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\taxonomy_genera.dta", clear 
use "C:\Users\asjaramillo\Dropbox\UPEI\TESIS UPEI\1. Validation Study\71 samples\storage_core-metrics-results\taxonomy_genera_sum.dta", clear 

*Genera
tab genus method, summ(abundance) nostandard nofreq 
graph bar abundance,  over(genus) over(method) stack asyvars
graph bar (sum) abundance,  over(genus) over(method) stack asyvars
graph bar abundance,  over(genus) over (farmid) stack asyvars
graph bar abundance,  over(genus) over (herdid) over(type)   stack asyvars

*Reshape from long to wide 

replace phylum = "WPS" in 72
replace phylum = "WPS" in 54
replace phylum = "WPS" in 36
replace phylum = "WPS" in 18

reshape wide abundance, i(v1) j(phylum) string 

rename abundanceAD3 AD3
rename abundanceAcidobacteria Acidobacteria
rename abundanceActinobacteria Actinobacteria
rename abundanceBacteroidetes Bacteroidetes
rename abundanceChloroflexi Chloroflexi
rename abundanceCyanobacteria Cyanobacteria
rename abundanceElusimicrobia Elusimicrobia
rename abundanceFibrobacteres Fibrobacteres
rename abundanceFirmicutes Firmicutes
rename abundanceFusobacteria Fusobacteria
rename abundanceLentisphaerae Lentisphaerae
rename abundanceOD1 OD1
rename abundancePlanctomycetes Planctomycetes
rename abundanceProteobacteria Proteobacteria
rename abundanceSpirochaetes Spirochaetes
rename abundanceTenericutes Tenericutes
rename abundanceVerrucomicrobia Verrucomicrobia
rename abundanceWPS WPS
rename abundanceOther Other 
 
 
encode method, generate(metho)
encode farmid, generate(farm) 
**BACTEROIDETES

* evaluate normality of residuals
anova Bacteroidetes metho farm 
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

* transform bacteroidetes for normality

xi: boxcox Bacteroidetes i.meth i.farmid

mixed Bacteroidetes i.metho|| farm:
graph box Bacteroidetes , over(metho)

pwcompare metho , pv mcomp(bon) groups
 
margins i.metho, asbalanced
marginsplot

*0vs3
di 52.2-46.4
*0vs7
di 52.2-39
*0vsE
di 52.2-35.5
*3vs7
di 46.4-39
*3vsE
di 46.4-35.5
*7vsE
di 39-35.5
**-------------------------------------------------------------------------

**FIRMICUTES
* evaluate normality of residuals 
anova Firmicutes metho farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

* transform firmicutes for normality  

xi: boxcox Firmicutes i.meth i.farmid 
 
* generate normalized Firmicutes 

gen firmicutes_bc = Firmicutes ^ 0.6

* Revaluate normality of residuals 

anova firmicutes_bc meth farmid
predict res, rstandard
qnorm res
hist res
rvfplot
drop res

mixed firmicutes_bc i.meth|| farmid:
mixed Firmicutes i.metho|| farm:
graph box firmicutes_bc , over(meth)
graph box Firmicutes,over(metho)


pwcompare metho , pv mcomp(bon) groups
 
margins i.metho, asbalanced
marginsplot

*0vs3
di 40.2-31.2
*0vs7
di 40.2-38.9
*0vsE
di 40.2-53.3
*3vs7
di 31.2-38.9
*3vsE
di 31.2-53.3
*7vsE
di 38.9-53.3
*----------------------------------------------------------------------------
**PROTEOBACTERIA
/* evaluate normality of residuals in proteobacteria vs days_4c and herd id */
anova Proteobacteria metho farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 

**trasformation

gen proteobacteria_1 = Proteobacteria + 1

/* transform firmicutes for normality */ 
xi: boxcox proteobacteria_1 i.metho i.farm  
 
  
 
/* generate normalized bacteroidetes */
gen lnproteobacteria = ln(proteobacteria_1) 
gen proteobacteria_bc = proteobacteria_1 ^ -0.44
drop proteobacteria_bc

/* evaluate normality of residuals in bacteroidetes vs days_4c and herd id */
anova proteobacteria_bc metho farm
predict res, rstandard
qnorm res 
hist res
rvfplot
drop res 



mixed proteobacteria_bc i.metho|| farm:
graph box lnproteobacteria , over(method)
graph box Proteobacteria,over(metho)
pwcompare metho , pv mcomp(bon) 

margins i.metho, asbalanced
marginsplot



*0
di ( 0.80^(1/-0.44))+1 
=2.7
*3
di (0.43^(1/-0.44))+1
=7.80
*7
di (0.39^(1/-0.44))+1
=9.5
*E
di (0.54^(1/-0.44))+1
=5



*0vs3
di 40.2-31.2
*0vs7
di 40.2-38.9
*0vsE
di 40.2-53.3
*3vs7
di 31.2-38.9
*3vsE
di 31.2-53.3
*7vsE
di 38.9-53.3
**---------------------------------------------------------------------------
