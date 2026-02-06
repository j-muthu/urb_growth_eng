/*-----------------------------------------------------------------------
hcgrowth_counterfact_morepermits.do

Stata do file to evaluate counterfactuals for
'Urban growth and its aggregate implications',
by Gilles Duranton and Diego Puga

Counterfactual: increase permits
-----------------------------------------------------------------------*/

* initialise
clear
set more off
set varabbrev off

tempfile tmp
tempfile tmp_base
tempfile tmp_table

set obs 9
gen parset = _n
gen sigma_beta = .
gen gamma_theta = .
gen pc_y = .
gen pc_c = .

save `tmp_table', replace

* loop through parameter sets
forvalues parset = 1/10 {
	clear
	* set parameters
	if `parset' == 1 {
		local sigma  = 0.02
		local beta   = 0.02
		local gamma  = 0.06
		local theta  = 0.03
	}
	else if `parset' == 2 {
		local sigma  = 0.03
		local beta   = 0.03
		local gamma  = 0.06
		local theta  = 0.03
	}
	else if `parset' == 3 {
		local sigma  = 0.04
		local beta   = 0.04
		local gamma  = 0.06
		local theta  = 0.03
	}
	else if `parset' == 4 {
		local sigma  = 0.02
		local beta   = 0.02
		local gamma  = 0.07
		local theta  = 0.04
	}
	else if `parset' == 5 {
		local sigma  = 0.03
		local beta   = 0.03
		local gamma  = 0.07
		local theta  = 0.04
	}
	else if `parset' == 6 {
		local sigma  = 0.04
		local beta   = 0.04
		local gamma  = 0.07
		local theta  = 0.04
	}
	else if `parset' == 7 {
		local sigma  = 0.02
		local beta   = 0.02
		local gamma  = 0.08
		local theta  = 0.05
	}
	else if `parset' == 8 {
		local sigma  = 0.03
		local beta   = 0.03
		local gamma  = 0.08
		local theta  = 0.05
	}
	else if `parset' == 9 {
		local sigma  = 0.04
		local beta   = 0.04
		local gamma  = 0.08
		local theta  = 0.05
	}
	* 9 sets for robustness table, 1 extra for main table 
	else {
		local sigma  = 0.04
		local beta   = 0.04
		local gamma  = 0.07
		local theta  = 0.04
	}
	* other parameters
	local lambda = 0.18
	local tau = 1
	local rho = 1
	* maximum feasible city size
	local pop_max = 40000000

	*** get data for relevant years
	set obs 1
	gen msa_name = "extra"
	save `tmp_base', replace
	foreach yy in 2010 {
		* previous generation is 30 years earlier
		local yy_pg = `yy' - 30
		* begin with actual us cities
		use ../../data/processed/citypop_msa_necma_wtotal, clear
		keep msa msa_name pop`yy' pop`yy_pg' pop2010
		* record total us pop by picking obs with us total
		sum pop`yy' if msa_name == "Conterminous United States"
		local us_total_pop = r(max)
		* record total msa pop
		gsort -pop`yy'
		gen long cumpop = sum(pop`yy') if !missing(msa)
		sum cumpop
		local us_msa_pop = r(max)
		drop cumpop
		* record non-msa total
		local us_rural_pop = `us_total_pop' - `us_msa_pop'
		* z from barriers
		merge 1:1 msa using ../../data/processed/controls_msa, keepusing(msa_cc30km_constrained msa_maincity)
		drop _merge
		gen z = 100/(100 - msa_cc30km_constrained)
		drop msa_cc30km_constrained
		* z for inframarginal cities (90th percentile)
		sum z, detail
		local z_p90 = r(p90)
		order msa msa_name msa_maincity pop`yy_pg' pop`yy' z
		keep msa msa_name msa_maincity pop`yy_pg' pop`yy' z
		drop if msa_name == "Conterminous United States"
		* use actual current year population
		gen long N_`yy'= pop`yy'
		* extra obs for rural and newcomers
		sum pop`yy'
		local pop_min = r(min)
		expand 2 if pop`yy' == `pop_min', gen(dup)
		replace msa_name = "Rural and newcomers" if dup == 1
		drop dup
		replace msa = . if msa_name == "Rural and newcomers"
		* compute c for incumbents (equilibrium relationship only)
		gen double c_`yy' = ( (`gamma' + `theta' - `sigma' - `beta') / ((`sigma' + `beta') * (`gamma' + 1)) ) * `tau' * z^(`gamma') * ( N_`yy'^(`gamma' + `theta') )
		replace z = . if msa_name == "Rural and newcomers"
		* rural and newcomer c is equal to c in marginal city
		sum c_`yy' if msa_name == "Rural and newcomers"
		local c_rural = r(max)
		* incumbent population in each city (min of old generation or current)
		gen long N_incumb_`yy' = min(pop`yy' , pop`yy_pg') if !missing(msa)
		* rural population (N_`yy' records rural only, N_incumb_`yy' also includes urban newcomers)
		replace N_`yy'= `us_rural_pop' if msa_name == "Rural and newcomers"
		gen long cumpop = sum(N_incumb_`yy')
		sum cumpop
		replace N_incumb_`yy' = `us_total_pop' - r(max) if msa_name == "Rural and newcomers"
		drop cumpop
		* compute A based on actual population and z
		gen double A_`yy' = ( (`gamma' + `theta') / ((`rho'^(`sigma')) * (`sigma' + `beta') * (`gamma' + 1)) ) * `tau' * z^(`gamma') * ( N_`yy'^(`gamma' + `theta' - `sigma' - `beta') ) if msa_name != "Rural and newcomers"
		* compute y
		gen double y_`yy' = (`rho'^`sigma') * A_`yy' * (N_`yy'^(`sigma' + `beta')) if msa_name != "Rural and newcomers"
		replace y_`yy' = c_`yy' if msa_name == "Rural and newcomers"
		* permitting costs
		gen double p_`yy' = c_`yy' - `c_rural' if msa_name != "Rural and newcomers"
		* save baseline to tmp file
		order msa msa_name msa_maincity z A_`yy' N_`yy' N_incumb_`yy' y_`yy' c_`yy' p_`yy'
		keep msa msa_name msa_maincity z A_`yy' N_`yy' N_incumb_`yy' y_`yy' c_`yy' p_`yy'
		merge 1:1 msa_name using `tmp_base'
		drop _merge
		replace msa_maincity = "Rural and newcomers" if msa_name == "Rural and newcomers"
		drop if msa_name == "extra"
		gsort -c_`yy' msa
		save `tmp_base', replace
	}

	*** baseline
	local yb = 2010
	foreach cfvar in N N_incumb {
		gen long `cfvar'_base = `cfvar'_`yb'
	}
	foreach cfvar in y c p {
		gen double `cfvar'_base = `cfvar'_`yb'
	}
	gen A = A_`yb'
	save `tmp_base', replace
	* record total msa pop
	gsort -N_base
	gen long cumpop = sum(N_base) if !missing(msa)
	sum cumpop
	local us_msa_pop = r(max)
	drop cumpop
	* recall rural pop
	sum N_base if msa_name == "Rural and newcomers", meanonly
	local us_rural_pop = r(mean)
	* recall total us pop
	local us_total_pop = `us_msa_pop' + `us_rural_pop'
	* display
	disp "Total: " %9.0f `us_total_pop'
	disp "Urban: " %9.0f `us_msa_pop'
	disp "Rural: " %9.0f `us_rural_pop'
	* compute rural A
	sum c_base if msa_name == "Rural and newcomers", meanonly
	local c_rural = r(mean)
	local A_rural = `c_rural' * (`us_rural_pop'^(`lambda'))
	disp "A_r = " `A_rural'

	*** define counterfactual
	* keep only msas (actual and counterfactual)
	drop if msa_name == "Rural and newcomers"
	preserve
	* calculate population with increased permitting
	* get permitting rate
	use ../../data/processed/housing_msa, clear
	keep msa msa_name permit_rate_1980_2010
	* merge population
	merge 1:1 msa using ../../data/processed/citypop_msa_necma, keepusing(pop1980 pop2010)
	drop _merge
	* merge wedge
	merge 1:1 msa using ../../results/hcgrowth_fig5_data, keepusing(msa_maincity wedge)
	* target cities with wedge > 200,000 and population > 3 million
	gen affected_cfact = 0
	replace affected_cfact = 1 if wedge >= 200000 & pop2010 > 3000000
	gsort -wedge
	list msa_maincity wedge pop2010 if affected_cfact == 1
	* set their permitting rate to at least top quartile
	sum permit_rate_1980_2010, detail
	local permit_rate_cfact = r(p75)
	disp `permit_rate_cfact'
	replace affected_cfact = 0 if permit_rate_1980_2010 >= `permit_rate_cfact'
	sum permit_rate_1980_2010 if affected_cfact == 1, detail
	* calculate counterfactual populations
	gen N = pop2010
	replace N = int((`permit_rate_cfact' / permit_rate_1980_2010) * (pop2010 - pop1980) + pop1980) if affected_cfact == 1
	format N %9.0f
	keep msa msa_name N affected_cfact permit_rate_1980_2010
	save `tmp', replace
	restore
	merge 1:1 msa using `tmp', keepusing(N affected_cfact permit_rate_1980_2010)
	drop _merge

	* drop cities one by one to find new marginal city
	gen long N_cfact = .
	gen long N_cfact_uncapped = .
	gen long N_marg = .
	gen double y_cfact = .
	gen double c_cfact = .
	gen long cityorder = .
	gen double c_rural_cut = .
	gen long cumpop = .
	local mc_last = 0
	count
	local cities = r(N)
	local mc = `cities'
	local cm = `c_rural'
	local cm_last = `c_rural' + 1
	local i = 1
	while ((`i' < `cities') & (`mc' > 1)) {
		quietly {
		* reset counterfactual population
		replace N_cfact = .
		* reset marginal city population
		replace N_marg = .
		* counterfactual population
		replace N_cfact = N_base if affected_cfact == 0
		replace N_cfact = N if affected_cfact == 1
		* counterfactual income and incumbent consumption
		replace y_cfact = (`rho'^`sigma') * A * (N_cfact^(`sigma' + `beta'))
		replace c_cfact = y_cfact - (1 / (`gamma' + 1)) * `tau' * z^(`gamma') * (N_cfact^(`gamma' + `theta'))
		* sort cities from highest to lowest incumbent consumption
		gsort -c_cfact
		replace cityorder = _n
		* cumulative urban population
		replace cumpop = sum(N_cfact)
		* urban population cannot exceed us total
		replace N_cfact = . if cumpop > `us_total_pop'
		* only cities above current marginal try are populated
		replace N_cfact = . if cityorder > `mc'
		* calculate rural consumption if each city were the marginal one
		replace c_rural_cut = `A_rural' * ( (`us_total_pop' - cumpop)^(-`lambda') )
		* update incumbent consumption in marginal city
		local cm_last = `cm'
		sum c_cfact if cityorder == `mc'
		local cm = r(mean)
		}
		disp "it `i', c: `cm'|`cm_last', cities: `mc'"
		* if marginal city implies urban population larger than us population, take out one more city
		local mc_last = `mc'
		if (missing(N_cfact[`mc'])) {
			local mc = `mc' - 1
		}
		* or, if marginal city implies rural consumption higher than marginal city incumbent consumption, take out one more city
		else {
			if (c_rural_cut[`mc'] > c_cfact[`mc']) {
				local mc = `mc' - 1
			}
			else {
				local mc = 0
			}
		}
		local i = `i' + 1
	}

	* set empty cities
	replace y_cfact = . if missing(N_cfact)
	replace c_cfact = . if missing(N_cfact)
	* extra obs for rural areas
	preserve
	use `tmp_base', clear
	keep if msa_name == "Rural and newcomers"
	save `tmp', replace
	restore
	append using `tmp'
	* rural consumption
	sum c_rural_cut if !missing(N_cfact)
	replace c_cfact = r(max) if msa_name == "Rural and newcomers"
	replace y_cfact = c_cfact if msa_name == "Rural and newcomers"
	* permitting costs
	gen p_cfact = c_cfact - r(max) if msa_name != "Rural and newcomers"
	* incumbent population in each city (min of previous generation or counterfactual)
	gen long N_incumb_cfact = min(N_cfact , N_incumb_base) if !missing(msa)
	* rural population (N_cfact records rural only, N_incumb_cfact also includes urban newcomers)
	replace cumpop = sum(N_cfact)
	sum cumpop
	replace N_cfact = `us_total_pop' - r(max) if msa_name == "Rural and newcomers"
	replace cumpop = sum(N_incumb_cfact)
	sum cumpop
	replace N_incumb_cfact = `us_total_pop' - r(max) if msa_name == "Rural and newcomers"
	drop cityorder N_cfact_uncapped N_marg c_rural_cut cumpop
	order msa msa_name msa_maincity N_base N_cfact N_incumb_base N_incumb_cfact c_base c_cfact y_base y_cfact p_base p_cfact

	* weighted national average y, baseline
	sum y_base [fweight=N_base], meanonly
	local y_base = r(mean)
	* weighted national average c (N_incumb_base also includes urban newcomers), baseline
	sum c_base [fweight=N_incumb_base], meanonly
	local c_base = r(mean)
	* weighted national average y, counterfactual
	sum y_cfact [fweight=N_cfact], meanonly
	local y_cfact = r(mean)
	* weighted national average c (N_incumb_cfact also includes urban newcomers), counterfactual
	sum c_cfact [fweight=N_incumb_cfact], meanonly
	local c_cfact = r(mean)
	* percentage changes for individual cities
	gen pc_y = 100 * (y_cfact - y_base) / y_base
	gen pc_c = 100 * (c_cfact - c_base) / c_base
	gen pc_permits = 100 * (`permit_rate_cfact' - permit_rate_1980_2010) / permit_rate_1980_2010
	* change in c for newcomers
	sum pc_c if msa_name == "Rural and newcomers"
	gen pc_c_nc = r(mean)
	* save for use by hcgrowth_text_results.do
	gsort -affected_cfact -c_base
	save ../../data/intermediate/cfact_morepermits, replace

	* format variables
	gen N_base_th = N_base / 1000
	gen N_cfact_th = N_cfact / 1000
	format %9.1f pc_permits
	format %12.0fc N_base N_cfact N_base_th N_cfact_th
	format %9.2f pc_y pc_c_nc
	format %9.3f pc_c
	* sort from highest to lowest counterfactual populations
	gen ind_rural = 0
	replace ind_rural = 1 if msa_name == "Rural and newcomers"
	gsort ind_rural -N_cfact
	
	* save results to robustness table
	if `parset' <= 9 {
		use `tmp_table', clear
		replace pc_y = 100* ((`y_cfact' - `y_base') / `y_base') if parset == `parset'
		replace pc_c = 100* ((`c_cfact' - `c_base') / `c_base') if parset == `parset'
		replace sigma_beta = `sigma' + `beta' if parset == `parset'
		replace gamma_theta = `gamma' + `theta' if parset == `parset'
		save `tmp_table', replace
	}
	
* end of loop through parameter sets
}

* produce latex table 3
gen str1 ampersand = "&"
gen str2 percent = "\%"
gen str4 slash = "\\"
split msa_maincity, parse(,)
gen state = substr(msa_maincity,-2,2)
replace msa_maincity = msa_maincity1 + ", \textsc{" + lower(state) + "}"
replace pc_c = . if msa_name == "Rural and newcomers"
replace pc_c_nc = .  if msa_name == "Rural and newcomers"
replace msa_maincity = "Rural areas"  if msa_name == "Rural and newcomers"
outsheet msa_maincity ampersand pc_permits percent ampersand N_base_th ampersand N_cfact_th ampersand pc_y percent ampersand pc_c percent ampersand pc_c_nc percent slash if affected_cfact == 1 | msa_name == "Rural and newcomers" using ../../results/hcgrowth_table3.tex, nonames noquote replace
shell sed 's/\t*\\%/\\%/g' "../../results/hcgrowth_table3.tex" > "../../results/hcgrowth_table3_.tex"
shell sed -re 's/([0-9])([0-9]{3})($|[^0-9])/\1,\2\3/g' "../../results/hcgrowth_table3_.tex" > "../../results/hcgrowth_table3.tex"
shell sed 's/\&\\%/\&/g' "../../results/hcgrowth_table3.tex" > "../../results/hcgrowth_table3_.tex"
copy "../../results/hcgrowth_table3_.tex" "../../results/hcgrowth_table3.tex", replace
erase "../../results/hcgrowth_table3_.tex"

* produce latex table c2
use `tmp_table', clear
forvalues parset = 1/9 {
	local sigma_beta_`parset' : di %4.2f sigma_beta[`parset']
	local gamma_theta_`parset' : di %4.2f gamma_theta[`parset']
	local c`parset' : di %4.2f pc_c[`parset']
	local y`parset' : di %4.2f pc_y[`parset']
}

file open texfile using ../../results/hcgrowth_tablec2.tex, write replace
file write texfile "& & & \multicolumn{3}{c}{$\sigma+\beta$}\\" _n
file write texfile "\cmidrule(r){4-6}" _n
file write texfile "& & & `sigma_beta_1' & `sigma_beta_2' & `sigma_beta_3'\\" _n
file write texfile "\toprule" _n
file write texfile "& `gamma_theta_1' & consumption & `c1'\% & `c2'\% & `c3'\%\\" _n
file write texfile "& & output & `y1'\% & `y2'\% & `y3'\%\\" _n
file write texfile "\cmidrule(r){2-6}" _n
file write texfile "$\gamma+\theta$ & `gamma_theta_4' & consumption & `c4'\% & `c5'\% & `c6'\%\\" _n
file write texfile "& & output & `y4'\% & `y5'\% & `y6'\%\\" _n
file write texfile "\cmidrule(r){2-6}" _n
file write texfile "& `gamma_theta_7' & consumption & `c7'\% & `c8'\% & `c9'\%\\" _n
file write texfile "& & output & `y7'\% & `y8'\% & `y9'\%\\" _n
file close texfile


