///Code Thesis///
//MRM//
* Step 1: importing dataset
clear
import delimited "~/Desktop/Riccardo/Paper/DATA/DiD/Model 1 originale.csv", clear
browse

////////////////////////////////////////////////////////////////////////////////
* Step 2: checking the dataset
drop in 2
drop if v1 == "Model_1_numeric"
rename v1 country
rename v2 year
rename v3 gdp_growth
rename v4 gov_expense
rename v5 inflation
rename v6 trade_openness
rename v7 unemployment
rename v8 gdp
rename v9 export_to_us
rename v10 export_to_us_share
rename v11 import_from_us
rename v12 import_from_us_share
destring year , replace
destring gdp_growth , replace
destring gov_expense , replace
destring inflation , replace
destring trade_openness , replace
destring unemployment , replace
destring gdp , replace
destring unemployment , replace
destring export_to_us , replace
destring export_to_us_share , replace
destring import_from_us , replace
destring import_from_us_share , replace
local pctvars gdp_growth gov_expense inflation trade_openness ///
             unemployment export_to_us_share import_from_us_share
quietly foreach v of varlist `pctvars' {
    replace `v' = `v'/100   if !missing(`v')
}
rename export_to_us_share export_to_us_pct
rename import_from_us_share import_from_us_pct

////////////////////////////////////////////////////////////////////////////////
* Step 3: panel structure
egen country_id = group(country) //Creating numeric variables
label variable country_id "Numeric country panel ID"
//drop if country_id == 9

////////////////////////////////////////////////////////////////////////////////
* Step 4: dummies creation
gen covid = 0
replace covid = 1 if year == 2020 | year == 2021
gen crisis2008 = 0
replace crisis2008 = 1 if year == 2008 | year == 2009

collapse (mean) share_us_gdp = export_to_us_pct, by(country_id)
save avg_share_export.dta, replace
collapse (mean) share_us_gdp = import_from_us_pct, by(country_id)
save avg_share_retaliation.dta, replace
restore

////////////////////////////////////////////////////////////////////////////////
* Step 5: assumptions
local vars gdp_growth inflation unemployment gov_expense trade_openness export_to_us_pct import_from_us_pct

* Linearità - GDP Growth vs X
foreach var in inflation unemployment gov_expense trade_openness export_to_us_pct import_from_us_pct {
    scatter gdp_growth `var', msymbol(circle) || lfit gdp_growth `var', lcolor(red) ///
        title("Linearity: GDP Growth vs `var'") legend(label(1 "gdp_growth") label(2 "Fitted values")) ///
        graphregion(color(white)) ///
        name(gdp_growth_`var', replace)
    graph export linearity_gdp_growth_`var'.png, replace
}

* Linearità - Inflation vs X
foreach var in unemployment gov_expense trade_openness export_to_us_pct import_from_us_pct {
    scatter inflation `var', msymbol(circle) || lfit inflation `var', lcolor(red) ///
        title("Linearity: Inflation vs `var'") legend(label(1 "inflation") label(2 "Fitted values")) ///
        graphregion(color(white)) ///
        name(inflation_`var', replace)
	 graph export linearity_`var'.png, replace
}

* Linearità - Unemployment vs X
foreach var in gov_expense trade_openness export_to_us_pct import_from_us_pct {
    scatter unemployment `var', msymbol(circle) || lfit unemployment `var', lcolor(red) ///
        title("Linearity: Unemployment vs `var'") legend(label(1 "unemployment") label(2 "Fitted values")) ///
        graphregion(color(white)) ///
        name(unemployment_`var', replace)
	 graph export linearity_`var'.png, replace
}

* Linearità - Gov Expense vs X
foreach var in trade_openness export_to_us_pct import_from_us_pct {
    scatter gov_expense `var', msymbol(circle) || lfit gov_expense `var', lcolor(red) ///
        title("Linearity: Gov Expense vs `var'") legend(label(1 "gov_expense") label(2 "Fitted values")) ///
        graphregion(color(white)) ///
        name(gov_expense_`var', replace)
	 graph export linearity_`var'.png, replace
}

* Linearità - Trade Openness vs X
foreach var in export_to_us_pct import_from_us_pct {
    scatter trade_openness `var', msymbol(circle) || lfit trade_openness `var', lcolor(red) ///
        title("Linearity: Trade Openness vs `var'") legend(label(1 "trade_openness") label(2 "Fitted values")) ///
        graphregion(color(white)) ///
        name(trade_openness_`var', replace)
	 graph export linearity_`var'.png, replace
}

* Linearità - Export to the US(%) vs X
foreach var in  import_from_us_pct trade_openness {
    scatter export_to_us_pct `var', msymbol(circle) || lfit export_to_us_pct `var', lcolor(red) ///
        title("Linearity: Export to the US(%) vs `var'") legend(label(1 "export_to_us_pct") label(2 "Fitted values")) ///
        graphregion(color(white)) ///
        name(export_to_us_pct`var', replace)
	 graph export linearity_`var'.png, replace
}

gen log_gdp = log(gdp)
gen log_export_to_us_pct = log(export_to_us_pct) 

* Multicollinearity:
corr

* Summary statistics:
summarize gdp_growth inflation unemployment gov_expense trade_openness gdp export_to_us_pct

////////////////////////////////////////////////////////////////////////////////
* Step 6a: MRM 
«Il modello base include soltanto l'elasticità export-GDP (β=0,75, p<0,0000).
Aggiungendo inflazione e disoccupazione – controlli macro standard – il coefficiente rimane stabile (β=0,8022854) e la media VIF sale a  5.02.
L'inserimento di dummies di anno distrugge la significatività perché assorbe gli shock globali (2008, 2020), lasciando poca variazione nell'export; tali specifiche sono quindi riportate solo come test addizionali.»

* 1 Model with interactions export × country_id:
xtset country_id year
xtreg gdp_growth c.export_to_us_pct##i.country_id inflation unemployment trade_openness gov_expense covid crisis2008,  fe vce(cluster country_id)
vif, uncentered
xttest3

xtreg gdp_growth c.export_to_us_pct##i.country_id unemployment inflation,  fe vce(cluster country_id)
vif, uncentered
xttest3

xtreg gdp_growth c.export_to_us_pct##i.country_id,  fe vce(cluster country_id)
vif, uncentered
xttest3

scalar b_base = _b[c.export_to_us_pct]
levelsof country_id, local(clist)

preserve
    tempfile slopes
    postfile H int(country) double(slope se) using `slopes', replace
    
    scalar b_base = _b[c.export_to_us_pct]
    levelsof country_id, local(clist)
    
    foreach i of local clist {
        qui lincom c.export_to_us_pct + `i'.country_id#c.export_to_us_pct
        post H (`i') (r(estimate)) (r(se))
    }
    postclose H
    use `slopes', clear
    save slopes_xtreg, replace   //
restore   // 

* 2 Base prediction and scenario counter-factual con β_i:
predict double yhat_base, xbu               // fitted con FE
scalar beta_base = _b[c.export_to_us_pct]
gen double diff10 = .
gen double diff15 = .
gen double diff20 = .

levelsof country_id, local(clist)

foreach i of local clist {
    
    * slope totale β_i
    scalar beta_i = beta_base + _b[`i'.country_id#c.export_to_us_pct]
    
    * Reduction export (country specific) 
    generate double export10_cf = export_to_us_pct*0.90 if country_id==`i'
    generate double export15_cf = export_to_us_pct*0.85 if country_id==`i'
    generate double export20_cf = export_to_us_pct*0.80 if country_id==`i'
    
    * predicted losses
    replace diff10 = beta_i*(export10_cf - export_to_us_pct) if country_id==`i'
    replace diff15 = beta_i*(export15_cf - export_to_us_pct) if country_id==`i'
    replace diff20 = beta_i*(export20_cf - export_to_us_pct) if country_id==`i'
    
    drop export10_cf export15_cf export20_cf
}
* 3  Mean for country – Final file for graphs:
collapse (mean) diff10 diff15 diff20, by(country_id)
capture label drop cid
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
label values country_id cid
save country_losses_slope, replace

////////////////////////////////////////////////////////////////////////////////	  
* Assuming same elasticity in each country (mean effect)------------------------
xtset country_id year
xtreg gdp_growth export_to_us_pct unemployment inflation, fe vce(cluster country_id)
vif, uncentered
xttest3 
* Fitted con FE (xbu = Xβ + α_i)------------------------------------------------
predict double yhat_base, xbu     //
scalar b_export = _b[export_to_us_pct]

* Tariff 10 % ------------------------------------------------------------------
generate double export_scenario_10 = export_to_us_pct*0.90
generate double yhat_cf10 = yhat_base + b_export*(export_scenario_10 - export_to_us_pct)
generate double diff10    = yhat_cf10  - yhat_base

* Tariff 15 % ------------------------------------------------------------------
generate double export_scenario_15 = export_to_us_pct*0.85
generate double yhat_cf15 = yhat_base + b_export*(export_scenario_15 - export_to_us_pct)
generate double diff15    = yhat_cf15  - yhat_base

* Tariff 20 % ------------------------------------------------------------------
generate double export_scenario_20 = export_to_us_pct*0.80
generate double yhat_cf20 = yhat_base + b_export*(export_scenario_20 - export_to_us_pct)
generate double diff20    = yhat_cf20  - yhat_base

collapse (mean) diff10 diff15 diff20, by(country_id)
capture label drop cid
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
label values country_id cid
save country_losses_avg, replace 

////////////////////////////////////////////////////////////////////////////////
* Step 7: graphs interaction:
* Bar-chart:
* Tariff 10%--------------------------------------------------------------------
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/interactive/country_losses_slope.dta", clear
gen diff10_pct = diff10 * 100 
gsort -diff10_pct	                         // From biggest loser
label values country_id cid              //
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
graph bar diff10_pct ,                                        ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                /// <-- dentro la barra
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 10%")     ///
     note("Source: FE model with country-specific slopes") ///
     graphregion(color(white))
graph export bar_diff10_interactive.png, replace width(2000)

* Tariff 15%--------------------------------------------------------------------
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/interactive/country_losses_slope.dta", clear
gen diff15_pct = diff15 * 100 
gsort -diff15_pct	                         // From biggest loser
label values country_id cid              //
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
graph bar diff15_pct ,                                        ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                /// <-- dentro la barra
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 15%")     ///
     note("Source: FE model with country-specific slopes") ///
     graphregion(color(white))
graph export bar_diff15_interactive.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/interactive/country_losses_slope.dta", clear
gen diff20_pct = diff20 * 100 
gsort -diff20_pct	                         // From biggest loser
label values country_id cid              //
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
graph bar diff20_pct ,                                        ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                /// <-- dentro la barra
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 20%")     ///
     note("Source: FE model with country-specific slopes") ///
     graphregion(color(white))
graph export bar_diff20_interactive.png, replace width(2000)

*Scatter: Loss vs Export to the US:
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/interactive/country_losses_slope.dta", clear
merge 1:1 country_id using "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/interactive/avg_share_export.dta", keep(match) nogen
capture label drop cid
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
label values country_id cid

* Tariff 10%--------------------------------------------------------------------
gen diff10_pct = diff10 * 100 
gen share_us_pct = share_us_gdp * 100 
gsort diff10_pct                // 
twoway scatter diff10_pct share_us_pct, ///
     msymbol(O) mcolor(navy) ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall) ///
     xscale(log) ///
     xlabel(0.2 0.5 1 2 5 10, grid format(%4.1f)) ///
     /* ← aggiungi tacche che ti servono */ ///
     yscale(range(-0.60 0.3)) ///
     ylabel(-0.60(0.10)0.3, grid) ///
     yline(0, lcolor(gs11) lpattern(solid)) ///
     xtitle("Average export to US (% of GDP) – log scale") ///
     ytitle("Δ GDP growth (%) – tariff 10%") ///
     title("Export Exposure vs GDP-Loss (country-specific slopes)") ///
     note("Source: FE model with export×country interaction") ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss_interactive10.png, replace width(2000)

* Tariff 15%--------------------------------------------------------------------
gen diff15_pct = diff15 * 100 
gsort diff15_pct                // 
twoway scatter diff15_pct share_us_pct, ///
     msymbol(O) mcolor(navy) ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall) ///
     xscale(log) ///
     xlabel(0.2 0.5 1 2 5 10, grid format(%4.1f)) ///
     /* ← aggiungi tacche che ti servono */ ///
     yscale(range(-0.90 0.4)) ///
     ylabel(-0.90(0.10)0.4, grid) ///
     yline(0, lcolor(gs11) lpattern(solid)) ///
     xtitle("Average export to US (% of GDP) – log scale") ///
     ytitle("Δ GDP growth (%) – tariff 15%") ///
     title("Export Exposure vs GDP-Loss (country-specific slopes)") ///
     note("Source: FE model with export×country interaction") ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss_interactive15.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
gen diff20_pct = diff20 * 100 
gsort diff20_pct                // 
twoway scatter diff20_pct share_us_pct, ///
     msymbol(O) mcolor(navy) ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall) ///
     xscale(log) ///
     xlabel(0.2 0.5 1 2 5 10, grid format(%4.1f)) ///
     /* ← aggiungi tacche che ti servono */ ///
     yscale(range(-1.2 0.55)) ///
     ylabel(-1.2(0.10)0.55, grid) ///
     yline(0, lcolor(gs11) lpattern(solid)) ///
     xtitle("Average export to US (% of GDP) – log scale") ///
     ytitle("Δ GDP growth (%) – tariff 20%") ///
     title("Export Exposure vs GDP-Loss (country-specific slopes)") ///
     note("Source: FE model with export×country interaction") ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss_interactive20.png, replace width(2000)

////////////////////////////////////////////////////////////////////////////////
* Step 7b: graphs mean:
* Bar chart:
* Tariff 10%--------------------------------------------------------------------
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/average/country_losses_avg.dta", clear
gen diff10_pct = diff10 * 100 
gsort -diff10_pct
graph bar diff10_pct ,                                        ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                /// <-- dentro la barra
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 10%")     ///
     note("Source: FE model with single-EU export-growth slopes") ///
     graphregion(color(white))
graph export bar_diff10_avg.png, replace width(2000)

* Tariff 15%-------------------------------------------------------------------- 
gen diff15_pct = diff15 * 100 
gsort -diff15_pct
graph bar diff15_pct ,                                        ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                /// <-- dentro la barra
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 15%")     ///
     note("Source: FE model with single-EU export-growth slopes") ///
     graphregion(color(white))
graph export bar_diff15_avg.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
gen diff20_pct = diff20 * 100 
gsort -diff20_pct
graph bar diff20_pct ,                                        ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                /// <-- dentro la barra
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 20%")     ///
     note("Source: FE model with single-EU export-growth slopes") ///
     graphregion(color(white))
graph export bar_diff20_avg.png, replace width(2000)

* Scatter: Loss vs Export to the US:
* Tariff 10%--------------------------------------------------------------------
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/average/country_losses_avg.dta", clear               
merge 1:1 country_id using "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/average/avg_share_export.dta", keep(match) nogen
gen share_us_pct = share_us_gdp * 100 
label values country_id cid     // apply country-name labels
gsort diff10_pct
sort share_us_pct
twoway scatter diff10_pct share_us_pct,                        ///
     msymbol(O) mcolor(navy)                               ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall)     ///
     xscale(log)                                           ///
     xlabel(0.1 0.2 0.5 1 2 5 10, grid format(%4.1f))      ///
	 yscale(range(-0.8 0.0)) 							  ///
     ylabel(-0.8(0.2)0, grid)                             ///
     xtitle("Average export to US (% of GDP)  –  log scale") ///
     ytitle("Δ GDP growth (%) – tariff 10%")             ///
     title("Export Exposure vs GDP-Loss (10% tariff)")    ///
     note("Source: FE model with single-EU elasticity")     ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss_avg10.png, replace width(2000)

* Tariff 15%--------------------------------------------------------------------
gsort diff15_pct
sort share_us_pct
twoway scatter diff15_pct share_us_pct,                        ///
     msymbol(O) mcolor(navy)                               ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall)     ///
     xscale(log)                                           ///
     xlabel(0.1 0.2 0.5 1 2 5 10, grid format(%4.1f))      ///
	 yscale(range(-1.2 0.0)) 							  ///
     ylabel(-1.1(0.2)0, grid)                             ///
     xtitle("Average export to US (% of GDP)  –  log scale") ///
     ytitle("Δ GDP growth (%) – tariff 15%")             ///
     title("Export Exposure vs GDP-Loss (15% tariff)")    ///
     note("Source: FE model with single-EU elasticity")     ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss_avg15.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
gsort diff20_pct
sort share_us_pct
twoway scatter diff20_pct share_us_pct,                        ///
     msymbol(O) mcolor(navy)                               ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall)     ///
     xscale(log)                                           ///
     xlabel(0.1 0.2 0.5 1 2 5 10, grid format(%4.1f))      ///
	 yscale(range(-1.2 0.0)) 							  ///
     ylabel(-1.6(0.2)0, grid)                             ///
     xtitle("Average export to US (% of GDP)  –  log scale") ///
     ytitle("Δ GDP growth (%) – tariff 20%")             ///
     title("Export Exposure vs GDP-Loss (20% tariff)")    ///
     note("Source: FE model with single-EU elasticity")     ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss_avg20.png, replace width(2000)

////////////////////////////////////////////////////////////////////////////////
* Step 8: Retaliation:
clear
import delimited "~/Desktop/Riccardo/Paper/DATA/DiD/Model 1 originale.csv", clear
browse

////////////////////////////////////////////////////////////////////////////////
* Step 2: checking the dataset
drop in 2
drop if v1 == "Model_1_numeric"
rename v1 country
rename v2 year
rename v3 gdp_growth
rename v4 gov_expense
rename v5 inflation
rename v6 trade_openness
rename v7 unemployment
rename v8 gdp
rename v9 export_to_us
rename v10 export_to_us_share
rename v11 import_from_us
rename v12 import_from_us_share
destring year , replace
destring gdp_growth , replace
destring gov_expense , replace
destring inflation , replace
destring trade_openness , replace
destring unemployment , replace
destring gdp , replace
destring unemployment , replace
destring export_to_us , replace
destring export_to_us_share , replace
destring import_from_us , replace
destring import_from_us_share , replace
local pctvars gdp_growth gov_expense inflation trade_openness ///
             unemployment export_to_us_share import_from_us_share
quietly foreach v of varlist `pctvars' {
    replace `v' = `v'/100   if !missing(`v')
}
rename export_to_us_share export_to_us_pct
rename import_from_us_share import_from_us_pct

egen country_id = group(country) //Creating numeric variables
label variable country_id "Numeric country panel ID"
gen covid = 0
replace covid = 1 if year == 2020 | year == 2021
gen crisis2008 = 0
replace crisis2008 = 1 if year == 2008 | year == 2009

collapse (mean) share_us_gdp = import_from_us_pct, by(country_id)
save avg_share_retaliation.dta, replace

xtset country_id year
asdoc xtreg gdp_growth c.import_from_us_pct##i.country_id inflation unemployment, ///
      fe vce(cluster country_id) 
vif, uncentered //clear multicollinearity of the interactive ()
xttest3 //

scalar b_base = _b[c.import_from_us_pct]
levelsof country_id, local(clist)

preserve
    tempfile slopes
    postfile H int(country) double(slope se) using `slopes', replace
    
    scalar b_base = _b[c.import_from_us_pct]
    levelsof country_id, local(clist)
    
    foreach i of local clist {
        qui lincom c.import_from_us_pct + `i'.country_id#c.import_from_us_pct
        post H (`i') (r(estimate)) (r(se))
    }
    postclose H
    use `slopes', clear
    save slopes_xtreg_retaliation, replace   //
restore   // 

* 2 Base prediction and scenario counter-factual con β_i:
predict double yhat_base, xbu               // fitted con FE
scalar beta_base = _b[c.import_from_us_pct]
gen double diff10_import = .
gen double diff15_import = .
gen double diff20_import = .

levelsof country_id, local(clist)

foreach i of local clist {
    
    * slope totale β_i
    scalar beta_i = beta_base + _b[`i'.country_id#c.import_from_us_pct]
    
    * Reduction export (country specific) 
    generate double import10_cf = import_from_us_pct*0.90 if country_id==`i'
    generate double import15_cf = import_from_us_pct*0.85 if country_id==`i'
    generate double import20_cf = import_from_us_pct*0.80 if country_id==`i'
    
    * predicted losses
    replace diff10_import = beta_i*(import10_cf - import_from_us_pct) if country_id==`i'
    replace diff15_import = beta_i*(import15_cf - import_from_us_pct) if country_id==`i'
    replace diff20_import = beta_i*(import20_cf - import_from_us_pct) if country_id==`i'
    
    drop import10_cf import15_cf import20_cf
}
* 3  Mean for country – Final file for graphs:
collapse (mean) diff10_import diff15_import diff20_import, by(country_id)
save country_losses_slope_retaliation, replace

* Line-chart:
* Tariff 10%--------------------------------------------------------------------
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/retaliation int/country_losses_slope_retaliation.dta", clear          // 
gen diff10_import_pct =  diff10_import * 100
gen diff15_import_pct =  diff15_import * 100
gen diff20_import_pct =  diff20_import * 100

gsort -diff10_import_pct	                         // From biggest loser
label values country_id cid              //
label define cid  ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace						 
graph bar diff10_import_pct, ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                ///
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 10%")     ///
     note("Source: FE model with country-specific slopes") ///
     graphregion(color(white))
graph export bar_diff10_retaliation_interactive.png, replace width(2000)

* Tariff 15%--------------------------------------------------------------------
gsort -diff15_import_pct	                         // From biggest loser
label values country_id cid              //
label define cid  ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace	
graph bar diff15_import_pct, ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                ///
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 15%")     ///
     note("Source: FE model with country-specific slopes") ///
     graphregion(color(white))
graph export bar_diff15_retaliation_interactive.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
gsort -diff20_import_pct	                         // From biggest loser
label values country_id cid              //
label define cid  ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
graph bar diff20_import_pct, ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                ///
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 20%")     ///
     note("Source: FE model with country-specific slopes") ///
     graphregion(color(white))
graph export bar_diff20_retaliation_interactive.png, replace width(2000)

* Scatter: Loss vs Import from the US:
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/retaliation int/country_losses_slope_retaliation.dta", clear          // 
merge 1:1 country_id using "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/retaliation int/avg_share_retaliation.dta", keep(match) nogen //
gen share_us_pct = share_us_gdp * 100 
label define cid  ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace	
label values country_id cid  

* Tariff 10%--------------------------------------------------------------------
gsort diff10_import_pct                // 
twoway scatter diff10_import_pct share_us_pct, /// 
     msymbol(O) mcolor(navy) ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall) ///
     xscale(log) ///
     xlabel(0.05 0.1 0.2 0.5 1 2 5 10, grid format(%4.1f)) ///
     /* ← aggiungi tacche che ti servono */ ///
     yscale(range(-2.2 0.30)) ///
     ylabel(-2.2(0.10)0.30, grid) ///
     yline(0, lcolor(gs11) lpattern(solid)) ///
     xtitle("Average imports to US (% of GDP) – log scale") ///
     ytitle("Δ GDP growth (%) – tariff 10%") ///
     title("Import Exposure vs GDP-Loss (country-specific slopes)") ///
     note("Source: FE model with imports×country interaction") ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss10_retaliation_interactive.png, replace width(2000)

* Tariff 15%--------------------------------------------------------------------
gsort diff15_import_pct                // 
twoway scatter diff15_import_pct share_us_pct, ///
     msymbol(O) mcolor(navy) ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall) ///
     xscale(log) ///
     xlabel(0.05 0.1 0.2 0.5 1 2 5 10, grid format(%4.1f)) ///
     /* ← aggiungi tacche che ti servono */ ///
     yscale(range(-3.4 0.3)) ///
     ylabel(-3.4(0.10)0.3, grid) ///
     yline(0, lcolor(gs11) lpattern(solid)) ///
     xtitle("Average import to US (% of GDP) – log scale") ///
     ytitle("Δ GDP growth (%) – tariff 15%") ///
     title("Import Exposure vs GDP-Loss (country-specific slopes)") ///
     note("Source: FE model with import×country interaction") ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss15_retaliation_interactive.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
gsort diff20_import_pct                // 
twoway scatter diff20_import_pct share_us_pct, ///
     msymbol(O) mcolor(navy) ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall) ///
     xscale(log) ///
     xlabel(0.05 0.1 0.2 0.5 1 2 5 10, grid format(%4.1f)) ///
     /* ← aggiungi tacche che ti servono */ ///
     yscale(range(-4.5 0.70)) ///
     ylabel(-4.5(0.10)0.70, grid) ///
     yline(0, lcolor(gs11) lpattern(solid)) ///
     xtitle("Average import to US (% of GDP) – log scale") ///
     ytitle("Δ GDP growth (%) – tariff 20%") ///
     title("Import Exposure vs GDP-Loss (country-specific slopes)") ///
     note("Source: FE model with import×country interaction") ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss20_retaliation_interactive.png, replace width(2000)

////////////////////////////////////////////////////////////////////////////////
* Assuming same elasticity in each country (mean effect)------------------------
xtset country_id year
asdoc xtreg gdp_growth import_from_us_pct inflation unemployment, fe vce(cluster country_id)
vif, uncentered
xttest3 
* Fitted con FE (xbu = Xβ + α_i)------------------------------------------------
predict double yhat_base, xbu     //
scalar b_import = _b[import_from_us_pct]

* Tariff 10 % ------------------------------------------------------------------
generate double import_scenario_10 = import_from_us_pct*0.90
generate double yhat_cf10 = yhat_base + b_export*(import_scenario_10 - import_from_us_pct)
generate double diff10_import    = yhat_cf10  - yhat_base

* Tariff 15 % ------------------------------------------------------------------
generate double import_scenario_15 = import_from_us_pct*0.85
generate double yhat_cf15 = yhat_base + b_export*(import_scenario_15 - import_from_us_pct)
generate double diff15_import    = yhat_cf15  - yhat_base

* Tariff 20 % ------------------------------------------------------------------
generate double import_scenario_20 = import_from_us_pct*0.80
generate double yhat_cf20 = yhat_base + b_export*(import_scenario_20 - import_from_us_pct)
generate double diff20_import    = yhat_cf20  - yhat_base

collapse (mean) diff10_import diff15_import diff20_import, by(country_id)
capture label drop cid
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
label values country_id cid
save country_losses_avg_retaliation, replace 

* Bar chart:
* Tariff 10%--------------------------------------------------------------------
use country_losses_avg_retaliation, clear
gen diff10_import_pct =  diff10_import * 100
gen diff15_import_pct =  diff15_import * 100
gen diff20_import_pct =  diff20_import * 100
gsort -diff10_import_pct
graph bar diff10_import_pct, ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                ///
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 10%")     ///
     note("Source: FE model with single-EU import–growth elasticity") ///
     graphregion(color(white))
graph export bar_diff10_retaliation_avg.png, replace width(2000)

* Tariff 15%-------------------------------------------------------------------- 
use country_losses_avg_retaliation, clear
gsort -diff15_import_pct
graph bar diff15_import_pct, ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                ///
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 15%")     ///
     note("Source: FE model with single-EU import–growth elasticity") ///
     graphregion(color(white))
graph export bar_diff15_retaliation_avg.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
use country_losses_avg_retaliation, clear
gsort -diff20_import_pct
graph bar diff20_import_pct, ///
     over(country_id , gap(0) sort(1) descending          ///
          label(angle(45) valuelabel labsize(small)))     ///
     blabel(bar , format(%6.4f) size(tiny)                ///
                  position(center) gap(2))                ///
     bargap(40) yline(0 , lcolor(gs12))                   ///
     ytitle("Δ GDP growth (%)")                          ///
     title("Predicted GDP-Growth Loss – Tariff 20%")     ///
     note("Source: FE model with single-EU import–growth elasticity") ///
     graphregion(color(white))
graph export bar_diff20_retaliation_avg.png, replace width(2000)

* Scatter: Loss vs import to the US:
* Tariff 10%--------------------------------------------------------------------
use country_losses_avg_retaliation, clear               
merge 1:1 country_id using "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/retaliation int/avg_share_retaliation.dta", keep(match) nogen //
gen share_us_pct = share_us_gdp * 100 
label values country_id cid     // 
gsort diff10_import_pct
sort share_us_pct
twoway scatter diff10_import_pct share_us_pct,                        ///
     msymbol(O) mcolor(navy)                               ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall)     ///
     xscale(log)                                           ///
     xlabel(0.05 0.1 0.2 0.5 1 2 5 10, grid format(%4.1f))      ///
	 yscale(range(-0.3 0)) ///
     ylabel(-0.3(0.05)0, grid)                             ///
     xtitle("Average import to US (% of GDP)  –  log scale") ///
     ytitle("Δ GDP growth (%) – tariff 10%")             ///
     title("Import Exposure vs GDP-Loss (10% tariff)")    ///
     note("Source: FE model with single-EU elasticity")     ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss10_retaliation_avg.png, replace width(2000)

* Tariff 15%--------------------------------------------------------------------
label values country_id cid     // 
gsort diff15_import_pct
sort share_us_pct
twoway scatter diff15_import_pct share_us_pct,                        ///
     msymbol(O) mcolor(navy)                               ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall)     ///
     xscale(log)                                           ///
     xlabel(0.05 0.1 0.2 0.5 1 2 5 10, grid format(%4.1f))      ///
     ylabel(-0.45(0.05)0, grid)                             ///
     xtitle("Average import to US (% of GDP)  –  log scale") ///
     ytitle("Δ GDP growth (%) – tariff 15%")             ///
     title("Import Exposure vs GDP-Loss (15% tariff)")    ///
     note("Source: FE model with single-EU elasticity")     ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss15_retaliation_avg.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
label values country_id cid     //
gsort diff20_import_pct
sort share_us_pct
twoway scatter diff20_import_pct share_us_pct,                        ///
     msymbol(O) mcolor(navy)                               ///
     mlabel(country_id) mlabangle(45) mlabsize(vsmall)     ///
     xscale(log)                                           ///
     xlabel(0.05 0.1 0.2 0.5 1 2 5 10, grid format(%4.1f))      ///
     ylabel(-0.55(0.05)0, grid)                             ///
     xtitle("Average import to US (% of GDP)  –  log scale") ///
     ytitle("Δ GDP growth (%) – tariff 20%")             ///
     title("Import Exposure vs GDP-Loss (20% tariff)")    ///
     note("Source: FE model with single-EU elasticity")     ///
     legend(off) graphregion(color(white))
graph export scatter_log_loss20_retaliation_avg.png, replace width(2000)

////////////////////////////////////////////////////////////////////////////////
* Step 9: Final graphs:
* export-slope + import-slope: 
clear
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/interactive/country_losses_slope.dta", clear
merge 1:1 country_id using "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/retaliation int/country_losses_slope_retaliation.dta", ///
      keep(match) nogen   //  
rename diff10 diff10_export
rename diff15 diff15_export
rename diff20 diff20_export

gen diff10_export_pct = diff10_export*100
gen diff15_export_pct = diff15_export*100
gen diff20_export_pct = diff20_export*100

gen diff10_import_pct = diff10_import*100
gen diff15_import_pct = diff15_import*100
gen diff20_import_pct = diff20_import*100

gen diff10_net_pct = (diff10_export_pct + diff10_import_pct)     // net effect
gen diff15_net_pct = (diff15_export_pct + diff15_import_pct)     // net effect
gen diff20_net_pct = (diff20_export_pct + diff20_import_pct)     // net effect
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
label values country_id cid

* Tariff 10%--------------------------------------------------------------------
* scatter export vs import:
* Quasi tutti i paesi si dispongono nel quadrante ↙, il che indica che per la maggior parte degli Stati membri il dazio del 10 % riduce la crescita sia tramite il canale export sia (in misura minore) tramite il canale import.
	*•	La nuvola è allungata in diagonale: chi perde molto in export tende a perdere (un po') anche in import ⇒ gli effetti sono positivamente correlati.
	*•	Gli outlier (p. es. Estonia in basso a sinistra o Slovak Republic a destra) mostrano situazioni estreme: Estonia ha un impatto export elevato e al tempo stesso un import-shock accentuato; Slovac Republic viceversa ha quasi nulli effetti export ma un import-shock relativamente alto.
*	•	Messaggio economico
*	•	Export-shock dominante: lo spostamento lungo l'asse X è in media più ampio di quello lungo Y, confermando che, per l'UE, la trasmissione principale passa dalle esportazioni verso gli USA.
*	•	Diversità intra-UE: la pendenza della "nuvola" racconta che la struttura commerciale (quanto un paese * *esporta/importa dagli USA) spiega la diversa sensibilità tra Stati membri.
twoway scatter diff10_import_pct diff10_export_pct, ///
        msymbol(O) mcolor(navy)                 ///
        mlabel(country_id)                      ///
        mlabpos(0) mlabsize(vsmall) mlabcolor(navy*0.7) ///
        xline(0, lcolor(gs12)) yline(0, lcolor(gs12)) ///
        xlabel(-0.7(.2)0.4) ylabel(-2.2(.2)0.4)           ///
        xtitle("Export-shock 10% (%)")           ///
        ytitle("Import-shock 10% (%)")           ///
        title("Export vs Import losses – 10% tariff") ///
        legend(off) graphregion(color(white))
graph export scatter_exp_imp10_interactive.png, replace width(2000)

* (stacked):
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
label values country_id cid
graph bar diff10_export_pct diff10_import_pct, stack ///
    over(country_id, gap(0) sort(1) descending                           ///
         label(angle(45) valuelabel))                                    ///
    bar(1, color(navy)) bar(2, color(cranberry))                         ///
    legend(order(1 "Export-shock" 2 "Import-shock")                      ///
           pos(12) ring(0) rows(1) size(small) region(lstyle(none)))     ///
    ytitle("Δ GDP growth (%)")                                           ///
    ylabel(-3(0.5)1, format(%6.2f) labsize(vsmall) grid)                                 ///
    yline(0, lcolor(gs12))                                               ///
    title("Combined GDP-Growth Loss – 10% tariff")
graph export bar_net10_interactive.png, replace width(2000)

* Tariff 15%--------------------------------------------------------------------
* scatter export vs import:
twoway scatter diff15_import_pct diff15_export_pct, ///
        msymbol(O) mcolor(navy)                 ///
        mlabel(country_id)                      ///
        mlabpos(0) mlabsize(vsmall) mlabcolor(navy*0.7) ///
        xline(0, lcolor(gs12)) yline(0, lcolor(gs12)) ///
        xlabel(-0.7(.2)0.4) ylabel(-3.2(.2)0.4)           ///
        xtitle("Export-shock 15% (%)")           ///
        ytitle("Import-shock 15% (%)")           ///
        title("Export vs Import losses – 15% tariff") ///
        legend(off) graphregion(color(white))
graph export scatter_exp_imp15_interactive.png, replace width(2000)

* (stacked):
graph bar diff15_export_pct diff15_import_pct, stack ///
    over(country_id, gap(0) sort(1) descending                           ///
         label(angle(45) valuelabel))                                    ///
    bar(1, color(navy)) bar(2, color(cranberry))                         ///
    legend(order(1 "Export-shock" 2 "Import-shock")                      ///
           pos(12) ring(0) rows(1) size(small) region(lstyle(none)))     ///
    ytitle("Δ GDP growth (%)")                                           ///
    ylabel(-4(0.5)1, format(%6.2f) labsize(vsmall) grid)                                 ///
    yline(0, lcolor(gs12))                                               ///
    title("Combined GDP-Growth Loss – 15% tariff")
graph export bar_net15_interactive.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
* scatter export vs import:
twoway scatter diff20_import_pct diff20_export_pct, ///
        msymbol(O) mcolor(navy)                 ///
        mlabel(country_id)                      ///
        mlabpos(0) mlabsize(vsmall) mlabcolor(navy*0.7) ///
        xline(0, lcolor(gs12)) yline(0, lcolor(gs12)) ///
        xlabel(-0.7(.2)0.4) ylabel(-4.5(.2)0.5)           ///
        xtitle("Export-shock 20% (%)")           ///
        ytitle("Import-shock 20% (%)")           ///
        title("Export vs Import losses – 20% tariff") ///
        legend(off) graphregion(color(white))
graph export scatter_exp_imp20_interactive.png, replace width(2000)

* (stacked):
graph bar diff20_export_pct diff20_import_pct, stack ///
    over(country_id, gap(0) sort(1) descending                           ///
         label(angle(45) valuelabel))                                    ///
    bar(1, color(navy)) bar(2, color(cranberry))                         ///
    legend(order(1 "Export-shock" 2 "Import-shock")                      ///
           pos(12) ring(0) rows(1) size(small) region(lstyle(none)))     ///
    ytitle("Δ GDP growth (%)")                                           ///
    ylabel(-8(0.5)2, format(%6.2f) labsize(vsmall) grid)                                 ///
    yline(0, lcolor(gs12))                                               ///
    title("Combined GDP-Growth Loss – 20% tariff")
graph export bar_net20_interactive.png, replace width(2000)

* export-mean + import-mean: 
use "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/average/country_losses_avg.dta", clear
merge 1:1 country_id using "~/Desktop/Riccardo/Paper/DATA/DiD/versione 1/retaliation avg/country_losses_avg_retaliation.dta", ///
      keep(match) nogen   //  
rename diff10 diff10_export
rename diff15 diff15_export
rename diff20 diff20_export

gen diff10_export_pct = diff10_export*100
gen diff15_export_pct = diff15_export*100
gen diff20_export_pct = diff20_export*100

gen diff10_import_pct = diff10_import*100
gen diff15_import_pct = diff15_import*100
gen diff20_import_pct = diff20_import*100

gen diff10_net_pct = (diff10_export_pct + diff10_import_pct)     // net effect
gen diff15_net_pct = (diff15_export_pct + diff15_import_pct)     // net effect
gen diff20_net_pct = (diff20_export_pct + diff20_import_pct)     // net effect
label define cid ///
 1 "Austria"            ///
 2 "Belgium"            ///
 3 "Bulgaria"           ///
 4 "Croatia"            ///
 5 "Cyprus"             ///
 6 "Czechia"            ///
 7 "Denmark"            ///
 8 "Estonia"            ///
 9 "European Union (EU27)" ///
10 "Finland"            ///
11 "France"             ///
12 "Germany"            ///
13 "Greece"             ///
14 "Hungary"            ///
15 "Ireland"            ///
16 "Italy"              ///
17 "Latvia"             ///
18 "Lithuania"          ///
19 "Luxembourg"         ///
20 "Malta"              ///
21 "Netherlands"        ///
22 "Poland"             ///
23 "Portugal"           ///
24 "Romania"            ///
25 "Slovak Republic"    ///
26 "Slovenia"           ///
27 "Spain"              ///
28 "Sweden", replace
label values country_id cid

* Tariff 10%--------------------------------------------------------------------
* scatter export vs import:
twoway scatter diff10_import_pct diff10_export_pct, ///
        msymbol(O) mcolor(navy)                 ///
        mlabel(country_id)                      ///
        mlabpos(0) mlabsize(tiny) mlabcolor(navy*0.7) ///
        xline(0, lcolor(gs12)) yline(0, lcolor(gs12)) ///
        xlabel(-0.7(.2)0.1) ylabel(-0.4(.2)0)           ///
        xtitle("Export-shock 10% (%)")           ///
        ytitle("Import-shock 10% (%)")           ///
        title("Export vs Import losses – 10% tariff") ///
        legend(off) graphregion(color(white))
graph export scatter_exp_imp10_mean.png, replace width(2000)

* (stacked):
graph bar diff10_export_pct diff10_import_pct, stack ///
    over(country_id, gap(0) sort(1) descending                           ///
         label(angle(45) valuelabel))                                    ///
    bar(1, color(navy)) bar(2, color(cranberry))                         ///
    legend(order(1 "Export-shock" 2 "Import-shock")                      ///
           pos(12) ring(0) rows(1) size(small) region(lstyle(none)))     ///
    ytitle("Δ GDP growth (%)")                                           ///
    ylabel(-1(0.5)0.5, format(%6.2f) labsize(vsmall) grid)                                 ///
    yline(0, lcolor(gs12))                                               ///
    title("Combined GDP-Growth Loss – 10% tariff")
graph export bar_net10_mean.png, replace width(2000)

* Tariff 15%--------------------------------------------------------------------
* scatter export vs import:
twoway scatter diff15_import_pct diff15_export_pct, ///
        msymbol(O) mcolor(navy)                 ///
        mlabel(country_id)                      ///
        mlabpos(0) mlabsize(tiny) mlabcolor(navy*0.7) ///
        xline(0, lcolor(gs12)) yline(0, lcolor(gs12)) ///
        xlabel(-0.7(.2)0.1) ylabel(-0.6(.2)0.1)           ///
        xtitle("Export-shock 15% (%)")           ///
        ytitle("Import-shock 15% (%)")           ///
        title("Export vs Import losses – 15% tariff") ///
        legend(off) graphregion(color(white))
graph export scatter_exp_imp15_mean.png, replace width(2000)

* (stacked):
graph bar diff15_export_pct diff15_import_pct, stack ///
    over(country_id, gap(0) sort(1) descending                           ///
         label(angle(45) valuelabel))                                    ///
    bar(1, color(navy)) bar(2, color(cranberry))                         ///
    legend(order(1 "Export-shock" 2 "Import-shock")                      ///
           pos(12) ring(0) rows(1) size(small) region(lstyle(none)))     ///
    ytitle("Δ GDP growth (%)")                                           ///
    ylabel(-1.5(0.5)0.5, format(%6.2f) labsize(vsmall) grid)                                 ///
    yline(0, lcolor(gs12))                                               ///
    title("Combined GDP-Growth Loss – 15% tariff")
graph export bar_net15_mean.png, replace width(2000)

* Tariff 20%--------------------------------------------------------------------
* scatter export vs import:
twoway scatter diff20_import_pct diff20_export_pct, ///
        msymbol(O) mcolor(navy)                 ///
        mlabel(country_id)                      ///
        mlabpos(0) mlabsize(tiny) mlabcolor(navy*0.7) ///
        xline(0, lcolor(gs12)) yline(0, lcolor(gs12)) ///
        xlabel(-1.6(.2)0.1) ylabel(-0.8(.2)0.1)           ///
        xtitle("Export-shock 20% (%)")           ///
        ytitle("Import-shock 20% (%)")           ///
        title("Export vs Import losses – 20% tariff") ///
        legend(off) graphregion(color(white))
graph export scatter_exp_imp20_mean.png, replace width(2000)

* (stacked):
graph bar diff20_export_pct diff20_import_pct, stack ///
    over(country_id, gap(0) sort(1) descending                           ///
         label(angle(45) valuelabel))                                    ///
    bar(1, color(navy)) bar(2, color(cranberry))                         ///
    legend(order(1 "Export-shock" 2 "Import-shock")                      ///
           pos(12) ring(0) rows(1) size(small) region(lstyle(none)))     ///
    ytitle("Δ GDP growth (%)")                                           ///
    ylabel(-2(0.5)0.5, format(%6.2f) labsize(vsmall) grid)                                 ///
    yline(0, lcolor(gs12))                                               ///
    title("Combined GDP-Growth Loss – 20% tariff")
graph export bar_net20_mean.png, replace width(2000)

////////////////////////////////////////////////////////////////////////////////
* Step 10: final check assumptions
* Normality Errors--------------------------------------------------------------
drop resid
predict resid, resid
histogram resid, bin(672) normal ///
        start(`=min(resid)') ///
        lcolor(gs10) lwidth(thin)  ///
        ytitle("Frequency") ///
        xtitle("Residuals") ///
        title("Distribution of residuals") ///
        graphregion(color(white))
graph export resid_histogram_interactive.png, replace
//A colpo d'occhio i residui sono sostanzialmente centrati, ma le code non seguono perfettamente la curva nera → qualche (moderata) deviazione dalla normalità è plausibile
// Per β-stimatori in regressione lineare la normalità dei residui non è richiesta: per n abbastanza grande, grazie al teorema del limite centrale, i coefficienti sono comunque asintoticamente normali. Con ~700 osservazioni l'argomento "large-sample" è robusto.

* 3. Q-Q plot-------------------------------------------------------------------
qnorm resid, grid graphregion(color(white))
graph export resid_qnorm_interactive.png, replace
//Presentazione Mostra il Q–Q e riporta che, pur rifiutando la normalità a livello formale, le deviazioni sono limitate alle code e non alterano le conclusioni grazie all'uso di cluster-robust SE. Code pesanti / outlier Gli scarti nelle code indicano che alcuni anni-paese hanno shock particolari.

* 4. Test di normalità----------------------------------------------------------
sktest  resid     // Skewness-Kurtosis Con n = 672 il test ha molta potenza: rifiutiamo H0 (normalità). Le code risultano più pesanti di quanto atteso (kurtosi) e c'è una leggera asimmetria (p(skewness) ≈ 0.01)
swilk   resid     // Shapiro-Wilk Anche qui rifiutiamo H0: la distribuzione dei residui non è perfettamente normale
//I test rifiutano la normalità, ma il modello rimane valido perché usi cluster-robust errors e hai un campione ampio.


misstable summarize                               // riepilogo %NA

*–––  b) dettaglio per riga (se vuoi scoprire linee "sporche")
egen flag_miss = rowmiss(*)                       // n° di NA su ogni riga
list if flag_miss>0                               // osservazioni con NA
drop flag_miss                                    // pulizia

/* facoltativo – conteggio per singola variabile                     */
foreach v of varlist gdp_growth export_to_us_pct import_from_us_pct inflation ///
                    unemployment trade_openness gov_expense {
        count if missing(`v')
        di "`v'  →  " r(N)
}

/* 2.  BOXPLOT PER OUTLIER‐SCAN                                    */
* Esempio singolo -------------------------------------------------
graph box gdp, ytitle("GDP") ///
          title("Box-plot: GDP level")
graph box trade_openness, ytitle("Trade Openness (%)") ///
          title("Box-plot: Trade Openness (% of GDP)")
graph box trade_openness, ytitle("Government Expense (%)") ///
          title("Box-plot: Government Expense (% of GDP)")
* Esempio multiplo in una sola figura ----------------------------
graph box export_to_us_pct import_from_us_pct inflation unemployment gdp_growth  ///
          trade_openness gov_expense, ///
          asyvars nooutsides  /* oppure outliers per mostrarli */ ///
          ytitle("Value (decimal)") ///
          title("Box-plots — variables (%)")

* Box-plot disaggregato per paese (opzionale) --------------------
graph box export_to_us_pct, over(country_id, gap(0) label(angle(45))) ///
          ytitle("Export to US (% GDP)") ///
          title("Export exposure by country")


















































