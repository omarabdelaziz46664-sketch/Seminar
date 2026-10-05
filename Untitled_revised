*******************************************************
* Greenwood, Shleifer, and You (2019) — Germany Replication
*******************************************************

cd "A:\UHH\Seminar\Exhibits\GSY_Germany_Replication"

*******************************************************
* Step 1: Data Preparation
*******************************************************

use "comp_global_daily.dta", clear

* Germany only
keep if fic == "DEU"

* Common equity
keep if tpci == "0"

* Valid industry classification and price
keep if !missing(gsector)
keep if prccd > 0

* Keep the variables required for the replication
keep fic gvkey iid datadate conm isin sedol exchg secstat ///
     conml ggroup gind gsector gsubind stko ajexdi cshoc ///
     cshtrd curcdd div divd prccd prchd prcld tpci trfd

sort gvkey iid datadate

compress

save "germany_prepared.dta", replace

* Diagnostics
display "Observations = " _N
display "Firms/securities = " _N

tabulate gsector, missing

*******************************************************
* Step 2: Currency Conversion
*******************************************************

use "germany_prepared.dta", clear

gen fx_eur = .

replace fx_eur = 1 if curcdd == "EUR"
replace fx_eur = 1/1.95583 if curcdd == "DEM"

* Keep only EUR- and DEM-denominated observations
keep if !missing(fx_eur)

* Diagnostics
count if curcdd == "EUR"
display "EUR observations = " r(N)

count if curcdd == "DEM"
display "DEM observations = " r(N)

count
display "EUR/DEM observations retained = " r(N)

save "germany_currency_clean.dta", replace

*******************************************************
* Step 3: Total-Return Price Construction
*******************************************************

use "germany_currency_clean.dta", clear

* Adjusted total-return price in EUR
gen tr_price_eur = ///
    (prccd * fx_eur / ajexdi) * trfd ///
    if prccd > 0 & ajexdi > 0 & fx_eur > 0 & trfd > 0

* First observation for each security has no prior return
sort gvkey iid datadate

by gvkey iid: gen tr_price_lag = tr_price_eur[_n-1]

gen ret_daily = ///
    tr_price_eur / tr_price_lag - 1 ///
    if tr_price_lag > 0

count if !missing(ret_daily)
display "Daily return observations = " r(N)

save "germany_returns_raw.dta", replace

*******************************************************
* Step 4: Daily Return Cleaning
*******************************************************

use "germany_returns_raw.dta", clear

sort gvkey iid datadate

* Two-day return
by gvkey iid: gen tr_price_lag2 = tr_price_eur[_n-2]

gen ret_2day = ///
    tr_price_eur / tr_price_lag2 - 1 ///
    if tr_price_lag2 > 0

* Identify reversal/error observations
gen bad_reversal = 0

replace bad_reversal = 1 ///
    if (ret_daily > 1 | ret_daily[_n-1] > 1) & ///
       ret_2day < .20 & ///
       gvkey == gvkey[_n-1]

* Identify extreme single-day observations
gen bad_extreme = 0

replace bad_extreme = 1 ///
    if ret_daily > 2

* Final return-cleaning indicator
gen bad_return = ///
    bad_reversal == 1 | bad_extreme == 1

count if bad_reversal == 1
display "Reversal/error observations = " r(N)

count if bad_extreme == 1
display "Extreme-return observations = " r(N)

count if bad_return == 1
display "Total observations removed from return calculation = " r(N)

replace ret_daily = . if bad_return == 1

* Remove temporary variables
drop tr_price_lag tr_price_lag2 ///
     ret_2day bad_reversal bad_extreme bad_return

save "germany_daily_clean.dta", replace

*******************************************************
* Step 5: Monthly Security Returns
*******************************************************

use "germany_daily_clean.dta", clear

gen mdate = mofd(datadate)
format mdate %tm

sort gvkey iid mdate datadate

* Keep the last available trading day for each security-month
by gvkey iid mdate: gen month_last = ///
    datadate == datadate[_N]

keep if month_last

* Previous month-end total-return price
sort gvkey iid mdate

by gvkey iid: gen tr_price_month_lag = ///
    tr_price_eur[_n-1]

gen ret_monthly = ///
    tr_price_eur / tr_price_month_lag - 1 ///
    if tr_price_month_lag > 0 & ///
       mdate == mdate[_n-1] + 1

count
display "Month-end security observations = " r(N)

count if !missing(ret_monthly)
display "Monthly return observations = " r(N)

count
display "Unique securities = " ///
    `: display %12.0f _N'

summarize ret_monthly, detail

save "germany_monthly_security.dta", replace

*******************************************************
* Step 6: Industry Portfolio Returns
*******************************************************

use "germany_monthly_security.dta", clear

sort gvkey iid mdate

* Beginning-of-month market capitalization
gen mktcap_eur = ///
    prccd * fx_eur * cshoc ///
    if prccd > 0 & fx_eur > 0 & cshoc > 0

by gvkey iid: gen mktcap_lag = ///
    mktcap_eur[_n-1]

* Keep observations with valid returns and lagged market capitalization
keep if !missing(gsector) & ///
        !missing(ret_monthly) & ///
        mktcap_lag > 0

* Industry market capitalization
bysort mdate gsector: egen industry_mktcap = ///
    total(mktcap_lag)

* Industry portfolio weights
gen industry_weight = ///
    mktcap_lag / industry_mktcap

* Value-weighted industry returns
gen weighted_industry_return = ///
    industry_weight * ret_monthly

bysort mdate gsector: egen industry_return = ///
    total(weighted_industry_return)

* German market capitalization
bysort mdate: egen market_mktcap = ///
    total(mktcap_lag)

* German market weights
gen market_weight = ///
    mktcap_lag / market_mktcap

* German value-weighted market return
gen weighted_market_return = ///
    market_weight * ret_monthly

bysort mdate: egen market_return = ///
    total(weighted_market_return)

* Net-of-market industry return
gen net_market_return = ///
    industry_return - market_return

* Keep one observation per industry-month
keep gsector mdate industry_return ///
     market_return net_market_return

duplicates drop

sort gsector mdate

* Diagnostics
count
display "Industry-month observations = " r(N)

tabulate gsector, missing

summarize industry_return net_market_return, detail

save "germany_industry_monthly.dta", replace

*******************************************************
* Step 7: Identify GSY run-up episodes
*******************************************************

use "germany_industry_monthly.dta", clear

sort gsector mdate
encode gsector, gen(sector_id)

tsset sector_id mdate, monthly

* -----------------------------------------------------
* 1. Cumulative return measures
* -----------------------------------------------------

gen log_raw = ln(1 + industry_return)
gen log_net = ln(1 + net_market_return)

by sector_id (mdate): gen cumlog_raw = sum(log_raw)
by sector_id (mdate): gen cumlog_net = sum(log_net)

gen raw_24 = ///
    exp(cumlog_raw - L24.cumlog_raw) - 1 ///
    if mdate == L24.mdate + 24

gen net_24 = ///
    exp(cumlog_net - L24.cumlog_net) - 1 ///
    if mdate == L24.mdate + 24

gen raw_60 = ///
    exp(cumlog_raw - L60.cumlog_raw) - 1 ///
    if mdate == L60.mdate + 60

* Information available at run-up month t
gen runup_raw24 = L1.raw_24
gen runup_net24 = L1.net_24
gen runup_raw60 = L1.raw_60

* -----------------------------------------------------
* 2. Run-up qualification
* -----------------------------------------------------

foreach X in 50 75 100 125 150 {

    local x = `X'/100

    gen qualify`X' = 0

    replace qualify`X' = 1 ///
        if !missing(runup_raw24, runup_net24, runup_raw60) & ///
           runup_raw24 >= `x' & ///
           runup_net24 >= `x' & ///
           runup_raw60 >= .50
}

* -----------------------------------------------------
* 3. Keep distinct run-up episodes
* -----------------------------------------------------

foreach X in 50 75 100 125 150 {

    gen prior_event`X' = 0

    forvalues L = 1/24 {
        replace prior_event`X' = 1 ///
            if L`L'.qualify`X' == 1
    }

    gen runup`X' = ///
        qualify`X' == 1 & prior_event`X' == 0
}

* Episode counts
foreach X in 50 75 100 125 150 {
    quietly count if runup`X' == 1
    display "Distinct `X'% run-up episodes = " r(N)
}

save "germany_runup_monthly.dta", replace


*******************************************************
* Step 8: Forward returns and 24-month crash measures
*******************************************************

use "germany_runup_monthly.dta", clear

sort sector_id mdate
tsset sector_id mdate, monthly

* -----------------------------------------------------
* 1. Forward 12- and 24-month returns
* -----------------------------------------------------

gen byte complete12 = 1
forvalues j = 1/12 {
    replace complete12 = 0 ///
        if missing(F`j'.mdate) | F`j'.mdate != mdate + `j'
}

gen byte complete24 = 1
forvalues j = 1/24 {
    replace complete24 = 0 ///
        if missing(F`j'.mdate) | F`j'.mdate != mdate + `j'
}

gen fwd12_raw = ///
    exp(F12.cumlog_raw - cumlog_raw) - 1 ///
    if complete12

gen fwd24_raw = ///
    exp(F24.cumlog_raw - cumlog_raw) - 1 ///
    if complete24

gen fwd12_net = ///
    exp(F12.cumlog_net - cumlog_net) - 1 ///
    if complete12

gen fwd24_net = ///
    exp(F24.cumlog_net - cumlog_net) - 1 ///
    if complete24

* -----------------------------------------------------
* 2. Maximum drawdown over following 24 months
* -----------------------------------------------------

gen maxdd_raw24 = 0
gen maxdd_net24 = 0

forvalues j = 1/24 {

    gen wealth_raw = ///
        exp(F`j'.cumlog_raw - cumlog_raw) ///
        if complete24

    gen wealth_net = ///
        exp(F`j'.cumlog_net - cumlog_net) ///
        if complete24

    forvalues k = 1/`j' {

        gen peak_raw = ///
            exp(F`k'.cumlog_raw - cumlog_raw) ///
            if complete24

        gen peak_net = ///
            exp(F`k'.cumlog_net - cumlog_net) ///
            if complete24

        replace maxdd_raw24 = min(maxdd_raw24, ///
            wealth_raw/peak_raw - 1) ///
            if complete24 & !missing(peak_raw, wealth_raw)

        replace maxdd_net24 = min(maxdd_net24, ///
            wealth_net/peak_net - 1) ///
            if complete24 & !missing(peak_net, wealth_net)

        drop peak_raw peak_net
    }

    drop wealth_raw wealth_net
}

* -----------------------------------------------------
* 3. Crash indicators
* -----------------------------------------------------

gen crash_raw40 = ///
    maxdd_raw24 <= -.40 ///
    if complete24

gen crash_net40 = ///
    maxdd_net24 <= -.40 ///
    if complete24

display "100% run-up episodes = " ///
    r(N)

count if runup100 == 1
display "100% run-ups = " r(N)

count if runup100 == 1 & complete24 == 1
display "100% run-ups with complete 24M outcome = " r(N)

count if runup100 == 1 & crash_raw40 == 1
display "100% run-ups followed by raw 40% crash = " r(N)

count if runup100 == 1 & crash_net40 == 1
display "100% run-ups followed by net-market 40% crash = " r(N)

save "germany_runup_outcomes.dta", replace


*******************************************************
* Step 9: GSY pre-run-up annualized volatility
*******************************************************

use "germany_daily_clean.dta", clear

gen mdate = mofd(datadate)
format mdate %tm

gen mktcap_eur = ///
    prccd * fx_eur * cshoc ///
    if prccd > 0 & fx_eur > 0 & cshoc > 0

sort gvkey iid datadate
by gvkey iid: gen mktcap_lag = mktcap_eur[_n-1]

keep if !missing(gsector) & ///
        !missing(ret_daily) & ///
        mktcap_lag > 0

bysort datadate gsector: egen sector_mktcap = ///
    total(mktcap_lag)

gen daily_weight = ///
    mktcap_lag / sector_mktcap

gen weighted_daily_ret = ///
    daily_weight * ret_daily

bysort datadate gsector: egen industry_daily_ret = ///
    total(weighted_daily_ret)

keep datadate mdate gsector industry_daily_ret
duplicates drop

sort gsector datadate

gen n_daily = !missing(industry_daily_ret)

gen sum_daily = ///
    cond(missing(industry_daily_ret),0,industry_daily_ret)

gen sq_daily = ///
    cond(missing(industry_daily_ret),0,industry_daily_ret^2)

by gsector: gen cum_n = sum(n_daily)
by gsector: gen cum_sum = sum(sum_daily)
by gsector: gen cum_sq = sum(sq_daily)

bysort gsector mdate (datadate): ///
    gen month_last = (_n == _N)

keep if month_last

encode gsector, gen(sector_id)
tsset sector_id mdate, monthly

* Previous 12 months: t-12 through t-1
gen n12 = L1.cum_n - L13.cum_n
gen sum12 = L1.cum_sum - L13.cum_sum
gen sq12 = L1.cum_sq - L13.cum_sq

gen volatility_annual = ///
    sqrt((sq12 - (sum12^2 / n12)) / (n12 - 1)) * sqrt(252) ///
    if n12 > 1 & ///
       mdate == L1.mdate + 1 & ///
       mdate == L13.mdate + 13

keep gsector mdate volatility_annual

save "germany_industry_volatility.dta", replace


*******************************************************
* Step 10: Merge pre-run-up volatility
*******************************************************

use "germany_runup_outcomes.dta", clear

sort gsector mdate

merge 1:1 gsector mdate ///
    using "germany_industry_volatility.dta"

keep if _merge == 3
drop _merge

count if runup100 == 1
display "100% run-up episodes = " r(N)

count if runup100 == 1 & !missing(volatility_annual)
display "100% run-ups with volatility = " r(N)

save "germany_gsy_analysis.dta", replace


*******************************************************
* Step 11: Main crash-probability results
* Final output: Excel
*******************************************************

use "germany_gsy_analysis.dta", clear

tempfile results

postfile results ///
    threshold episodes ///
    raw_crashes raw_crash_prob ///
    net_crashes net_crash_prob ///
    mean_drawdown mean_fwd12 mean_fwd24 ///
    using `results', replace

foreach X in 50 75 100 125 150 {

    quietly count if runup`X' == 1
    local N = r(N)

    quietly count if runup`X' == 1 & crash_raw40 == 1
    local rawN = r(N)

    quietly count if runup`X' == 1 & crash_net40 == 1
    local netN = r(N)

    quietly summarize maxdd_raw24 if runup`X' == 1
    local dd = r(mean)

    quietly summarize fwd12_raw if runup`X' == 1
    local r12 = r(mean)

    quietly summarize fwd24_raw if runup`X' == 1
    local r24 = r(mean)

    post results ///
        (`X') ///
        (`N') ///
        (`rawN') ///
        (`rawN'/`N') ///
        (`netN') ///
        (`netN'/`N') ///
        (`dd') ///
        (`r12') ///
        (`r24')
}

postclose results

use `results', clear

gen raw_crash_pct = round(100*raw_crash_prob,.1)
gen net_crash_pct = round(100*net_crash_prob,.1)
gen fwd12_raw_pct = round(100*mean_fwd12,.1)
gen fwd24_raw_pct = round(100*mean_fwd24,.1)
gen drawdown_pct = round(100*mean_drawdown,.1)

label variable threshold     "Pick-up threshold (%)"
label variable episodes      "Number of run-ups identified"
label variable raw_crash_pct "Raw crashes (%)"
label variable net_crash_pct "Net-market crashes (%)"
label variable fwd12_raw_pct "12M raw return (%)"
label variable fwd24_raw_pct "24M raw return (%)"
label variable drawdown_pct   "24M drawdown (%)"

order threshold episodes ///
      fwd12_raw_pct fwd24_raw_pct ///
      raw_crash_pct net_crash_pct ///
      drawdown_pct

export excel using ///
    "germany_crash_results.xlsx", ///
    firstrow(varlabels) replace


*******************************************************
* Step 12: Unconditional vs. conditional crash test
* Final output: Excel
*******************************************************

use "germany_gsy_analysis.dta", clear

quietly count if complete24 == 1
local N_all = r(N)

quietly count if complete24 == 1 & crash_raw40 == 1
local C_all = r(N)

local P_all = `C_all'/`N_all'

quietly count if runup100 == 1
local N_cond = r(N)

quietly count if runup100 == 1 & crash_raw40 == 1
local C_cond = r(N)

local P_cond = `C_cond'/`N_cond'

local P_pool = (`C_all' + `C_cond')/ ///
               (`N_all' + `N_cond')

local SE = sqrt(`P_pool'*(1-`P_pool') * ///
    (1/`N_all' + 1/`N_cond'))

local Z = (`P_cond' - `P_all')/`SE'
local pvalue = 2*normal(-abs(`Z'))

quietly count if complete24 == 1 & runup100 == 0
local N_non = r(N)

quietly count if complete24 == 1 & ///
    runup100 == 0 & crash_raw40 == 1
local C_non = r(N)

local P_non = `C_non'/`N_non'

local P_pool2 = (`C_cond' + `C_non')/ ///
                (`N_cond' + `N_non')

local SE2 = sqrt(`P_pool2'*(1-`P_pool2') * ///
    (1/`N_cond' + 1/`N_non'))

local Z2 = (`P_cond' - `P_non')/`SE2'
local pvalue2 = 2*normal(-abs(`Z2'))

tempfile testresults

postfile testresults ///
    str30 comparison ///
    N crashes probability ///
    difference z pvalue ///
    using `testresults', replace

post testresults ///
    ("Unconditional") ///
    (`N_all') (`C_all') (`P_all') ///
    (.) (.) (.)

post testresults ///
    ("100% run-up") ///
    (`N_cond') (`C_cond') (`P_cond') ///
    (`P_cond'-`P_all') (`Z') (`pvalue')

post testresults ///
    ("Non-100% months") ///
    (`N_non') (`C_non') (`P_non') ///
    (`P_cond'-`P_non') (`Z2') (`pvalue2')

postclose testresults

use `testresults', clear

gen probability_pct = round(100*probability,.1)
gen difference_pct = round(100*difference,.1)

label variable comparison "Comparison"
label variable N "Observations"
label variable crashes "Crashes"
label variable probability_pct "Crash probability (%)"
label variable difference_pct "Difference (percentage points)"
label variable z "z statistic"
label variable pvalue "p-value"

keep comparison N crashes ///
     probability_pct difference_pct z pvalue

export excel using ///
    "germany_crash_test.xlsx", ///
    firstrow(varlabels) replace

* Fisher exact test
use "germany_gsy_analysis.dta", clear

tabulate runup100 crash_raw40 if complete24 == 1, ///
    row exact

*******************************************************
* Step 13: Footnote-15 crash regressions
* Final output: Excel
*******************************************************

use "germany_gsy_analysis.dta", clear

gen calyear = year(dofm(mdate))

foreach X in 50 75 100 125 150 {

    local x = `X'/100

    gen rup_raw`X' = ///
        (runup_raw24 > `x') if !missing(runup_raw24)

    gen rup_net`X' = ///
        (runup_net24 > `x') if !missing(runup_net24)
}

tempfile regresults

postfile regresults ///
    str5 returntype ///
    threshold ///
    str8 volatility ///
    N ///
    b_runup se_runup p_runup ///
    b_vol se_vol p_vol ///
    r2 ///
    using `regresults', replace

foreach type in raw net {

    foreach X in 50 75 100 125 150 {

        quietly regress crash_raw40 ///
            rup_`type'`X' ///
            if complete24 == 1, ///
            vce(cluster calyear)

        local b = _b[rup_`type'`X']
        local se = _se[rup_`type'`X']
        local p = 2*ttail(e(df_r),abs(`b'/`se'))
        local N = e(N)
        local r2 = e(r2)

        post regresults ///
            ("`type'") (`X') ("No") ///
            (`N') (`b') (`se') (`p') ///
            (.) (.) (.) (`r2')

        quietly regress crash_raw40 ///
            rup_`type'`X' volatility_annual ///
            if complete24 == 1 & !missing(volatility_annual), ///
            vce(cluster calyear)

        local b = _b[rup_`type'`X']
        local se = _se[rup_`type'`X']
        local p = 2*ttail(e(df_r),abs(`b'/`se'))
        local bv = _b[volatility_annual]
        local sv = _se[volatility_annual]
        local pv = 2*ttail(e(df_r),abs(`bv'/`sv'))
        local N = e(N)
        local r2 = e(r2)

        post regresults ///
            ("`type'") (`X') ("Yes") ///
            (`N') (`b') (`se') (`p') ///
            (`bv') (`sv') (`pv') (`r2')
    }
}

postclose regresults

use `regresults', clear

replace b_runup = round(b_runup,.0001)
replace se_runup = round(se_runup,.0001)
replace p_runup = round(p_runup,.0001)
replace b_vol = round(b_vol,.0001)
replace se_vol = round(se_vol,.0001)
replace p_vol = round(p_vol,.0001)
replace r2 = round(r2,.0001)

label variable returntype "Return type"
label variable threshold "Threshold (%)"
label variable volatility "Volatility control"
label variable N "Observations"
label variable b_runup "Run-up coefficient"
label variable se_runup "Run-up standard error"
label variable p_runup "Run-up p-value"
label variable b_vol "Volatility coefficient"
label variable se_vol "Volatility standard error"
label variable p_vol "Volatility p-value"
label variable r2 "R-squared"

export excel using ///
    "germany_footnote15_regressions.xlsx", ///
    firstrow(varlabels) replace


*******************************************************
* Step 14: Event-time normalized price paths
* Output: Excel data + PNG figure
*******************************************************

use "germany_gsy_analysis.dta", clear

preserve

keep if runup100 == 1
keep sector_id mdate crash_raw40

rename mdate event0
rename crash_raw40 episode_crash

gen episode_id = _n

tempfile episodes
save `episodes', replace

restore

keep sector_id mdate industry_return market_return

joinby sector_id using `episodes'

gen event_month = mdate - event0
keep if inrange(event_month,-24,48)

sort episode_id event_month

gen log_ind = ln(1 + industry_return)
gen log_mkt = ln(1 + market_return)

bysort episode_id (event_month): ///
    gen cumlog_ind = sum(log_ind)

bysort episode_id (event_month): ///
    gen cumlog_mkt = sum(log_mkt)

bysort episode_id: egen base_ind = ///
    max(cond(event_month==0,cumlog_ind,.))

bysort episode_id: egen base_mkt = ///
    max(cond(event_month==0,cumlog_mkt,.))

gen ind_index = exp(cumlog_ind - base_ind)
gen mkt_index = exp(cumlog_mkt - base_mkt)

gen crash_index = ind_index if episode_crash == 1
gen noncrash_index = ind_index if episode_crash == 0

collapse ///
    (mean) average=ind_index ///
           crashes=crash_index ///
           noncrashes=noncrash_index ///
           market=mkt_index, ///
    by(event_month)

sort event_month

export excel using ///
    "germany_event_time_paths.xlsx", ///
    firstrow(variables) replace

twoway ///
    (line crashes event_month, sort lcolor(green)) ///
    (line noncrashes event_month, sort lcolor(red) lpattern(dash)) ///
    (line average event_month, sort lcolor(black) lpattern(dash_dot)) ///
    (line market event_month, sort lcolor(blue) lpattern(dot)), ///
    xlabel(-24 -12 0 12 24 36 48) ///
    xscale(range(-24 48)) ///
    xtitle("Event month") ///
    ytitle("Normalized return index") ///
    legend(order(3 "Run-up average" ///
                 1 "Crashes" ///
                 2 "Non-crashes" ///
                 4 "Market return index") cols(2)) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    name(germany_fig1, replace)

graph export ///
    "germany_event_time_paths.png", ///
    replace width(2000)


*******************************************************
* Step 15: Germany Table 1 — paper-style raw 100% run-ups
*******************************************************

cd "A:\UHH\Seminar\Exhibits\GSY_Germany_Replication"

*------------------------------------------------------*
* 1. Identify raw-only 100% run-up episodes
*------------------------------------------------------*

use "germany_industry_monthly.dta", clear

sort gsector mdate

capture drop sector_id
encode gsector, gen(sector_id)

tsset sector_id mdate, monthly

gen log_raw = ln(1 + industry_return)

by sector_id (mdate): gen cumlog_raw = sum(log_raw)

gen raw_24 = ///
    exp(cumlog_raw - L24.cumlog_raw) - 1 ///
    if mdate == L24.mdate + 24

gen runup_raw24 = L1.raw_24

gen qualify100_raw = 0

replace qualify100_raw = 1 ///
    if !missing(runup_raw24) & ///
       runup_raw24 >= 1.00

* Distinct episodes: first qualifying month,
* then suppress following 24 months

gen prior_event = 0

forvalues L = 1/24 {
    replace prior_event = 1 ///
        if L`L'.qualify100_raw == 1
}

gen table1_runup100 = ///
    qualify100_raw == 1 & ///
    prior_event == 0

keep if table1_runup100 == 1

keep gsector mdate runup_raw24

gen episode_id = _n

tempfile rawepisodes
save `rawepisodes', replace

*------------------------------------------------------*
* 2. Attach post-run-up outcomes
*------------------------------------------------------*

use "germany_gsy_analysis.dta", clear

merge 1:1 gsector mdate ///
    using `rawepisodes'

keep if _merge == 3

drop _merge

* Table 1 requires a complete 24-month outcome window
keep if complete24 == 1

display " "
display "==============================================="
display "TABLE 1 ELIGIBLE EPISODES"
display "==============================================="

count

display "Raw-only 100% episodes with complete 24M outcome = " r(N)

*------------------------------------------------------*
* 3. Count firms in each sector-month
*------------------------------------------------------*

preserve

use "germany_daily_clean.dta", clear

gen mdate = mofd(datadate)

bysort gvkey gsector mdate: ///
    gen firm_tag = (_n == 1)

collapse (sum) number_firms = firm_tag, ///
    by(gsector mdate)

tempfile firmcounts
save `firmcounts', replace

restore

merge 1:1 gsector mdate ///
    using `firmcounts', ///
    keep(master match) nogen

*------------------------------------------------------*
* 4. Industry names
*------------------------------------------------------*

gen industry_name = ""

replace industry_name = "Energy"                 if gsector == "10"
replace industry_name = "Materials"              if gsector == "15"
replace industry_name = "Industrials"            if gsector == "20"
replace industry_name = "Consumer Discretionary" if gsector == "25"
replace industry_name = "Consumer Staples"       if gsector == "30"
replace industry_name = "Health Care"            if gsector == "35"
replace industry_name = "Financials"             if gsector == "40"
replace industry_name = "Information Technology" if gsector == "45"
replace industry_name = "Communication Services" if gsector == "50"
replace industry_name = "Utilities"              if gsector == "55"
replace industry_name = "Real Estate"            if gsector == "60"

*------------------------------------------------------*
* 5. Return statistics
*------------------------------------------------------*

gen raw12_pct  = 100*fwd12_raw
gen raw24_pct  = 100*fwd24_raw

* No risk-free series under one-file constraint
gen nrf12_pct = .
gen nrf24_pct = .

gen netm12_pct = 100*fwd12_net
gen netm24_pct = 100*fwd24_net

gen dd24_pct = 100*maxdd_raw24

*------------------------------------------------------*
* 6. Calculate months to price peak and return to peak
*------------------------------------------------------*

preserve

keep gsector mdate

rename mdate event0

gen episode_id = _n

tempfile peakepisodes
save `peakepisodes', replace

restore

preserve

keep gsector mdate industry_return

joinby gsector using `peakepisodes'

gen event_month = mdate - event0

keep if inrange(event_month,0,24)

sort episode_id event_month

* Price index begins at 1.0 in the run-up month
gen log_forward = ///
    cond(event_month == 0, 0, ln(1 + industry_return))

by episode_id (event_month): ///
    gen cumlog_forward = sum(log_forward)

gen price_index = exp(cumlog_forward)

bysort episode_id: ///
    egen peak_index = max(price_index)

gen peak_month_candidate = ///
    event_month if price_index == peak_index

bysort episode_id: ///
    egen months_to_peak = ///
    min(peak_month_candidate)

bysort episode_id: ///
    egen return_to_peak = ///
    max(peak_index)

gen return_to_peak_pct = ///
    100*(return_to_peak - 1)

keep episode_id ///
     months_to_peak ///
     return_to_peak_pct

duplicates drop episode_id, force

tempfile peakresults
save `peakresults', replace

restore

merge 1:1 episode_id ///
    using `peakresults', ///
    keep(master match) nogen

*------------------------------------------------------*
* 7. Readable date and outcome
*------------------------------------------------------*

gen runup_date = string(year(dofm(mdate))) + "-" + ///
    string(month(dofm(mdate)), "%02.0f")

gen outcome = ""

replace outcome = "Crash" ///
    if crash_raw40 == 1

replace outcome = "Non-crash" ///
    if crash_raw40 == 0

*------------------------------------------------------*
* 8. Clean presentation
*------------------------------------------------------*

format raw12_pct raw24_pct ///
       nrf12_pct nrf24_pct ///
       netm12_pct netm24_pct ///
       dd24_pct return_to_peak_pct ///
       %9.1f

sort crash_raw40 mdate

display " "
display "==============================================="
display "GERMANY TABLE 1 — PAPER-STYLE"
display "==============================================="

list ///
    industry_name ///
    number_firms ///
    runup_date ///
    raw12_pct ///
    raw24_pct ///
    nrf12_pct ///
    nrf24_pct ///
    netm12_pct ///
    netm24_pct ///
    dd24_pct ///
    months_to_peak ///
    return_to_peak_pct ///
    outcome, ///
    noobs sep(0)

*------------------------------------------------------*
* 9. Export final Table 1
*------------------------------------------------------*

keep ///
    industry_name ///
    number_firms ///
    runup_date ///
    raw12_pct ///
    raw24_pct ///
    nrf12_pct ///
    nrf24_pct ///
    netm12_pct ///
    netm24_pct ///
    dd24_pct ///
    months_to_peak ///
    return_to_peak_pct ///
    outcome

export excel using ///
    "germany_table1_paper_style.xlsx", ///
    firstrow(variables) replace

display " "
display "==============================================="
display "Saved: germany_table1_paper_style.xlsx"
display "==============================================="

*******************************************************
* End Step 15
*******************************************************

*******************************************************
* Step 16: GSY-style Table 3
* Final output: Excel
*******************************************************

use "germany_gsy_analysis.dta", clear

tempfile results

postfile results ///
    threshold episodes ///
    nrf12_pct nrf24_pct ///
    netm12_pct netm24_pct ///
    nrf24_sd nrf24_skew nrf24_kurt ///
    crash_pct drawdown_crash_pct ///
    boom_pct boom24_nrf_pct ///
    using `results', replace

foreach X in 50 75 100 125 150 {

    quietly count if runup`X' == 1
    local N = r(N)

    quietly count if runup`X' == 1 & crash_raw40 == 1
    local rawN = r(N)

    quietly summarize fwd12_net if runup`X' == 1
    local net12 = 100*r(mean)

    quietly summarize fwd24_net if runup`X' == 1
    local net24 = 100*r(mean)

    quietly summarize maxdd_raw24 ///
        if runup`X' == 1 & crash_raw40 == 1
    local dd = 100*r(mean)

    post results ///
        (`X') ///
        (`N') ///
        (.) ///
        (.) ///
        (`net12') ///
        (`net24') ///
        (.) ///
        (.) ///
        (.) ///
        (100*`rawN'/`N') ///
        (`dd') ///
        (.) ///
        (.)
}

postclose results

use `results', clear

replace netm12_pct = round(netm12_pct,.1)
replace netm24_pct = round(netm24_pct,.1)
replace crash_pct = round(crash_pct,.1)
replace drawdown_crash_pct = round(drawdown_crash_pct,.1)

label variable threshold ///
    "Pick-up threshold (%)"

label variable episodes ///
    "Number of run-ups identified"

label variable nrf12_pct ///
    "12M net of risk-free return (%)"

label variable nrf24_pct ///
    "24M net of risk-free return (%)"

label variable netm12_pct ///
    "12M net of market return (%)"

label variable netm24_pct ///
    "24M net of market return (%)"

label variable nrf24_sd ///
    "Standard deviation of 24M net of risk-free return"

label variable nrf24_skew ///
    "Skewness of 24M net of risk-free return"

label variable nrf24_kurt ///
    "Kurtosis of 24M net of risk-free return"

label variable crash_pct ///
    "Crashes (%)"

label variable drawdown_crash_pct ///
    "Drawdown of crashes (%)"

label variable boom_pct ///
    "Booms (%)"

label variable boom24_nrf_pct ///
    "24M net of risk-free return of booms (%)"

order threshold episodes ///
       nrf12_pct nrf24_pct ///
       netm12_pct netm24_pct ///
       nrf24_sd nrf24_skew nrf24_kurt ///
       crash_pct drawdown_crash_pct ///
       boom_pct boom24_nrf_pct

export excel using ///
    "germany_table3_main.xlsx", ///
    firstrow(varlabels) replace

display "NOTE: Risk-free and boom statistics are N/A because"
display "the replication uses only comp_global_daily.dta."


*******************************************************
* Step 17: Final replication integrity checks
* Final output: Excel
*******************************************************

* Close any leftover postfile handle from a previous failed run
capture postclose checks

* Load final analysis dataset
use "germany_gsy_analysis.dta", clear

* Create temporary checks file
tempfile checks

postfile checks ///
    str55 check ///
    result ///
    using `checks', replace

*------------------------------------------------------*
* 1. Run-up episode counts
*------------------------------------------------------*

foreach X in 50 75 100 125 150 {
    quietly count if runup`X' == 1
    post checks ///
        ("`X'% run-up episodes") ///
        (r(N))
}

*------------------------------------------------------*
* 2. 100% run-up outcome completeness
*------------------------------------------------------*

quietly count if runup100 == 1 & complete24 == 1
post checks ///
    ("100% run-ups with complete 24M outcome") ///
    (r(N))

quietly count if runup100 == 1 & missing(crash_raw40)
post checks ///
    ("100% run-ups with missing crash outcome") ///
    (r(N))

quietly count if runup100 == 1 & missing(volatility_annual)
post checks ///
    ("100% run-ups with missing volatility") ///
    (r(N))

*------------------------------------------------------*
* 3. Event-time output checks
*------------------------------------------------------*

import excel using "germany_event_time_paths.xlsx", ///
    firstrow clear

quietly count
post checks ///
    ("Event-time path observations") ///
    (r(N))

quietly count if event_month == 0
post checks ///
    ("Event-time observations at month 0") ///
    (r(N))

*------------------------------------------------------*
* 4. Close and export integrity checks
*------------------------------------------------------*

postclose checks

use `checks', clear

export excel using ///
    "germany_replication_checks.xlsx", ///
    firstrow(varlabels) replace

* Display final checks in Stata
list, noobs