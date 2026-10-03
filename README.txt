German Replication Notebook

This repository contains a Jupyter notebook for the German empirical replication/adaptation of Greenwood, Shleifer, and You (2019), "Bubbles for Fama."

The notebook is designed to read the executed project files from the same project folder at runtime. It expects the following files to be present in the project directory:

- comp_global_daily.dta
- currency_transition_diagnostic.dta
- germany_daily_clean.dta
- germany_industry_monthly.dta
- germany_runup_monthly.dta
- germany_runup_outcomes.dta
- germany_industry_volatility.dta
- germany_gsy_analysis.dta
- germany_crash_results.xlsx
- germany_crash_test.xlsx
- germany_footnote15_regressions.xlsx
- germany_event_time_paths.xlsx
- germany_table1_paper_style.xlsx
- germany_table3_main.xlsx
- germany_replication_checks.xlsx
- germany_event_time_paths.png
- GSY_Germany_Steps_11_17_revised.do

It is a reporting and reproducibility layer. The main raw-data calculations were executed in Stata.
