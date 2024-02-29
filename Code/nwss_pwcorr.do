
import delimited "C:\Users\mustam23\OneDrive - Pfizer\Documents\Projects\underreporting\output\bigfile_nwss_2024-01-18-02PM.csv", clear

drop start_nonvoc-end_ba4ba5 continent region date_period
gen date2 = date(date, "YMD")
format date2 %td

summ inf_mean daily_cases_15dc national

*gen inf_mean_norm = inf_mean
*gen daily_cases_norm = daily_cases_15dc 
*gen national_norm = national
	*foreach var in inf_mean_norm daily_cases_norm national_norm {
		*qui sum `var'
		*replace `var' = (`var' - `r(min)') / (`r(max)'-`r(min)')

	*}

gen reported_ww_ratio =  daily_cases_15dc/(national*100000)
gen estimated_ww_ratio =  inf_mean/(national*100000)

gen ww_reported_ratio =  (national*100000)/daily_cases_15dc
gen ww_estimated_ratio =  (national*100000)/inf_mean

summ reported_ww_ratio estimated_ww_ratio ww_reported_ratio ww_estimated_ratio date2

line reported_ww_ratio estimated_ww_ratio ww_reported_ratio ww_estimated_ratio date2
line reported_ww_ratio date2 || line idr_v5 date2, yaxis(2) 

line estimated_ww_ratio date2 || line idr_v5 date2, yaxis(2) 
 
\\\
gen reported_to_ww =  daily_cases_norm/national_norm
gen estimated_to_ww =  inf_mean_norm/national_norm
gen reported_to_estimated = daily_cases_norm/inf_mean_norm
gen estimated_to_reported = inf_mean_norm/daily_cases_norm

summ inf_mean daily_cases_15dc national inf_mean_norm daily_cases_norm national_norm

pwcorr inf_mean_norm daily_cases_norm national_norm inf_mean daily_cases_15dc national

bys subvariant: pwcorr inf_mean_norm daily_cases_norm national_norm  

summarize reported_to_ww estimated_to_ww reported_to_estimated estimated_to_reported

pwcorr reported_to_ww estimated_to_ww reported_to_estimated estimated_to_reported
bys subvariant: pwcorr reported_to_ww estimated_to_ww reported_to_estimated estimated_to_reported

line reported_to_ww estimated_to_ww reported_to_estimated estimated_to_reported date2

sort date2
line estimated_to_ww reported_to_ww  date2 if reported_to_ww >-0