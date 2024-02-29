import delimited "C:\Users\mustam23\OneDrive - Pfizer\Documents\Projects\underreporting\Data\OWID\covid-hospitalizations.csv", varnames(1) clear
gen date2 = date(date, "YMD")
keep if entity == "United States"
format date2 %td
line value date2 if indicator == "Daily hospital occupancy"
line value date2 if indicator == "Daily hospital occupancy" || line value date2 if indicator == "Weekly new hospital admissions"
