import delimited "C:\Users\mustam23\OneDrive - Pfizer\Documents\Projects\underreporting\output\leadlag_15dc_list27.csv"
line unitedstatesofamerica canada lead_lag
line unitedstatesofamerica canada japan unitedkingdom italy france germany spain denmark finland sweden netherlands belgium greece austria ireland luxembourg qatar brazil china chile singapore colombia malaysia southafrica portugal lead_lag
line unitedstatesofamerica canada japan unitedkingdom italy france qatar brazil china chile singapore colombia malaysia southafrica lead_lag
line unitedstatesofamerica canada lead_lag
line unitedstatesofamerica canada lead_lag
line brazil chile colombia lead_lag
line southafrica lead_lag
line unitedstatesofamerica canada lead_lag
line qatar israel lead_lag
line japan china singapore malaysia lead_lag
line unitedkingdom ireland france germany italy spain portugal  greece austria  luxembourg  denmark finland sweden netherlands belgium lead_lag
line unitedkingdom ireland france germany italy spain portugal  greece lead_lag
line unitedkingdom ireland france germany italy spain lead_lag
line greece austria  luxembourg  denmark finland sweden netherlands belgium lead_lag
label var lead_lag "Lead/lag (days)"
save "C:\Users\mustam23\OneDrive - Pfizer\Documents\Projects\underreporting\output\lead_lag_by_country.dta"
