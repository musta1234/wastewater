
#function to create epiweek
days <- 1:3650 #approx 10 years
date <- as.Date("2018-01-01") + days
epi_week <- epiweek(date)

#all_days <- 
#  c("Sunday", "Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday")

df <- 
  data.frame(date, epi_week) %>%
  mutate(next_saturday = date + 7 - match(weekdays(date), 
                 c("Sunday", "Monday","Tuesday","Wednesday",
                   "Thursday","Friday", "Saturday")))

#match statement is used to find the index of the day of the week 
# Sunday = 1, Monday = 2, etc. 

