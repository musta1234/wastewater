
#function to create epiweek
#days <- 1:3650 #approx 10 years
#date <- as.Date("2018-01-01") + days
#epi_week <- epiweek(date)

#all_days <- 
#  c("Sunday", "Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday")

#df <- 
#  data.frame(date, epi_week) %>%
#  mutate(next_saturday = date + 7 - match(weekdays(date), 
#                                          c("Sunday", "Monday","Tuesday","Wednesday",
#                                            "Thursday","Friday", "Saturday")))


#this function returns the next Saturday from a given date
next_saturday <- function(date){
  date + 7 - match(weekdays(date), 
                   c("Sunday", "Monday","Tuesday","Wednesday",
                     "Thursday","Friday", "Saturday"))
}

