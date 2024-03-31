


# write a function with a single input year that returns a correction factor of -3 if the year is 2020, 2 if the year is 2021, 1 if the year is 2022, 0 if the year is 2023, -1 if the year is 2024
correction_factor <- function(year) {
  #force year to be an integer
  year <- as.integer(year)
  if (year == 2019){
    return(+5)
  } else if (year == 2020) {
    return(-3)
  } else if (year == 2021) {
    return(2)
  } else if (year == 2022) {
    return(1)
  } else if (year == 2023) {
    return(0)
  } else if (year == 2024) {
    return(-1)
  } else if (year == 2025) {
    return(-3)
  } else {
    return(NA)
  }
}

epiweek_to_date <- function(Year = 0, Week =0) {
  #force Year and Week to be integers
  Year <- as.integer(Year)
  Week <- as.integer(Week)
  #return the date of the first day of the week
  return(as.Date(paste0(as.character(Year), "-01-01")) + 7 * as.integer(Week) - 1 
         + correction_factor(Year))
  
}



# Date to CDC date - function to convert date to CDC date (the closest epiweek ())
# The function takes date as input and returns the corresponding CDC date.
# source https://ndc.services.cdc.gov/wp-content/uploads/2021/02/MMWR_Week_overview.pdf
# https://www.cmmcp.org/mosquito-surveillance-data/pages/epi-week-calendars-2008-2024

# need to think further about whether we want to add up forward or backward

date_to_cdcdate <- function(date) {
  #force date to be a date
  date <- as.Date(date)
  year = year(date)
  day = as.numeric(date - as.Date(paste0(year, "-01-01")))
  #calculate number of days from the beginning of the year
  week = as.integer(day/7) + 1
  #calculate the week number
  year_week = paste0(year, "-", week) 
  #calculate the year-week
  cdcdate = epiweek_to_date(year, week) 
  diff = cdcdate -date
  #calculate the difference between the date and the epiweek
  # need to think further about whether we want to add up forward or backward
  
  week = ifelse(diff > 3, week - 1, week)
  #adjust week number if the difference between the date and the epiweek is greater than 3
  week = ifelse(diff < -3, week + 1, week) 
  #adjust week number if the difference between the date and the epiweek is less than -3
  cdcdate = epiweek_to_date(year, week)
  #calculate the epiweek from the adjusted week number
  #diff = epiweek - date 
  #calculate the difference between the date and the epiweek
  return(cdcdate)
}


