


# write a function with a single input year that returns a correction factor of -3 if the year is 2020, 2 if the year is 2021, 1 if the year is 2022, 0 if the year is 2023, -1 if the year is 2024
correction_factor <- function(year) {
  #force year to be an integer
  year <- as.integer(year)
  if (year == 2019) {
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

epiweek_to_date <- function(Year = 2020, Week =1) {
  #force Year and Week to be integers
  Year <- as.integer(Year)
  Week <- as.integer(Week)
  #return the date of the first day of the week
  return(as.Date(paste0(as.character(Year), "-01-01")) + 7 * as.integer(Week) - 1 + correction_factor(Year))
}

#test the function

