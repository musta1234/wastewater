# Import WWS data from NWSS
# Source: https://www.cdc.gov/nwss/rv/COVID19-nationaltrend.html
# Source: https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSRegionalLevel.json



coeff = 30000
min_nwss <- (big_nwss %>% filter(!is.na(nwss)) %>% select(date))

pp1 <-
  
  ggplot(big_nwss, aes(x=date)) +
  geom_line(aes(y=daily_cases), color = "blue3") +
  geom_line(aes(y=nwss*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Daily reported infections (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

pp1

coeff = 240000

pp2 <-
  ggplot(bigfile_nwss, aes(x=date)) +
  geom_line(aes(y=inf_mean), color = "blue3") +
  geom_line(aes(y=National*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Daily estimated infections (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )


nwss <- 
  read.delim(paste0(file_loc, "/Data/NWSS/NWSSRegionalLevel_17Jan2024.tab")) %>% 
  filter(date_period == "All Results") %>% 
  mutate(date=as.Date.character(date)) #%>% 
#select(date, National) %>% plot(type="l")

min_nwss <- min(nwss$date) #this is the earliest data point in NWSS database

bigfile_nwss <-
  bigfile %>% filter(location_name == "United States of America" 
                     #& date >= min_nwss
  ) %>% 
  full_join(., nwss) %>% 
  filter(!is.na(National) & !is.na(daily_cases))

coeff = 30000
pp1 <-
  
  ggplot(bigfile_nwss, aes(x=date)) +
  geom_line(aes(y=daily_cases), color = "blue3") +
  geom_line(aes(y=National*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Daily reported infections (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

coeff = 240000

pp2 <-
  ggplot(bigfile_nwss, aes(x=date)) +
  geom_line(aes(y=inf_mean), color = "blue3") +
  geom_line(aes(y=National*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Daily estimated infections (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

# look for WW data for other 26 countries
# check with IHME whether they used WW data as model input
# Research letter -
# Big takeaway - IHME estimations suggest really strong correl with WW so if ID is 15-ish
# reported to WW ratio
# estimated to WW ratio
# reported to estimated == IDR
# where does 2-19x reported by Maria come from?
# Get in touch with Laura Choi regarding finding WW data
# we want to go from WW to estimated infections
# check study protocol to see if analysis is in scope
# look by subvariant


# correlation overall and by strain
#
# look at correlations (rolling)
# MAD mean absolute deviation
# find a good reference in mmwr? about use of home testing
# can we find at home test use dataset?
# https://data.cdc.gov/Public-Health-Surveillance/U-S-COVID-19-Self-Test-Data/275g-9x8h/about_data

# Export to CSV without row names
write.csv(bigfile_nwss,
          file = paste0(getwd(), "/output/" ,"bigfile_nwss_", 
                                     paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE,
          na = "",
          row.names = FALSE)
