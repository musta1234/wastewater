# predominance periods
#US BQ.1 begins Nov 21 2022
#US XBB start January 16, 2023
#US JN.1 start December 18, 2023

#Den BA45/ends BQ.1 begins Nov 21 2022
#Den BQ.1 never reached 4 weeks prevalence
#Den XBB start Feb 13 16, 2023
#Den JN.1 start November 20, 2023

#check and confirm if how start/end dates cut off was done in previous code

#ba1 = case_when(
#  ((date >= start_BA.1) & (date < end_BA.1)) ~ 1, .default = 0),
#- lag analyses
#- correlations
#- revisit covariants
#- min, max, lag, correlations 
#- US Biobot analysis (October 2024)
#charts but with infections (reported/estimated)

# day of the week
# Within the scope of MSC proposal see if any other countries now qualify
# check if a log scale would make sense for Denmark





#### This section creates plot comparing New and Weekly hospitalizations in the US
# write code to import the csv file "C:\Users\mustam23\OneDrive - Pfizer\Documents\Projects\Data\OWID\covid-hospitalizations.csv"
mycsv <-
  read.csv("C:/Users/mustam23/OneDrive - Pfizer/Documents/Projects/Data/OWID/covid-hospitalizations.csv")
head(mycsv)

mycsv2 <- 
  mycsv %>%
  mutate(date = as.Date(date), 
         indicator = gsub(" ", "_" , indicator)) %>%
  filter(entity == "United States") %>%
  select(date, indicator, value) %>%
  spread(indicator, value) %>%
  select(date, Weekly_new_hospital_admissions, Daily_hospital_occupancy) %>% 
  arrange(date) #%>% View()


x <-
  ggplot(
    mycsv2, 
    aes(x=date)) +
  geom_line(aes(y=Weekly_new_hospital_admissions), color = "blue3") +
  geom_line(aes(y=Daily_hospital_occupancy), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Weekly new hospital admissions (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Daily hospital occupancy (red)", labels = comma),
    labels = comma
  )

x

#### This code determines the earliest and laltest date for each numeric variable in the dataset


earliest_latest_dates <- function(df){ 
  df %>% arrange(date) %>%
  summarise(across(where(is.numeric), list(
    earliest_date = ~ date[which.min(.)],
    latest_date = ~ date[which.max(.)]
  )))
}

print(earliest_latest_dates(usa_dataset))

usa_dates <- earliest_latest_dates(usa_dataset)

usa_earliest_latest <-
 data.frame(vars = names(usa_dates), 
           date = (usa_dates %>% unlist()) %>% as.Date()
           ) %>%
  mutate(
    earliest_latest = case_when(
      grepl("earliest", vars) ~ "Earliest",
      grepl("latest", vars) ~ "Latest"
    ),
    varname = gsub("_earliest_date|_latest_date", "", vars)
    
  ) %>% 
  select(-vars) %>%
  pivot_wider(names_from = earliest_latest, values_from = date) %>%
  mutate(location_name = "United States")



den_dates <- earliest_latest_dates(ww_dk_hosp)

dk_earliest_latest <-
  data.frame(vars = names(den_dates), 
             date = (den_dates %>% unlist()) %>% as.Date()
  ) %>%
  mutate(
    earliest_latest = case_when(
      grepl("earliest", vars) ~ "Earliest",
      grepl("latest", vars) ~ "Latest"
    ),
    varname = gsub("_earliest_date|_latest_date", "", vars)
    
  ) %>% 
  select(-vars) %>%
  pivot_wider(names_from = earliest_latest, values_from = date) %>%
  mutate(location_name = "Denmark")

afile <- rbind(usa_earliest_latest, dk_earliest_latest) 

#export afile to csv

write.csv(afile, 
          paste0(file_loc,"/output/", "earliest_latest_dates.csv")
          , row.names = FALSE)

usa_cor <- 
  usa_dataset %>% select(is.numeric) %>% correlate()

den_cor <- 
  ww_dk_hosp %>% select(is.numeric) %>% correlate()

write.csv(usa_cor, 
          paste0(file_loc,"/output/", "usa_cor_", 
                 paste0(as.character(Sys.Date()), ".csv")),
          row.names = FALSE)

write.csv(den_cor, 
          paste0(file_loc,"/output/", "den_cor_", 
                 paste0(as.character(Sys.Date()), ".csv")),
          row.names = FALSE)

#make ggplot charts for the following variables
##USA
#viral_load vs (inf_mean_7dt, daily_cases_7dt, hosp_current, hosp_new)

##Denmark
#viral_load vs (hosp_new_wkly, cases, inf_mean_7dt, daily_cases_7dt)


