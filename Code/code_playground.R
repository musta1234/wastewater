

nums <- sort(sample.int(nrow(ic_long), 5000, replace = FALSE))

ic_lite <- ic_long[nums, ] 

# Using dplyr
library(dplyr)
x <-
  %>% 
  merge(x=ihme_stack, y=ic_wide,
        by.x = c('ihme_Date', 'ihme_country_code'),
        by.y = c('ic_date', 
                 'ihme_country_code' = 'iso3c'
        )) %>% View()

inner_join(x=ihme_stack, y=ic_long,
           by = c('ihme_Date' = 'ic_date',
                  'ihme_country_code' = 'ic_iso3c'
           )) %>% View()

full_join(x=ihme_stack, y=ic_wide,
          by = c('ihme_Date' = 'ic_date',
                 'ihme_country_code' = 'ic_iso3c'
          )) %>% nrow()
inner_join(x=ihme_stack, y=ic_wide,
           by = c('ihme_Date' = 'ic_date',
                  'ihme_country_code' = 'ic_iso3c'
           )) %>% View()

right_join(x=ihme_stack, y=ic_wide,
           by = c('ihme_Date' = 'ic_date',
                  'ihme_country_code' = 'ic_iso3c'
           )) %>% nrow()
left_join(x=ihme_stack, y=ic_wide,
          by = c('ihme_Date' = 'ic_date',
                 'ihme_country_code' = 'ic_iso3c'
          )) %>% nrow()


ic_long %>%
  pivot_wider(id_cols = c("ic_date", "ic_country", "ic_iso3c"),
              names_from = c("ic_compartment", "ic_fit_type"),
              values_from = c("ic_y_mean", "ic_y_025", "ic_y_975")
  ) %>% View()
a <- ihme_stack[["ihme_country_code"]] %>% sort() %>% unique() 
b <- ic_long[["ic_iso3c"]] %>% sort() %>% unique()
c <- intersect(a, b); c
length(a); length(b); length(c)
(c %in% b) %>% table()

ic_long %>% spread(c(ic_date, ic_iso3c, ic_compartment, ic_fit_type), ic_y_mean ) %>% View()
ic_deaths <- 
  ic_long %>% 
  filter(ic_compartment == "deaths") %>%
  rename("ic_y_mean_deaths" = "ic_y_mean",
         "ic_y_025_deaths" = "ic_y_025",
         "ic_y_975_deaths" = "ic_y_975") %>%
  select(-ic_compartment)

ic_infections <- 
  ic_long %>% 
  filter(ic_compartment == "infections") %>%
  rename("ic_y_mean_inf" = "ic_y_mean",
         "ic_y_025_inf" = "ic_y_025",
         "ic_y_975_inf" = "ic_y_975") %>%
  select(-ic_compartment)


common_cols <- intersect(names(ic_infections), 
                         names(ic_deaths))

merge(ic_infections, ic_deaths, 
      by = common_cols,
      all.x = TRUE, all.y = TRUE) %>%
  View()


pivot_wider(names_from = c("ic_compartment"),
            values_from = c("ic_y_mean")) %>% 
  View()

library(dplyr)
library(tidyr)
library(stringr)
df %>%
  mutate(time = str_c("time", time)) %>%
  pivot_wider(names_from = time, values_from = c("x", "y"), names_sep="")


[1] "ic_date"             "ic_compartment"      "ic_y_mean"           "ic_fit_type"        
[5] "ic_death_calibrated" "ic_country"          "ic_iso3c" 

#########################################




```{r summarize_ihme, echo=TRUE}

summary_table <- function(df) {
  # Group the data frame by location
  df_grouped <- df %>% 
    group_by(location) %>% 
    # Calculate the number of missing observations and zeros for each column
    
    summarise_all(
      list(
        n_missing = ~sum(is.na(.)),
        n_zeros = ~sum(. == 0)
      )
    )
  
  # Return the summary table
  return(df_grouped)
}


summary_table_ihme <- summary_table(data_stack)
write.csv(summary_table_ihme, file = "./output/summary_table_ihme.csv", row.names = FALSE, quote = TRUE)

write.csv(data_stack, file = "./data_ihme.csv", row.names = FALSE, quote = TRUE)

```





##Calculate underreporting as a percentage
```{r under_report, echo=TRUE}
#calculate under_reports by country and drop missing values
data_under <- 
  data_stack %>%
  # Create a column for the 7 day moving average cases/inf by location using zoo::rollapplyr
  group_by(location_name) %>%
  #drop weeks with 0 predicted cases, mostly from March and April 2020 in some islands
  filter(inf_mean_7d != 0 & !is.na(inf_mean_7d) ) %>% 
  ungroup() %>%
  # Calculate column C = A/B only if A and B are both non-missing and not zero
  # denominator is 7d average
  mutate(under_report_7d = daily_cases_7d/inf_mean_7d) %>% 
  # denominator is not averaged
  mutate(under_report = daily_cases_7d/inf_mean) %>%
  # convert date column to year-quarter format
  mutate(year_quarter= paste0(year(date), "Q", quarter(date)))
#hist(., freq=TRUE, breaks = 1000)

#Add region designation
data_regions <- 
  left_join(data_under, regions, by="location_name") %>% 
  #keep only countries in the 'regions' dataset
  filter(!is.na(region)) 

```




```{r summarize_ihme, echo=TRUE}
stats_country_ranks <- 
  data_regions %>% group_by(location_name) %>%
  summarize(
    na_daily_cases = sum(is.na(daily_cases)),
    zero_daily_cases = sum(daily_cases==0, na.rm = TRUE)
  ) %>% 
  mutate(nazero_daily_cases = na_daily_cases - 110 + zero_daily_cases) %>%
  #arrange by descending missingness score, nazero_daily_cases
  arrange(desc(nazero_daily_cases)) %>%
  mutate(completeness_rank = rank(nazero_daily_cases, ties.method = "random"))

#generate summary stats by one or two grouping variables
summarize_ihme <- function(ihme_data, var1, var2 = ""){
  ihme_data %>%
    filter(!is.na(daily_cases_7d)) %>% 
    group_by({{var1}}, {{var2}}) %>% 
    summarize(
      n = sum(!is.na(under_report)),
      min_date = min(as.Date(date), na.rm = TRUE),
      max_date = max(as.Date(date), na.rm = TRUE),
      mean = mean(under_report, na.rm = TRUE),
      max = max(under_report, na.rm = TRUE),
      min = min(under_report, na.rm = TRUE),
      median = median(under_report, na.rm = TRUE),
      upper_quartile = quantile(under_report, 0.75,na.rm = TRUE),
      lower_quartile = quantile(under_report, 0.25, na.rm = TRUE)
    )
}

# calculate summary stats for region and country and by quarter
stats_region <- summarize_ihme(data_regions, region)
stats_loc <- summarize_ihme(data_regions, location_name)
stats_loc_qtr <- summarize_ihme(data_regions, location_name, year_quarter)
stats_region_qtr <- summarize_ihme(data_regions, region, year_quarter)

#export summary stats
for (name in c("stats_region", "stats_loc", "stats_loc_qtr", "stats_region_qtr", "stats_country_ranks")){
  df <- get(name)
  write.xlsx(df, file = paste0(name, ".xlsx"), rowNames = FALSE, colNames = TRUE)
}

```

##Export country level and regional data for excel
```{r under_report, echo=TRUE}
#save big file
write.xlsx(data_regions, 
           file = paste0("./output/" ,"bigfile", ".xlsx"),
           colNames = TRUE,
           rowNames = FALSE)

places <- sort(as.vector(data_regions$location_name)) %>% unique()
# write excel files for each country
for (place in places){
  data_regions %>% 
    filter(location_name == place) %>% 
    write.xlsx(., 
               file = paste0("./output/" ,place, ".xlsx"),
               colNames = TRUE,
               rowNames = FALSE)
}

# write excel files for each continent
##Need to summarize by day first
for (continent in 
     unique(as.vector(data_regions$region))){
  data_regions %>% 
    filter(region == continent) %>% 
    write.xlsx(., 
               file = paste0("./output/" ,continent, ".xlsx"),
               colNames = TRUE,
               rowNames = FALSE)
}


```
## Make line plots and barcharts
```{r plots, echo=TRUE}
#1 Create line plots for each country and #2) save as pdfs
# create plot
df <- data_regions %>% filter(location_name == "United States of America")
ggplot(df, 
       aes(year_quarter, under_report)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



geom_line() +
  labs(x = "Date", y = "Under reports for USA") +
  ggtitle("Number of cases over time")

ggplot(df, 
       aes(x = date, y = under_report)) + 
  geom_line() +
  labs(x = "Date", y = "Under reports for USA") +
  ggtitle("Number of cases over time")


```


##Older code
```{r backup, echo=TRUE}
# View the imported data
View(country_groups)

# check overlaps
# need a clean list of countries by region using world bank format
# a clean list of US states
#All location names
location_vec <- data_under[["location_name"]] %>% unique()

#All countries from World Bank
world <- 
  country_groups %>% 
  filter(GroupName == "World") %>%
  select(CountryName) %>%
  as.vector() %>%
  unlist()

# How many countries are correctly spelt? 
length(location_vec)
intersect(world, location_vec) %>% length() #145

# A list of poor spellings and countries that do not have a model
world_mismatch <- 
  setdiff(world, location_vec) %>% sort() %>% as.data.frame()

location_mismatch <- 
  setdiff(location_vec, world) %>% sort() %>% as.data.frame()

#remove US states
location_mismatch2 <- setdiff(location_mismatch[[1]], as.data.frame(state.name)[[1]]) %>% as.data.frame()


#####################
length(state.name)

setdiff(as.data.frame(state.name)[[1]], location_mismatch[[1]])

```

```{r location_clean, echo=TRUE}
# need to make sure Georgia, USA is not mixed up with Republic of Georgia
data_under %>% 
  #filter(grepl('Macao', location_name)) %>% 
  select(location_name) %>% 
  #View() %>%
  #remove everything in parentheses
  mutate(location_clean = gsub("\\ \\(.*\\)", "", location_name)) %>%
  mutate(location_clean = gsub("^Macao.*", "Macao", location_clean)) %>%
  select(location_clean) %>% table() %>% View()
```
## Group IHME data by location and calculate summary statistics
```{r summary_table}
# 1) for each location summarize Max date, min date, mean, median, max, min, upper and lower quartile
summary_table <- data_under %>% 
  filter(!is.na(inf_mean))
group_by(location_name) %>%
  summarize(
    n = sum(!is.na(inf_mean)),
    min_date = min(as.Date(date), na.rm = TRUE),
    max_date = max(as.Date(date), na.rm = TRUE),
    mean = mean(under_report, na.rm = TRUE),
    max = max(under_report, na.rm = TRUE),
    min = min(under_report, na.rm = TRUE),
    median = median(under_report, na.rm = TRUE),
    upper_quartile = quantile(under_report, 0.75,na.rm = TRUE),
    lower_quartile = quantile(under_report, 0.25, na.rm = TRUE)
  )

# View the resulting summary table
View(summary_table)
```

## Calculate breakpoints for time series data
```{r summary_table}
#To conduct statistical methods detection of breakpoints in time series data using Chow test, CUSUM test, and Pettitt's test, you can use the strucchange package in R. Here's an example code for each of the three methods:

#Chow test:
library(strucchange)
# create a time series object
ts_data <- ts(your_data, start = start_year, frequency = 12)

# perform Chow test
chow_test <- breakpoints(ts_data ~ 1)
summary(chow_test)

#CUSUM test:
# perform CUSUM test
cusum_test <- sctest(ts_data ~ 1, type = "CUSUM")
summary(cusum_test)

#Pettitt's test:
# perform Pettitt's test
pettitt_test <- sctest(ts_data ~ 1, type = "Pettitt")
summary(pettitt_test)
#Note that in the above code, your_data should be replaced with the name of your time series data, and start_year should be replaced with the start year of your time series. Also, you can adjust the frequency argument depending on the frequency of your time series (12 if it is monthly, 4 if it is quarterly, etc.).

```

## Including Plots



You can also embed plots, for example:
  
  ```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
  
  
  
  nums <- sort(sample.int(nrow(ic_long), 5000, replace = FALSE))

ic_lite <- ic_long[nums, ] 

# Using dplyr
library(dplyr)
x <-
  %>% 
  merge(x=ihme_stack, y=ic_wide,
        by.x = c('ihme_Date', 'ihme_country_code'),
        by.y = c('ic_date', 
                 'ihme_country_code' = 'iso3c'
        )) %>% View()

inner_join(x=ihme_stack, y=ic_long,
           by = c('ihme_Date' = 'ic_date',
                  'ihme_country_code' = 'ic_iso3c'
           )) %>% View()

full_join(x=ihme_stack, y=ic_wide,
          by = c('ihme_Date' = 'ic_date',
                 'ihme_country_code' = 'ic_iso3c'
          )) %>% nrow()
inner_join(x=ihme_stack, y=ic_wide,
           by = c('ihme_Date' = 'ic_date',
                  'ihme_country_code' = 'ic_iso3c'
           )) %>% View()

right_join(x=ihme_stack, y=ic_wide,
           by = c('ihme_Date' = 'ic_date',
                  'ihme_country_code' = 'ic_iso3c'
           )) %>% nrow()
left_join(x=ihme_stack, y=ic_wide,
          by = c('ihme_Date' = 'ic_date',
                 'ihme_country_code' = 'ic_iso3c'
          )) %>% nrow()


ic_long %>%
  pivot_wider(id_cols = c("ic_date", "ic_country", "ic_iso3c"),
              names_from = c("ic_compartment", "ic_fit_type"),
              values_from = c("ic_y_mean", "ic_y_025", "ic_y_975")
  ) %>% View()
a <- ihme_stack[["ihme_country_code"]] %>% sort() %>% unique() 
b <- ic_long[["ic_iso3c"]] %>% sort() %>% unique()
c <- intersect(a, b); c
length(a); length(b); length(c)
(c %in% b) %>% table()

ic_long %>% spread(c(ic_date, ic_iso3c, ic_compartment, ic_fit_type), ic_y_mean ) %>% View()
ic_deaths <- 
  ic_long %>% 
  filter(ic_compartment == "deaths") %>%
  rename("ic_y_mean_deaths" = "ic_y_mean",
         "ic_y_025_deaths" = "ic_y_025",
         "ic_y_975_deaths" = "ic_y_975") %>%
  select(-ic_compartment)

ic_infections <- 
  ic_long %>% 
  filter(ic_compartment == "infections") %>%
  rename("ic_y_mean_inf" = "ic_y_mean",
         "ic_y_025_inf" = "ic_y_025",
         "ic_y_975_inf" = "ic_y_975") %>%
  select(-ic_compartment)


common_cols <- intersect(names(ic_infections), 
                         names(ic_deaths))

merge(ic_infections, ic_deaths, 
      by = common_cols,
      all.x = TRUE, all.y = TRUE) %>%
  View()


pivot_wider(names_from = c("ic_compartment"),
            values_from = c("ic_y_mean")) %>% 
  View()

library(dplyr)
library(tidyr)
library(stringr)
df %>%
  mutate(time = str_c("time", time)) %>%
  pivot_wider(names_from = time, values_from = c("x", "y"), names_sep="")


[1] "ic_date"             "ic_compartment"      "ic_y_mean"           "ic_fit_type"        
[5] "ic_death_calibrated" "ic_country"          "ic_iso3c" 

#########################################




```{r summarize_ihme, echo=TRUE}

summary_table <- function(df) {
  # Group the data frame by location
  df_grouped <- df %>% 
    group_by(location) %>% 
    # Calculate the number of missing observations and zeros for each column
    
    summarise_all(
      list(
        n_missing = ~sum(is.na(.)),
        n_zeros = ~sum(. == 0)
      )
    )
  
  # Return the summary table
  return(df_grouped)
}


summary_table_ihme <- summary_table(data_stack)
write.csv(summary_table_ihme, file = "./output/summary_table_ihme.csv", row.names = FALSE, quote = TRUE)

write.csv(data_stack, file = "./data_ihme.csv", row.names = FALSE, quote = TRUE)

```





##Calculate underreporting as a percentage
```{r under_report, echo=TRUE}
#calculate under_reports by country and drop missing values
data_under <- 
  data_stack %>%
  # Create a column for the 7 day moving average cases/inf by location using zoo::rollapplyr
  group_by(location_name) %>%
  #drop weeks with 0 predicted cases, mostly from March and April 2020 in some islands
  filter(inf_mean_7d != 0 & !is.na(inf_mean_7d) ) %>% 
  ungroup() %>%
  # Calculate column C = A/B only if A and B are both non-missing and not zero
  # denominator is 7d average
  mutate(under_report_7d = daily_cases_7d/inf_mean_7d) %>% 
  # denominator is not averaged
  mutate(under_report = daily_cases_7d/inf_mean) %>%
  # convert date column to year-quarter format
  mutate(year_quarter= paste0(year(date), "Q", quarter(date)))
#hist(., freq=TRUE, breaks = 1000)

#Add region designation
data_regions <- 
  left_join(data_under, regions, by="location_name") %>% 
  #keep only countries in the 'regions' dataset
  filter(!is.na(region)) 

```




```{r summarize_ihme, echo=TRUE}
stats_country_ranks <- 
  data_regions %>% group_by(location_name) %>%
  summarize(
    na_daily_cases = sum(is.na(daily_cases)),
    zero_daily_cases = sum(daily_cases==0, na.rm = TRUE)
  ) %>% 
  mutate(nazero_daily_cases = na_daily_cases - 110 + zero_daily_cases) %>%
  #arrange by descending missingness score, nazero_daily_cases
  arrange(desc(nazero_daily_cases)) %>%
  mutate(completeness_rank = rank(nazero_daily_cases, ties.method = "random"))

#generate summary stats by one or two grouping variables
summarize_ihme <- function(ihme_data, var1, var2 = ""){
  ihme_data %>%
    filter(!is.na(daily_cases_7d)) %>% 
    group_by({{var1}}, {{var2}}) %>% 
    summarize(
      n = sum(!is.na(under_report)),
      min_date = min(as.Date(date), na.rm = TRUE),
      max_date = max(as.Date(date), na.rm = TRUE),
      mean = mean(under_report, na.rm = TRUE),
      max = max(under_report, na.rm = TRUE),
      min = min(under_report, na.rm = TRUE),
      median = median(under_report, na.rm = TRUE),
      upper_quartile = quantile(under_report, 0.75,na.rm = TRUE),
      lower_quartile = quantile(under_report, 0.25, na.rm = TRUE)
    )
}

# calculate summary stats for region and country and by quarter
stats_region <- summarize_ihme(data_regions, region)
stats_loc <- summarize_ihme(data_regions, location_name)
stats_loc_qtr <- summarize_ihme(data_regions, location_name, year_quarter)
stats_region_qtr <- summarize_ihme(data_regions, region, year_quarter)

#export summary stats
for (name in c("stats_region", "stats_loc", "stats_loc_qtr", "stats_region_qtr", "stats_country_ranks")){
  df <- get(name)
  write.xlsx(df, file = paste0(name, ".xlsx"), rowNames = FALSE, colNames = TRUE)
}

```

##Export country level and regional data for excel
```{r under_report, echo=TRUE}
#save big file
write.xlsx(data_regions, 
           file = paste0("./output/" ,"bigfile", ".xlsx"),
           colNames = TRUE,
           rowNames = FALSE)

places <- sort(as.vector(data_regions$location_name)) %>% unique()
# write excel files for each country
for (place in places){
  data_regions %>% 
    filter(location_name == place) %>% 
    write.xlsx(., 
               file = paste0("./output/" ,place, ".xlsx"),
               colNames = TRUE,
               rowNames = FALSE)
}

# write excel files for each continent
##Need to summarize by day first
for (continent in 
     unique(as.vector(data_regions$region))){
  data_regions %>% 
    filter(region == continent) %>% 
    write.xlsx(., 
               file = paste0("./output/" ,continent, ".xlsx"),
               colNames = TRUE,
               rowNames = FALSE)
}


```
## Make line plots and barcharts
```{r plots, echo=TRUE}
#1 Create line plots for each country and #2) save as pdfs
# create plot
df <- data_regions %>% filter(location_name == "United States of America")
ggplot(df, 
       aes(year_quarter, under_report)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



geom_line() +
  labs(x = "Date", y = "Under reports for USA") +
  ggtitle("Number of cases over time")

ggplot(df, 
       aes(x = date, y = under_report)) + 
  geom_line() +
  labs(x = "Date", y = "Under reports for USA") +
  ggtitle("Number of cases over time")


```


##Older code
```{r backup, echo=TRUE}
# View the imported data
View(country_groups)

# check overlaps
# need a clean list of countries by region using world bank format
# a clean list of US states
#All location names
location_vec <- data_under[["location_name"]] %>% unique()

#All countries from World Bank
world <- 
  country_groups %>% 
  filter(GroupName == "World") %>%
  select(CountryName) %>%
  as.vector() %>%
  unlist()

# How many countries are correctly spelt? 
length(location_vec)
intersect(world, location_vec) %>% length() #145

# A list of poor spellings and countries that do not have a model
world_mismatch <- 
  setdiff(world, location_vec) %>% sort() %>% as.data.frame()

location_mismatch <- 
  setdiff(location_vec, world) %>% sort() %>% as.data.frame()

#remove US states
location_mismatch2 <- setdiff(location_mismatch[[1]], as.data.frame(state.name)[[1]]) %>% as.data.frame()


#####################
length(state.name)

setdiff(as.data.frame(state.name)[[1]], location_mismatch[[1]])

```

```{r location_clean, echo=TRUE}
# need to make sure Georgia, USA is not mixed up with Republic of Georgia
data_under %>% 
  #filter(grepl('Macao', location_name)) %>% 
  select(location_name) %>% 
  #View() %>%
  #remove everything in parentheses
  mutate(location_clean = gsub("\\ \\(.*\\)", "", location_name)) %>%
  mutate(location_clean = gsub("^Macao.*", "Macao", location_clean)) %>%
  select(location_clean) %>% table() %>% View()
```
## Group IHME data by location and calculate summary statistics
```{r summary_table}
# 1) for each location summarize Max date, min date, mean, median, max, min, upper and lower quartile
summary_table <- data_under %>% 
  filter(!is.na(inf_mean))
group_by(location_name) %>%
  summarize(
    n = sum(!is.na(inf_mean)),
    min_date = min(as.Date(date), na.rm = TRUE),
    max_date = max(as.Date(date), na.rm = TRUE),
    mean = mean(under_report, na.rm = TRUE),
    max = max(under_report, na.rm = TRUE),
    min = min(under_report, na.rm = TRUE),
    median = median(under_report, na.rm = TRUE),
    upper_quartile = quantile(under_report, 0.75,na.rm = TRUE),
    lower_quartile = quantile(under_report, 0.25, na.rm = TRUE)
  )

# View the resulting summary table
View(summary_table)
```

## Calculate breakpoints for time series data
```{r summary_table}
#To conduct statistical methods detection of breakpoints in time series data using Chow test, CUSUM test, and Pettitt's test, you can use the strucchange package in R. Here's an example code for each of the three methods:

#Chow test:
library(strucchange)
# create a time series object
ts_data <- ts(your_data, start = start_year, frequency = 12)

# perform Chow test
chow_test <- breakpoints(ts_data ~ 1)
summary(chow_test)

#CUSUM test:
# perform CUSUM test
cusum_test <- sctest(ts_data ~ 1, type = "CUSUM")
summary(cusum_test)

#Pettitt's test:
# perform Pettitt's test
pettitt_test <- sctest(ts_data ~ 1, type = "Pettitt")
summary(pettitt_test)
#Note that in the above code, your_data should be replaced with the name of your time series data, and start_year should be replaced with the start year of your time series. Also, you can adjust the frequency argument depending on the frequency of your time series (12 if it is monthly, 4 if it is quarterly, etc.).

```

## Including Plots



You can also embed plots, for example:
  
  ```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this: