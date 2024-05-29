
# Calculate correlation matrices by group in R using the tidyverse package

library(tidyverse)

usa_leadlag <-  
  usa_dataset %>% 
  mutate(
    log_viral_load = log(viral_load),
    lead1_viral_load = lead(viral_load, n = 1),
    lead2_viral_load = lead(viral_load, n = 2),
    lead3_viral_load = lead(viral_load, n = 3),
    lead4_viral_load = lead(viral_load, n = 4),
    lag1_viral_load = lag(viral_load, n = 1),
    lag2_viral_load = lag(viral_load, n = 2),
    lag3_viral_load = lag(viral_load, n = 3),
    lag4_viral_load = lag(viral_load, n = 4)
    ) %>%
  select_if(is.numeric) %>% correlate() %>% 
  mutate(location_name = "United States of America") %>%
  rename( var = 1)

neth_leadlag <-  
  neth_dataset %>% 
  mutate(
    log_viral_load = log(viral_load),
    lead1_viral_load = lead(viral_load, n = 1),
    lead2_viral_load = lead(viral_load, n = 2),
    lead3_viral_load = lead(viral_load, n = 3),
    lead4_viral_load = lead(viral_load, n = 4),
    lag1_viral_load = lag(viral_load, n = 1),
    lag2_viral_load = lag(viral_load, n = 2),
    lag3_viral_load = lag(viral_load, n = 3),
    lag4_viral_load = lag(viral_load, n = 4)
  ) %>%
  select_if(is.numeric) %>% correlate() %>% 
  mutate(location_name = "Netherlands") %>%
  rename( var = 1)

den_leadlag <-  
  den_dataset %>% 
  mutate(
    log_viral_load = log(viral_load),
    lead1_viral_load = lead(viral_load, n = 1),
    lead2_viral_load = lead(viral_load, n = 2),
    lead3_viral_load = lead(viral_load, n = 3),
    lead4_viral_load = lead(viral_load, n = 4),
    lag1_viral_load = lag(viral_load, n = 1),
    lag2_viral_load = lag(viral_load, n = 2),
    lag3_viral_load = lag(viral_load, n = 3),
    lag4_viral_load = lag(viral_load, n = 4)
  ) %>%
  select_if(is.numeric) %>% correlate() %>% 
  mutate(location_name = "Denmark") %>%
  rename( var = 1)

# Combine correlation matrices into a single data frame
dfs <- list(
  usa_leadlag = usa_leadlag,
  neth_leadlag = neth_leadlag,
  den_leadlag = den_leadlag
  )
  
lapply(names(dfs), function(name) {
  write.csv(dfs[[name]], 
            file = paste0(getwd(), "/output/leadlag/" ,paste0(name), 
                          paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
            quote = TRUE,
            na = "",
            row.names = FALSE)
})



############Rolling correlation###############################################


# Define a function to calculate rolling correlation
calculate_rolling_correlation <- function(x, y, width) {
  rollapply(
    data.frame(x, y), 
    width = width, 
    FUN = function(z) cor(z[, 1], z[, 2], use = "complete.obs"), 
    by.column = FALSE, 
    align = "right",
    fill = NA
  )
}


# Function to calculate correlation matrix for each group
calc_cor <- function(df) {
  cor(df %>% select_if(is.numeric), use = "pairwise.complete.obs")
}

# Calculate correlation matrices by group and store in a list

fxn_correlations <-
  function(data, group_var) {
    data %>%
      group_by({{ group_var }}) %>%
      nest() %>%
      mutate(correlation = map(data, calc_cor)) %>%
      select({{ group_var }}, correlation) %>%
      mutate(correlation = map(correlation, as.data.frame)) %>%
      unnest(correlation) %>%
      filter({{ group_var }} != "" & !is.na({{ group_var }})) %>%
      mutate(my_var = names(data %>% select_if(is.numeric))) %>%
      relocate(., my_var, .after = {{ group_var }})
    
  }
# Add rolling correlation as a new column
#sample_data <- sample_data %>%
#  mutate(RollingCorrelation = calculate_rolling_correlation(Var1, Var2, 20))
calculate_rolling_correlation <- function(x, y, window_size) {
  rollapply(data.frame(x, y), width = window_size, 
            FUN = function(z) correlate(z[, 1], z[, 2]), by.column = FALSE, align = "right")
}

df_roll_corr <-
  usa_dataset %>% 
  filter(!is.na(viral_load)) %>% 
  select(date, viral_load, hosp_new) %>% 
  mutate(roll_corr_10 = calculate_rolling_correlation(viral_load, hosp_new, 10))

# Assuming your dataframe is named `your_dataframe`
ggplot(df_roll_corr, aes(x = date, y = roll_corr_10)) +
  geom_line() +
  labs(title = "Line Chart of Frequency over Time",
       x = "Date",
       y = "Frequency") +
  theme_minimal()





