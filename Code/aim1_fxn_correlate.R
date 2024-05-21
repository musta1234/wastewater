
# Calculate correlation matrices by group in R using the tidyverse package

library(tidyverse)

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


usa_variant_cor <-  
  fxn_correlations(usa_dataset, variant) %>% 
  mutate(location_name = "United States of America") %>%
  rename( var = 1)

usa_subvariant_cor <-  
  fxn_correlations(usa_dataset, subvariant) %>% 
  mutate(location_name = "United States of America") %>%
  rename(var = 1) 

neth_variant_cor <-  
  fxn_correlations(neth_dataset, variant) %>% 
  mutate(location_name = "Netherlands") %>%
  rename( var = 1)

neth_subvariant_cor <-  
  fxn_correlations(neth_dataset, subvariant) %>% 
  mutate(location_name = "Netherlands") %>%
  rename( var = 1)

den_variant_cor <-  
  fxn_correlations(den_dataset, variant) %>% 
  mutate(location_name = "Denmark") %>%
  rename( var = 1)

den_subvariant_cor <-  
  fxn_correlations(den_dataset, subvariant) %>%
  mutate(location_name = "Denmark") %>%
  rename( var = 1)

usa_cor <-  
  usa_dataset %>% select_if(is.numeric) %>% correlate() %>% 
  mutate(location_name = "United States of America") %>%
  rename( var = 1)

neth_cor <-  
  usa_dataset %>% select_if(is.numeric) %>% correlate() %>% 
  mutate(location_name = "Netherlands") %>%
  rename( var = 1)

den_cor <-  
  usa_dataset %>% select_if(is.numeric) %>% correlate() %>% 
  mutate(location_name = "Denmark") %>%
  rename( var = 1)

# Combine correlation matrices into a single data frame
dataframes <- list(
  usa_cor = usa_cor,
  neth_cor = neth_cor,
  den_cor = den_cor,
  usa_variant_cor = usa_variant_cor,
  usa_subvariant_cor = usa_subvariant_cor,
  neth_variant_cor = neth_variant_cor,
  neth_subvariant_cor = neth_subvariant_cor,
  den_variant_cor = den_variant_cor,
  den_subvariant_cor = den_subvariant_cor
)

lapply(names(dataframes), function(name) {
  write.csv(dataframes[[name]], 
            file = paste0(getwd(), "/output/cor/" ,paste0(name), 
                          paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
            quote = TRUE,
            na = "",
            row.names = FALSE)
})

