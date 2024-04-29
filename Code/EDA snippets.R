
#summarize variables in each dataset
head(ww_dk_import)


  ww_dk_import %>%
  summarize(min_date = min(date), 
            max_date = max(date), 
            min_viral_load = min(viral_load, na.rm = TRUE),
            max_viral_load = max(viral_load, na.rm = TRUE),
            mean_viral_load = mean(viral_load, na.rm = TRUE),
            median_viral_load = median(viral_load, na.rm = TRUE),
            Q1_viral_load = quantile(viral_load, 0.25, na.rm = TRUE),
            Q3_viral_load = quantile(viral_load, 0.75, na.rm = TRUE),
  ) %>% View()
  
  head(hosp_den_import)
  
  hosp_den_import %>%
    summarize(min_date = min(date), 
              max_date = max(date), 
              min_vhosp_new = min(hosp_new, na.rm = TRUE),
              max_hosp_new = max(hosp_new, na.rm = TRUE),
              mean_hosp_new = mean(hosp_new, na.rm = TRUE),
              median_hosp_new = median(hosp_new, na.rm = TRUE),
              Q1_hosp_new = quantile(hosp_new, 0.25, na.rm = TRUE),
              Q3_hosp_new = quantile(hosp_new, 0.75, na.rm = TRUE),
    ) %>% View()
              

   
  #convert the code below into a function
  #function to find the minimum and maximum for all numeric variables in a dataset
  min_max_n <-
    function(data){
      data %>%
        select_if(is.numeric) %>%
        summarize_all(list(min = min, max = max))
    }
  
  min_max_n(ww_dk_import)
  
      data %>%
        summarize(min_date = min(date), 
                  max_date = max(date), 
                  max_viral_load = max(viral_load, na.rm = TRUE),
                  min_viral_load = min(viral_load, na.rm = TRUE),
                  count = n())
    }
    ww_dk_import %>%
      
      
    head(ww_usa_import)
    

    ww_usa_import %>%
      summarize(min_date = min(date), 
                max_date = max(date), 
                min_viral_load = min(viral_load, na.rm = TRUE),
                max_viral_load = max(viral_load, na.rm = TRUE),
                mean_viral_load = mean(viral_load, na.rm = TRUE),
                median_viral_load = median(viral_load, na.rm = TRUE),
                Q1_viral_load = quantile(viral_load, 0.25, na.rm = TRUE),
                Q3_viral_load = quantile(viral_load, 0.75, na.rm = TRUE),
      ) %>% View()
      
      head(hosp_usa_import_new)
      
      hosp_usa_import_new %>%
        summarize(min_date = min(date), 
                  max_date = max(date), 
                  min_hosp_new = min(hosp_new, na.rm = TRUE),
                  max_hosp_new = max(hosp_new, na.rm = TRUE),
                  mean_hosp_new = mean(hosp_new, na.rm = TRUE),
                  median_hosp_new = median(hosp_new, na.rm = TRUE),
                  Q1_hosp_new = quantile(hosp_new, 0.25, na.rm = TRUE),
                  Q3_hosp_new = quantile(hosp_new, 0.75, na.rm = TRUE),
        ) %>% View()
      
      head(hosp2_usa_import_current)
      
      hosp2_usa_import_current %>%
        summarize(min_date = min(date), 
                  max_date = max(date), 
                  min_hosp_current = min(hosp_current, na.rm = TRUE),
                  max_hosp_current = max(hosp_current, na.rm = TRUE),
                  mean_hosp_current = mean(hosp_current, na.rm = TRUE),
                  median_hosp_current = median(hosp_current, na.rm = TRUE),
                  Q1_hosp_current = quantile(hosp_current, 0.25, na.rm = TRUE),
                  Q3_hosp_current = quantile(hosp_current, 0.75, na.rm = TRUE),
        ) %>% View()
        
        #convert the code below into a function
        #function to find the minimum and maximum for all numeric variables in a dataset
        min_max_n <-
          function(data){
            data %>%
              select_if(is.numeric) %>%
              summarize_all(list(min = min, max = max))
          }
        
        min_max_n(ww_usa_import)
        
        data %>%
          summarize(min_date = min(date), 
                    max_date = max(date), 
                    max_viral_load = max(viral_load, na.rm = TRUE),
                    min_viral_load = min(viral_load, na.rm = TRUE),
                    count = n())
        }
        ww_usa_import %>%
        
        
        head(ww_usa)
        
        ww_usa %>%
          summarize(min_date = min(date), 
                    max_date = max(date), 
                    min_viral_load = min(viral_load, na.rm = TRUE),
                    max_viral_load = max(viral_load, na.rm = TRUE


    
bigfile %>% 
  group_by(location_name) %>% 
  summarize(mindate = min(date), 
            maxdate = max(date)) %>%
  View()
    "hosp_usa",
    "hosp_usa_import_new",
    "hosp2_usa",
    "hosp2_usa_import_current",
    

    "ww_dk_hosp",
    "ww_dk_import",
    
    "ww_usa",
    "ww_usa_import"
    "death_usa",
    "death_usa_import",
    
    [7] "date_to_cdcdate"          "death_usa"               
    [9] "death_usa_import"         "epiweek_to_date"         
    [11] "file_loc"                 "hosp_den"                
    [13] "hosp_den_import"          "hosp_usa"                
    [15] "hosp_usa_import_new"      "hosp2_usa"               
    [17] "hosp2_usa_import_current" "min_max_n"               
    [19] "next_saturday"            "owid"                    
    [21] "preq"                     "selected_columns"        
    [23] "work_laptop"              "ww_dk"                   
    [25] "ww_dk_hosp"               "ww_dk_import"            
    [27] "ww_usa"                   "ww_usa_import" 