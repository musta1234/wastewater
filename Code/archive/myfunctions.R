# Functions
summarize_ihme <- function(ihme_df, group_index =1) {
  ihme_df %>%
    group_by(names(ihme_df[1])) %>%
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

summarize_ihme(data_regions, grouping = location_name) %>% View()
