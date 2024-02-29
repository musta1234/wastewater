
x1<- 
  bigfile %>% 
  #filter(subvariant == "BA.4 BA.5") %>% 
  group_by(region, subvariant) %>%
  #group_by(variant) %>%
  summarize(
    n_obs = sum(!is.na(daily_cases)),
    n_days = max(date, na.rm = TRUE) - min(date, na.rm = TRUE) + 1,
    median_idr = median(idr_v5, na.rm = TRUE),
    q1 = quantile(idr_v5, 0.25, na.rm = TRUE),
    q3 = quantile(idr_v5, 0.75, na.rm = TRUE)
  ) %>% 
  filter(!is.na(subvariant))
write.csv(x1,
          file = paste0(getwd(), "/output/" ,"x1_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)

x2<- 
  bigfile %>% 
  #filter(subvariant == "BA.4 BA.5") %>% 
  group_by(location_name, subvariant) %>%
  #group_by(variant) %>%
  summarize(
    n_obs = sum(!is.na(daily_cases)),
    n_days = max(date, na.rm = TRUE) - min(date, na.rm = TRUE) + 1,
    median_idr = median(idr_v5, na.rm = TRUE),
    q1 = quantile(idr_v5, 0.25, na.rm = TRUE),
    q3 = quantile(idr_v5, 0.75, na.rm = TRUE)
  ) %>%
  filter(!is.na(subvariant))

write.csv(x2,
          file = paste0(getwd(), "/output/" ,"x2_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)

x3<- 
  bigfile %>% 
  #filter(subvariant == "BA.4 BA.5") %>% 
  group_by(region, variant) %>%
  #group_by(variant) %>%
  summarize(
    n_obs = sum(!is.na(daily_cases)),
    n_days = max(date, na.rm = TRUE) - min(date, na.rm = TRUE) + 1,
    median_idr = median(idr_v5, na.rm = TRUE),
    q1 = quantile(idr_v5, 0.25, na.rm = TRUE),
    q3 = quantile(idr_v5, 0.75, na.rm = TRUE)
  ) %>% 
  filter(!is.na(variant) & variant != "Pre-Delta") #%>% View()

write.csv(x3,
          file = paste0(getwd(), "/output/" ,"x3_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)



x4<- 
  bigfile %>% 
  #filter(subvariant == "BA.4 BA.5") %>% 
  group_by(location_name, variant) %>%
  #group_by(variant) %>%
  summarize(
    n_obs = sum(!is.na(daily_cases)),
    n_days = max(date, na.rm = TRUE) - min(date, na.rm = TRUE) + 1,
    median_idr = median(idr_v5, na.rm = TRUE),
    q1 = quantile(idr_v5, 0.25, na.rm = TRUE),
    q3 = quantile(idr_v5, 0.75, na.rm = TRUE)
  ) %>% 
  filter(!is.na(variant) & variant != "Pre-Delta") #%>% View()

write.csv(x4,
          file = paste0(getwd(), "/output/" ,"x4_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)


x5<- 
  bigfile %>% 
  #filter(subvariant == "BA.4 BA.5") %>% 
  group_by(location_name) %>%
  #group_by(variant) %>%
  summarize(
    n_obs = sum(!is.na(daily_cases)),
    n_days = max(date, na.rm = TRUE) - min(date, na.rm = TRUE) + 1,
    median_idr = median(idr_v5, na.rm = TRUE),
    q1 = quantile(idr_v5, 0.25, na.rm = TRUE),
    q3 = quantile(idr_v5, 0.75, na.rm = TRUE)
  ) %>% #View()
  mutate(variant= "Full data set") #%>% View()
  #filter(!is.na(variant) & variant != "Pre-Delta") #%>% View()

write.csv(x5,
          file = paste0(getwd(), "/output/" ,"x5_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)




##########################


x <- 
  bigfile %>% filter(subvariant == "BA.4 BA.5") %>% 
  group_by(location_name) %>%
  summarize(
    median_idr = median(idr_v5, na.rm = TRUE)
  ) %>% 
  mutate(median_idr_pct = median_idr*100)



write.csv(x,
          file = paste0(getwd(), "/output/" ,"x_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)


