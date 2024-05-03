df <- big_nwss %>% filter(!is.na(nwss) & !is.na(ihme_wkly))

coeff = max(df$ihme_wkly)/max(df$nwss)
ww_inf <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=ihme_wkly), color = "blue3") +
  geom_line(aes(y=nwss*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Weekly estimated infections (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

ww_inf


View(ww_dk_hosp)
df <- 
  ww_dk_hosp %>%
  #calculate daily_cases_wkly
  mutate(daily_cases_wkly = rollapply(daily_cases, 7, mean, fill = NA, align = "right")) %>%
  select(date, inf_mean, inf_mean_wkly, daily_cases, hosp_new, hosp_new_wkly, viral_load)

#coeff = max(df$viral_load)/max(df$nwss)
coeff = 25
#ww_inf <-
  ggplot(
    df %>% filter(!is.na(viral_load)), 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=hosp_new_wkly*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Viral load (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="New Weekly Hospitalizations (red)")
  )
  
  
  ggplot(
    df %>% filter(!is.na(viral_load)), 
    aes(x=date)) +
    geom_line(aes(y=viral_load), color = "blue3")
