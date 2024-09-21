# Import WWS data from NWSS
# Source: https://www.cdc.gov/nwss/rv/COVID19-nationaltrend.html
# Source: https://www.cdc.gov/wcms/vizdata/NCEZID_DIDRI/NWSSRegionalLevel.json

#coeff = 300000
ds <- big_nwss %>% filter(!is.na(nwss) & !is.na(cases_15dc_wkly))

coeff = max(ds$cases_15dc_wkly)/max(ds$nwss)
ww_cases <-
  ggplot(
    ds, 
    aes(x=date)) +
    geom_line(aes(y=cases_15dc_wkly), color = "blue3") +
    geom_line(aes(y=nwss*coeff), color = "red2")+
  
    scale_y_continuous(
      # Features of the first axis
      name = "Weekly reported infections (blue)",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./coeff, name="NWSS (red)")
      )

ww_cases

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

dh <- big_nwss %>% filter(!is.na(nwss) & !is.na(cdc_hosp))

coeff = max(dh$cdc_hosp)/max(dh$nwss)
ww_hosp <-
  ggplot(
    dh, 
    aes(x=date)) +
  geom_line(aes(y=cdc_hosp), color = "blue3") +
  geom_line(aes(y=nwss*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Weekly hospitalizations (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

ww_hosp


dd <- big_nwss %>% filter(!is.na(nwss) & !is.na(cdc_death))

coeff = max(dd$cdc_death)/max(dd$nwss)
ww_death <-
  ggplot(
    dd, 
    aes(x=date)) +
  geom_line(aes(y=cdc_death), color = "blue3") +
  geom_line(aes(y=nwss*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Weekly deaths (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

ww_death

#####Death to hospitalizations ratio############
dx <- big_nwss %>% 
  filter( !is.na(cdc_hosp) & !is.na(cdc_death)) %>%
  mutate(death_to_hosp = cdc_death/cdc_hosp)

death_ratio <-
  ggplot(
    dx, 
    aes(x=date)) +
  geom_smooth(aes(y=death_to_hosp), color = "blue3") +
  geom_line(aes(y=death_to_hosp), color = "grey10") +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Death to hospitalizations ratio",
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

death_ratio


#####WW to Cases ratio############
dc <- big_nwss %>% 
  filter( !is.na(nwss) & !is.na(cases_15dc_wkly)) %>%
  mutate(case_nwss_ratio = cases_15dc_wkly/nwss)

wwcase_ratio <-
  ggplot(
    dc, 
    aes(x=date)) +
  geom_smooth(aes(y=case_nwss_ratio), color = "blue3") +
  geom_line(aes(y=case_nwss_ratio), color = "grey60") +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Cases to wastewater ratio",
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

wwcase_ratio


#####WW to Cases ratio############
di <- big_nwss %>% 
  filter( !is.na(nwss) & !is.na(ihme_wkly)) %>%
  mutate(inf_nwss_ratio = ihme_wkly/nwss)

wwinf_ratio <-
  ggplot(
    di, 
    aes(x=date)) +
  geom_smooth(aes(y=inf_nwss_ratio), color = "blue3") +
  geom_line(aes(y=inf_nwss_ratio), color = "grey60") +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Infections (IHME) to wastewater ratio",
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

wwinf_ratio

#####WW to hospitalization ratio############
dh <- big_nwss %>% 
  filter( !is.na(nwss) & !is.na(cdc_hosp)) %>%
  mutate(hosp_nwss_ratio = cdc_hosp/nwss)

wwhosp_ratio <-
  ggplot(
    dh, 
    aes(x=date)) +
  geom_smooth(aes(y=hosp_nwss_ratio), color = "blue3") +
  geom_line(aes(y=hosp_nwss_ratio), color = "grey60") +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Hospitalizations to wastewater ratio",
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~./coeff, name="NWSS (red)")
  )

wwhosp_ratio
# deaths 
# deaths per 100k/ hosp per 100k

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
