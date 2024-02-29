#date	country	est_infections	reported_cases_mov_avg	underreporting_value	
#wildtype	wildtype_median	wildtype_iqr_low	wildtype_iqr_high	alpha	alpha_median	
#alpha_iqr_low	alpha_iqr_high	beta	beta_median	beta_iqr_low	beta_iqr_high	
#gamma	gamma_median	gamma_iqr_low	gamma_iqr_high	delta	delta_median	
#delta_iqr_low	delta_iqr_high	omicron	omicron_median	omicron_iqr_low	
#omicron_iqr_high

summary_stats_wide <- 
  summary_stats %>% 
  select(location, variant, p25, median, p75, summary_by) %>% 
  filter((summary_by == "location_name" | summary_by == "location_name/variant" | 
           summary_by == "location_name/subvariant") & 
           !is.na(variant)) %>% 
  select(-summary_by) %>%
  pivot_wider(names_from = variant, values_from = c(median, p25, p75)) %>%
  rename(location_name = location)
  

#################################################################
 hannahs_file <-
   left_join(bigfile, summary_stats_wide, by=c("location_name" = "location_name")) %>% 
   filter(location_name %in% list27) %>% 
   select("location_name", "date", "inf_mean", "daily_cases","population_2023", 
        "nonvoc", "alpha", "beta", "delta", "gamma", "omicron", "ba1", "ba2", 
        "ba45", "prealpha", "prebeta", "predelta", "pregamma", 
        "daily_cases_15dc", "idr_v5", "daily_cases_15dc_mill", 
        "inf_mean_mill", "variant", "subvariant", 
        "continent", "region", "median_All", "median_Alpha", "median_Delta", 
        "median_Non-VOC", "median_Omicron", "median_Gamma", "median_Pre-Delta", 
        "median_Beta", "median_BA.1", "median_BA.2", "median_BA.4 BA.5", 
        "p25_All", "p25_Alpha", "p25_Delta", "p25_Non-VOC", "p25_Omicron", 
        "p25_Gamma", "p25_Pre-Delta", "p25_Beta", "p25_BA.1", "p25_BA.2", 
        "p25_BA.4 BA.5", "p75_All", "p75_Alpha", "p75_Delta", "p75_Non-VOC", 
        "p75_Omicron", "p75_Gamma", "p75_Pre-Delta", "p75_Beta", "p75_BA.1", 
        "p75_BA.2", "p75_BA.4 BA.5") %>%
   filter(daily_cases >= 0) %>%
   
   rename(
     country	=	location_name,
     date	=	date	,
     estimated_infections_daily	=	inf_mean	,
     reported_infections_daily	=	daily_cases	,
     population_2023	=	population_2023	,
     continent	=	continent	,
     daily_infections_15day_c	=	daily_cases_15dc	,
     daily_infections_15dc_permill	=	daily_cases_15dc_mill	,
     infection_detection_ratio_15dc	=	idr_v5	,
     estimated_infections_permill	=	inf_mean_mill	,
     region	=	region	,
     
     All_median	=	median_All	,
     All_q1	=	p25_All	,
     All_q3	=	p75_All	,
     Alpha_median	=	median_Alpha	,
 Alpha_q1	=	p25_Alpha	,
 Alpha_q3	=	p75_Alpha	,
 BA1_median	=	median_BA.1	,
 BA1_q1	=	p25_BA.1	,
 BA1_q3	=	p75_BA.1	,
 BA2_median	=	median_BA.2	,
 BA2_q1	=	p25_BA.2	,
 BA2_q3	=	p75_BA.2	,
 BA4BA5_median	=	"median_BA.4 BA.5"	,
 BA4BA5_q1	=	"p25_BA.4 BA.5"	,
 BA4BA5_q3	=	"p75_BA.4 BA.5"	,
 Beta_median	=	median_Beta	,
 Beta_q1	=	p25_Beta	,
 Beta_q3	=	p75_Beta	,
 Delta_median	=	median_Delta	,
 Delta_q1	=	p25_Delta	,
 Delta_q3	=	p75_Delta	,
 Gamma_median	=	median_Gamma	,
 Gamma_q1	=	p25_Gamma	,
 Gamma_q3	=	p75_Gamma	,
 NonVOC_median	=	"median_Non-VOC"	,
 NonVOC_q1	=	"p25_Non-VOC"	,
 NonVOC_q3	=	"p75_Non-VOC"	,
 Omicron_median	=	median_Omicron	,
 Omicron_q1	=	p25_Omicron	,
 Omicron_q3	=	p75_Omicron	,
 PreDelta_median	=	"median_Pre-Delta"	,
 PreDelta_q1	=	"p25_Pre-Delta"	,
 PreDelta_q3	=	"p75_Pre-Delta") %>% 

select(
  country	,
  date	,
  estimated_infections_daily	,
  reported_infections_daily	,
  
  daily_infections_15day_c	,
  daily_infections_15dc_permill	,
  infection_detection_ratio_15dc	,
  estimated_infections_permill	,
  population_2023	,
  continent	,
  region	,
  
  variant	,
  subvariant	,
  
  alpha	,
  ba1	,
  ba2	,
  ba45	,
  beta	,
  delta	,
  gamma	,
  nonvoc	,
  omicron	,
  prealpha	,
  prebeta	,
  predelta	,
  pregamma	,
  
  All_median	,
  All_q1	,
  All_q3	,
  Alpha_median	,
  Alpha_q1	,
  Alpha_q3	,
  BA1_median	,
  BA1_q1	,
  BA1_q3	,
  BA2_median	,
  BA2_q1	,
  BA2_q3	,
  BA4BA5_median	,
  BA4BA5_q1	,
  BA4BA5_q3	,
  Beta_median	,
  Beta_q1	,
  Beta_q3	,
  Delta_median	,
  Delta_q1	,
  Delta_q3	,
  Gamma_median	,
  Gamma_q1	,
  Gamma_q3	,
  NonVOC_median	,
  NonVOC_q1	,
  NonVOC_q3	,
  Omicron_median	,
  Omicron_q1	,
  Omicron_q3	,
  PreDelta_median	,
  PreDelta_q1	,
  PreDelta_q3
  ) %>% 
   
   mutate(
     Alpha_median	= case_when(alpha == 0 ~ NA, .default = Alpha_median),
     Alpha_q1	= case_when(alpha == 0 ~ NA, .default = Alpha_q1),
     Alpha_q3	= case_when(alpha == 0 ~ NA, .default = Alpha_q3),
     
     BA1_median	= case_when(ba1 == 0 ~ NA, .default = BA1_median),
     BA1_q1	= case_when(ba1 == 0 ~ NA, .default = BA1_q1),
     BA1_q3 = case_when(ba1 == 0 ~ NA, .default = BA1_q3),
     
     BA2_median	= case_when(ba2 == 0 ~ NA, .default = BA2_median),
     BA2_q1	= case_when(ba2 == 0 ~ NA, .default = BA2_q1),
     BA2_q3	= case_when(ba2 == 0 ~ NA, .default = BA2_q3),
     
     BA4BA5_median = case_when(ba45 == 0 ~ NA, .default = BA4BA5_median),
     BA4BA5_q1 = case_when(ba45 == 0 ~ NA, .default = BA4BA5_q1),
     BA4BA5_q3 = case_when(ba45 == 0 ~ NA, .default = BA4BA5_q3),
     
     Beta_median = case_when(beta == 0 ~ NA, .default = Beta_median),
     Beta_q1 = case_when(beta == 0 ~ NA, .default = Beta_q1),
     Beta_q3 = case_when(beta == 0 ~ NA, .default = Beta_q3),
     
     Delta_median = case_when(delta == 0 ~ NA, .default = Delta_median),
     Delta_q1	 = case_when(delta == 0 ~ NA, .default = Delta_q1),
     Delta_q3	 = case_when(delta == 0 ~ NA, .default = Delta_q3),
     
     Gamma_median	 = case_when(gamma == 0 ~ NA, .default = Gamma_median),
     Gamma_q1	 = case_when(gamma == 0 ~ NA, .default = Gamma_q1),
     Gamma_q3	 = case_when(gamma == 0 ~ NA, .default = Gamma_q3),
     
     NonVOC_median = case_when(nonvoc == 0 ~ NA, .default = NonVOC_median),
     NonVOC_q1 = case_when(nonvoc == 0 ~ NA, .default = NonVOC_q1),
     NonVOC_q3 =	case_when(nonvoc == 0 ~ NA, .default = NonVOC_q3),
     
     Omicron_median = case_when(omicron == 0 ~ NA, .default = Omicron_median),
     Omicron_q1 = case_when(omicron == 0 ~ NA, .default = Omicron_q1),
     Omicron_q3 = case_when(omicron == 0 ~ NA, .default = Omicron_q3),
     
     PreDelta_median = case_when(predelta == 0 ~ NA, .default = PreDelta_median),
     PreDelta_q1 = case_when(predelta == 0 ~ NA, .default = PreDelta_q1),
     PreDelta_q3 = case_when(predelta == 0 ~ NA, .default = PreDelta_q3)
     
   )
 #######################################################
 
 View(hannahs_file)

 write.csv(hannahs_file, 
           file = paste0(getwd(), "./output/" ,"hannahs_file", 
                         paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
           quote = TRUE,
           na = "",
           row.names = FALSE)
 

#big rnames
 
wildtype_median	median value of underreporting per country-VOC period, where all wildtype date cells have the median value and all other cells are null
wildtype_iqr_low	lower IQR value of underreporting per country-VOC period where all wildtype date cells have the lower IQR value and all other cells are null
wildtype_iqr_high	higher IQR value of underreporting per country-VOC period where all wildtype date cells have the higher IQR value and all other cells are null
alpha	repeat of lines 7-10 but for alpha
alpha_median	
alpha_iqr_low	
alpha_iqr_high	
beta	repeat of lines 7-10 but for beta
beta_median	
beta_iqr_low	
beta_iqr_high	
gamma	repeat of lines 7-10 but for gamma
gamma_median	
gamma_iqr_low	
gamma_iqr_high	
delta	repeat of lines 7-10 but for delta
delta_median	
delta_iqr_low	
delta_iqr_high	
omicron	repeat of lines 7-10 but for omicron
omicron_median	
omicron_iqr_low	
omicron_iqr_high	
#wildtype	dates that are considered wildtype per date-country have a value of 1; dates that are not considered wildtype have a value of null
