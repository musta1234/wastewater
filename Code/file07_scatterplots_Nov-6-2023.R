#scatterplot limited to BA45, color dots by countryand then put a line through 
#each countries dots and overall line regardless of country


df_ba45_na <- 
  bigfile %>% 
  ungroup() %>% filter(subvariant == "BA.4 BA.5" ) %>%  
  select(location_name, region, continent, date, daily_cases, daily_cases_mill, idr_v5, subvariant) %>%
  rename(Location = location_name)

p0 <- NULL
p0

p0 <- 
  ggplot(data=df_hc1A, aes(x= daily_cases_mill, y=idr_v5)) + 
  #geom_point(aes(color=region)) +
  #facet_wrap(~location_name) + 

  geom_smooth(method = "lm", linetype = "dashed") + 
  geom_smooth(method = "lm", aes(color=Location)) +
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")
p0

p0 <- 
  ggplot(data=df_ba45, aes(x= daily_cases_mill, y=idr_v5)) + 
  #geom_point(aes(color=region)) +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")
p0

p0 + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  geom_smooth(method = "lm", aes(color=region), se = FALSE)

p <- 
  ggplot(data=df_ba45, aes(x= daily_cases_mill, y=idr_v5, color = region)) + 
  geom_point() +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")

p + stat_summary(fun.data= mean_cl_normal) 
p + geom_smooth(method='lm') 


##############################################################

set_a <- 
  c("Singapore",	"Luxembourg",	"Portugal",	"Japan",	"Italy",	"Greece",
    "France",	"Germany",	"Austria")

set_b <- 
  c("Belgium", "Sweden", "Spain", "United States of America", "Canada", 
    "Denmark", "United Kingdom", "Ireland", "Netherlands", "Finland")

set_c <- 
  c("Brazil", "Colombia", "Israel", "Malaysia", "Qatar", "Chile", "China", 
    "South Africa")

df_ba45_setB <- 
  bigfile %>% 
  ungroup() %>% filter(subvariant == "BA.4 BA.5" & location_name %in% set_b) %>%  
  select(location_name, region, continent, date, daily_cases, daily_cases_mill, idr_v5, subvariant) %>%
  rename(Location = location_name)

p <- 
  ggplot(data=df_ba45_setB, aes(x= daily_cases_mill, y=idr_v5, color = Location)) + 
  geom_point() +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")

p + geom_smooth(method = "lm", se = FALSE) + 
  geom_smooth(method = "lm", aes(color=region), se = FALSE, linetype = "dashed")


df_ba45_setA <- 
  bigfile %>% 
  ungroup() %>% filter(subvariant == "BA.4 BA.5" & location_name %in% set_a) %>%  
  select(location_name, region, continent, date, daily_cases, daily_cases_mill, idr_v5, subvariant) %>%
  rename(Location = location_name)

p <- 
  ggplot(data=df_ba45_setA, aes(x= daily_cases_mill, y=idr_v5, color = Location)) + 
  geom_point() +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")

p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  geom_smooth(method = "lm", aes(color=Location), se = FALSE)



df_ba45_setC <- 
  bigfile %>% 
  ungroup() %>% filter(subvariant == "BA.4 BA.5" & location_name %in% set_c) %>%  
  select(location_name, region, continent, date, daily_cases, daily_cases_mill, idr_v5, subvariant) %>%
  rename(Location = location_name)

p <- 
  ggplot(data=df_ba45_setC, aes(x= daily_cases_mill, y=idr_v5, color = Location)) + 
  geom_point() +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")

p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  geom_smooth(method = "lm", aes(color=Location), se = FALSE)




##########################################################################
df_europe <- 
  bigfile %>% 
  ungroup() %>% filter(continent == "Europe", !is.na(subvariant)) %>% 
  select(daily_cases_mill, idr_v5, subvariant, location_name)


ggplot(data=df_europe, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color = subvariant)) +
  facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")


df_asia <- 
  bigfile %>% 
  ungroup() %>% filter(continent == "Asia", !is.na(subvariant)) %>% 
  select(daily_cases_mill, idr_v5, subvariant, location_name)


ggplot(data=df_asia, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color = subvariant)) +
  facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")


df_others <- 
  bigfile %>% 
  ungroup() %>% filter(continent != "Asia" & continent != "Europe", 
                       !is.na(subvariant)) %>% 
  select(daily_cases_mill, idr_v5, subvariant, location_name)


ggplot(data=df_others, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color = subvariant)) +
  facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")


df_europe2 <- 
  bigfile %>% 
  ungroup() %>% filter(continent == "Europe", !is.na(subvariant)) %>% 
  select(daily_cases_mill, idr_v5, subvariant, location_name)


ggplot(data=df_europe, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color = subvariant)) +
  facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
         y = "Infection detection ratio")


df_china <- 
  bigfile %>% 
  ungroup() %>% filter(location_name == "China", 
                       !is.na(subvariant)) %>% 
  select(daily_cases_mill, idr_v5, subvariant, location_name)


ggplot(data=df_china, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color = subvariant)) +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")



df_malaysia <- 
  bigfile %>% 
  ungroup() %>% filter(location_name == "Malaysia", 
                       !is.na(subvariant)) %>% 
  select(daily_cases_mill, idr_v5, subvariant, location_name)


ggplot(data=df_malaysia, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color = subvariant)) +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")


df_qatar <- 
  bigfile %>% 
  ungroup() %>% filter(location_name == "Qatar", 
                       !is.na(subvariant)) %>% 
  select(daily_cases_mill, idr_v5, subvariant, location_name)


ggplot(data=df_qatar, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color = subvariant)) +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")

df_qatar <- 
  bigfile %>% 
  ungroup() %>% filter(location_name == "Qatar", 
                       !is.na(subvariant)) %>% 
  select(daily_cases_mill, idr_v5, subvariant, location_name)


ggplot(data=df_qatar, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color = subvariant)) +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")


