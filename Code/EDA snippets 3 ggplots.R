


#make ggplot charts for the following variables
##USA
#viral_load vs (inf_mean_7dt, daily_cases_7dt, hosp_current, hosp_new)

##Denmark
#viral_load vs (hosp_new_wkly, cases, inf_mean_7dt, daily_cases_7dt)

## Correlations by quarter/variant

############usa_viral_hosp###########################
df <- usa_dataset %>% 
  filter(!is.na(viral_load) & !is.na(hosp_new)) #%>%
#select(date, var1, var2) %>%


coeff = max(df$viral_load)/max(df$hosp_new)

pic_viral_hosp <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=hosp_new*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater level (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="New weekly hospitalizations (red)", labels = comma)
  ) +
  ggtitle("Wastewater viral load vs New weekly hospitalizations, USA") +
  labs(x = "Date")

pic_viral_hosp

#############usa_viral_hosp2################################

df <- usa_dataset %>% 
  filter(!is.na(viral_load) & !is.na(hosp_current)) #%>%
#select(date, var1, var2) %>%


coeff = max(df$viral_load)/max(df$hosp_current)

pic_viral_hosp2 <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=hosp_current*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater level (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Current Covid  hospitalizations (red)", labels = comma)
  ) +
  ggtitle("Wastewater viral load vs Current Covid hospitalizations, USA") +
  labs(x = "Date")

pic_viral_hosp2

##############usa_viral_inf###############################


df <- usa_dataset %>% 
  filter(!is.na(viral_load) & !is.na(inf_mean_7dts)) #%>%
#select(date, var1, var2) %>%


coeff = max(df$viral_load)/max(df$inf_mean_7dts)

pic_viral_inf <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=inf_mean_7dts*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater level (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Estimated infections, (red)", labels = comma)
  ) +
  ggtitle("Wastewater viral load vs Estimated infections, USA") +
  labs(x = "Date")

pic_viral_inf

##############usa_viral_cases###############################

df <- usa_dataset %>% 
  filter(!is.na(viral_load) & !is.na(daily_cases_7dts)) #%>%
#select(date, var1, var2) %>%

coeff = max(df$viral_load)/max(df$daily_cases_7dts)

pic_viral_cases <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=daily_cases_7dts*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater level (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Reported infections, (red)", labels = comma)
  ) +
  ggtitle("Wastewater viral load vs Reported infections, USA") +
  labs(x = "Date")

pic_viral_cases


################den_viral_hosp#############################

df <- ww_dk_hosp %>% 
  filter(!is.na(viral_load) & !is.na(hosp_new_wkly)) #%>%
#select(date, var1, var2) %>%


coeff = max(df$viral_load)/max(df$hosp_new_wkly)

den_viral_hosp <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=hosp_new_wkly*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater viral load (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="New weekly hospitalizations (red)", labels = comma),
    labels = comma
  ) +
  ggtitle("Wastewater viral load vs New weekly hospitalizations, Denmark") +
  labs(x = "Date")

den_viral_hosp

##################den_viral_inf###########################

df <- ww_dk_hosp %>% 
  filter(!is.na(viral_load) & !is.na(inf_mean_7dts)) #%>%
#select(date, var1, var2) %>%


coeff = max(df$viral_load)/max(df$inf_mean_7dts)

den_viral_inf <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=inf_mean_7dts*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater viral load (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Estimated Weekly Infections (red)", labels = comma),
    labels = comma
  ) +
  ggtitle("Wastewater viral load vs Estimated Weekly Infections, Denmark") +
  labs(x = "Date")

den_viral_inf #+ scale_y_continuous(trans='log10')

####################den_viral_cases#########################

df <- ww_dk_hosp %>% 
  filter(!is.na(viral_load) & !is.na(daily_cases_7dts)) #%>%
#select(date, var1, var2) %>%
coeff = max(df$viral_load)/max(df$daily_cases_7dts)

den_viral_cases <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=daily_cases_7dts*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater viral load (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Reported Weekly Infections (red)", labels = comma),
    labels = comma
  ) +
  ggtitle("Wastewater viral load vs Reported Weekly Infections, Denmark") +
  labs(x = "Date")

den_viral_cases

#############################################

####################Netherlands#########################

df <- neth_dataset %>% 
  filter(!is.na(viral_load) & !is.na(inf_mean_7dts)) #%>%
#select(date, var1, var2) %>%
coeff = max(df$viral_load)/max(df$inf_mean_7dts)

x <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=inf_mean_7dts*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater viral load (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Estimated Infections (red)", labels = comma),
    labels = comma
  ) +
  #ggtitle("Wastewater viral load vs Reported Weekly Infections, Denmark") +
  labs(x = "Date")

x

#############################################

df <- neth_dataset %>% 
  filter(!is.na(viral_load) & !is.na(hosp_new)) #%>%
#select(date, var1, var2) %>%
coeff = max(df$viral_load)/max(df$hosp_new)

x <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=hosp_new*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater viral load (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="New Covid Hospitalizations(red)", labels = comma),
    labels = comma
  ) +
  #ggtitle("Wastewater viral load vs Reported Weekly Infections, Denmark") +
  labs(x = "Date")
x


################################################################

df <- neth_dataset %>% 
  filter(!is.na(viral_load) & !is.na(daily_cases_7dts)) #%>%
#select(date, var1, var2) %>%
coeff = max(df$viral_load)/max(df$daily_cases_7dts)

x <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=viral_load), color = "blue3") +
  geom_line(aes(y=daily_cases_7dts*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wastewater viral load (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Reported Infections (red)", labels = comma),
    labels = comma
  ) +
  #ggtitle("Wastewater viral load vs Reported Weekly Infections, Denmark") +
  labs(x = "Date")

x

################################################################


#####################neth_hospnew###########################################


df <- neth_dataset %>% 
  filter(!is.na(hosp_new) & !is.na(inf_mean_7dts)) #%>%
#select(date, var1, var2) %>%
coeff = max(df$hosp_new)/max(df$inf_mean_7dts)

x <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=hosp_new), color = "blue3") +
  geom_line(aes(y=inf_mean_7dts*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "New Covid Hospitalizations (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Estimated Infections (red)", labels = comma),
    labels = comma
  ) +
  #ggtitle("New Covid Hospitalizations vs Reported Weekly Infections, Denmark") +
  labs(x = "Date")

x

################################################################

df <- neth_dataset %>% 
  filter(!is.na(hosp_new) & !is.na(daily_cases_7dts)) #%>%
#select(date, var1, var2) %>%
coeff = max(df$hosp_new)/max(df$daily_cases_7dts)

x <-
  ggplot(
    df, 
    aes(x=date)) +
  geom_line(aes(y=hosp_new), color = "blue3") +
  geom_line(aes(y=daily_cases_7dts*coeff), color = "red2")+
  
  scale_y_continuous(
    # Features of the first axis
    name = "New Covid Hospitalizations (blue)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Reported Infections (red)", labels = comma),
    labels = comma
  ) +
  #ggtitle("New Covid Hospitalizations vs Reported Weekly Infections, Denmark") +
  labs(x = "Date")

x

################################################################

