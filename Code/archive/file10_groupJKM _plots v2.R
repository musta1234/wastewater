
#write a function for ggplot with and without dots
#make plots for J groups
#facet wrap
# make plots for K and M groups 
# facet wrap m and k groups 
#edit slides
# One figure with three groups with dashed line +SE no dots//
#Randos go to supplement without dashed line

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
#library(ggpubr)

final_groups <- 
  read.delim(paste0(file_loc, "/output/groupings.tab")) %>% 
  select(location_name, final_groups) %>%
  mutate(location_name = 
           replace(location_name, location_name == "United States of America", "USA"))

groups_3ltr <- 
  read.delim(paste0(file_loc, "/output/groupings_3letter.tab")) %>% 
  mutate(Group = trimws(Group)) %>% #trimming white space from " Other"
  #adding #new_slope to move disentangle country markers in the plot 
  mutate(new_slope = case_when( Country == "Sweden" ~ 13.5,
                              Country == "Colombia" ~ 6.65,
                              Country == "Germany" ~ 15,
                              Country == "Italy" ~ 8.5,
                              Country == "Japan" ~ 8,
                              Country == "Austria" ~ 15,
                              Country == "Luxembourg" ~ 5,
                              TRUE ~ Slope
                              ))

########CREATE TABLE OF GROUPS AND MEDIAN IDR #####################
slope_int_idr <- 
  bigfile %>% filter(ba45 == 1) %>% 
  summarize(median_idr = median(idr_v5, na.rm = TRUE),
            median_dlycasesmill15dc = median(daily_cases_15dc_mill, na.rm =TRUE)) %>% 
  mutate(across('location_name', 
                \(x) str_replace(x,'United States of America', 'United States'))) %>% 
  rename(Country = location_name) %>% 
  left_join(groups_3ltr %>% select(-new_slope), .) #%>% View()
dir.create(tempdir())
write.csv(slope_int_idr, 
          file = paste0(getwd(), "./output/" ,"slope_int_idr_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE,
          na = "",
          row.names = FALSE)

###############################

point <- 
  ggplot(groups_3ltr, aes(y=Intercept, color = Group)) +
  geom_point(aes(x = Slope), size=2) +
  geom_text(aes(x = new_slope, label=Code), size=3.2, 
            nudge_y = 0.005, show.legend = FALSE)

point_exChina <- 
  ggplot(groups_3ltr %>% filter(Slope > 0),
         aes(y=Intercept, color = Group)) +
  geom_point(aes(x = Slope), size=2) +
  geom_text(aes(x = new_slope, label=Code), size=3.2, 
            nudge_y = 0.005, show.legend = FALSE)


point <-
  ggplot(groups_3ltr, aes(x=Slope, y=Intercept, color = Group)) +
  geom_point(size=0) +
  geom_text(aes(label=Code), size=4)

#point_exChina <-
#  ggplot(groups_3ltr %>% filter(Slope > 0), aes(x=Slope, y=Intercept, color = Group)) +
#  geom_point(size=2) +
#  geom_text(aes(label=Code), size=3.2, 
#            nudge_y = 0.005, show.legend = FALSE)



  #select(location_name, final_groups) %>%
  #mutate(location_name = 
  #         replace(location_name, location_name == "United States of America", "USA"))
p <- ggplot(oc, aes(M_w_earnings, W_w_earnings, label = as.character(Occupational.Group)))
p + geom_point(aes(size = size, colour=percent_women)) + scale_size_continuous(range=c(0,30)) + #scale_area()+
  

df_ba45_na <- 
  bigfile %>% 
  ungroup() %>% filter(subvariant == "BA.4 BA.5" ) %>%  
  select(location_name, region, continent, date, daily_cases, daily_cases_mill, idr_v5, subvariant) %>%
  mutate(location_name = 
           replace(location_name, 
                   location_name == "United States of America", "USA")
         ) %>%
  rename(Country = location_name)
  
#####################################################################################
#myplaces <- 
#  combine_groups %>% filter(group_J =="B") %>% select(location_name) %>% unlist()
#names(myplaces) <- NULL
#myplaces

plot_lines <- 
  #takes in dataset, vector of places and shade 0/1
  function(data_in = df_ba45_na, places=myplaces, shade=0){
    dframe<- data_in %>% filter(Country %in% places)
    
    p0 <- NULL
    p0 <- 
      ggplot(data=dframe, aes(x= daily_cases_mill, y=idr_v5)) + 
      geom_smooth(method = "lm", linetype = "dashed", se = shade, alpha=0.1, size=1) + 
      #geom_smooth(method = "lm", aes(color = Country), se = shade, alpha=0.2) +
      geom_smooth(method = "lm", aes(color = rev(Country)), se = shade, alpha=0.2) + #this reverses the order of lines helping Austria stand out
      labs(x = "Daily reported infections per million population",
           y = "Infection detection ratio") +
      guides(color = guide_legend(title = "Country")) 
    
    return(p0)
  }


plot_lines_supp <- 
  #takes in dataset, vector of places and shade 0/1
  function(data_in = df_ba45_na, places=myplaces, shade=0){
    dframe<- data_in %>% filter(Country %in% places)
    
    p0 <- NULL
    p0 <- 
      ggplot(data=dframe, aes(x= daily_cases_mill, y=idr_v5)) + 
      geom_smooth(method = "lm", linetype = "dashed", se = shade) + 
      #geom_smooth(method = "lm", aes(color=Country), se = shade) +
      labs(x = "Daily reported infections per million",
           y = "Infection detection ratio")
    return(p0)
  }
#########################################################################

final_a <- 
  final_groups %>% filter(final_groups =="Group A") %>% select(location_name) %>% unlist()
final_b <- 
  final_groups %>% filter(final_groups =="Group B") %>% select(location_name) %>% unlist()
final_c <- 
  final_groups %>% filter(final_groups =="Group C") %>% select(location_name) %>% unlist()
final_d <- 
  final_groups %>% filter(final_groups =="Group D") %>% select(location_name) %>% unlist()

plot_finala_se <- plot_lines(places = final_a, shade = 1) + ggtitle("Group A")
plot_finalb_se <- plot_lines(places = final_b, shade = 1) + ggtitle("Group B")
plot_finalc_se <- plot_lines(places = final_c, shade = 1) + ggtitle("Group C")
plot_finald_se <- plot_lines_supp(places = final_d, shade = 1) + ggtitle("Group D")

plot_finala_se
plot_finalb_se
plot_finalc_se
plot_finald_se

ggarrange(plot_finala_se, plot_finalb_se, plot_finalc_se)


plot_finala <- plot_lines(places = final_a, shade = 0) + ggtitle("Group A")
plot_finalb <- plot_lines(places = final_b, shade = 0) + ggtitle("Group B")
plot_finalc <- plot_lines(places = final_c, shade = 0) + ggtitle("Group C")
plot_finald <- plot_lines_supp(places = final_d, shade = 0) #+ ggtitle("Group D")

plot_finala
plot_finalb
plot_finalc
plot_finald

ggarrange(plot_finala, plot_finalb, plot_finalc)

plot_finald_se

ja <- 
  combine_groups %>% filter(group_J =="A") %>% select(location_name) %>% unlist()
jb <- 
  combine_groups %>% filter(group_J =="B" 
                            #| location_name == "Israel"
                            ) %>% select(location_name) %>% unlist()
jc <- 
  combine_groups %>% filter(group_J =="C") %>% select(location_name) %>% unlist()


plot_jb <- plot_lines(places = jb) + ggtitle("Group B")
plot_ja <- plot_lines(places = ja) + ggtitle("Group A")
plot_jc <- plot_lines(places = jc) + ggtitle("Group C")

plot_jb_se <- plot_lines(places = jb, shade = 1) + ggtitle("Group B")
plot_ja_se <- plot_lines(places = ja, shade = 1) + ggtitle("Group A")
plot_jc_se <- plot_lines(places = jc, shade = 1) + ggtitle("Group C")

plot_ja_se
plot_jb_se
plot_jc_se

plot_ja + geom_point(aes(color = Country))
plot_jb + geom_point(aes(color = Country))
plot_jc + geom_point(aes(color = Country))

ka <- 
  combine_groups %>% filter(group_K == "Group A" | location_name == "Belgium") %>% select(location_name) %>% unlist()
kb <- 
  combine_groups %>% filter(group_K == "Group B") %>% select(location_name) %>% unlist()
kc <- 
  combine_groups %>% filter(group_K == "Group C") %>% select(location_name) %>% unlist()
kd <- 
  combine_groups %>% filter(group_K == "Group D") %>% select(location_name) %>% unlist()


plot_ka_se <- plot_lines(places = ka, shade = 1) + ggtitle("Alt Group A")
plot_kb_se <- plot_lines(places = kb, shade = 1) + ggtitle("Alt Group B")
plot_kc_se <- plot_lines(places = kc, shade = 1) + ggtitle("Alt Group C")
plot_kd_se <- plot_lines(places = kd, shade = 1) + ggtitle("Alt Group D")

plot_ka_se
plot_kb_se
plot_kc_se
plot_kd_se


ggarrange(plot_ka_se, plot_kb_se, plot_kc_se, plot_kd_se, ncol=2)


#+ geom_point(aes(color=Location))

  
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
