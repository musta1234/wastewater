

# combine pvalues_allcuts, kslopes_k4, jvalues, kslopes, mat_pvalues_01
#import Jenns classification
#import Ks classification
# combine all


###############################
 combine_groups <-
   pvalues_allcuts %>% 
   left_join(kslopes_k4, by="location_name") %>% 
   left_join(kbetas_k5, by="location_name") %>% 
   left_join(jvalues, by="location_name") %>% 
   left_join(kslopes, by="location_name") %>% 
   left_join(mat_pvalues_01, by="location_name") %>% 
   mutate(location_name = gsub(".", " ", location_name, fixed = TRUE))
 
 write.csv(combine_groups,
           file = paste0(getwd(), "/output/" ,"combine_groups_", 
                         paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
           quote = TRUE, na = "", row.names = FALSE)
 
   

###############################
jvalues <- 
  read.delim(paste0(file_loc, "/output/intercept_slopes_r_groupJ.tab")) %>%
  mutate(location_name =  gsub(" ", ".", location_name, fixed = TRUE)) #%>% View()

mat_pvalues_01 <- 
  (mat_pvalues <0.05) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "location_name") %>%
  mutate(across(Austria:United.States.of.America, as.numeric)) #%>% View()


#######################################

pvalues <- 
  read.delim(paste0(file_loc, "/output/pvalues_pairwise.tab"))
mat_pvalues <- 
  as.matrix(pvalues[ ,2:28])

#colnames(mat_pvalues) <- 
#  colnames(mat_pvalues) %>% gsub("idr_", "", ., fixed = TRUE)

rownames(mat_pvalues) <- 
  colnames(mat_pvalues)

mat_pvalues <0.05 %>% as.numeric() %>% View()
(dist(mat_pvalues <0.05))^2 %>% as.matrix() %>% View()

hc1 <- hclust(as.dist(mat_pvalues <0.05), method = "complete")

plot(as.dendrogram(hc1))
plot(hc1)

abline(h=4, col = "blue", lty = 3)

hc1_cut0 <- 
  cutree(hc1, h=0) %>% as.data.frame() %>% rownames_to_column() %>% 
  rename(location_name = 1, cut_p0 = 2)
hc1_cut1 <- 
  cutree(hc1, h=1) %>% as.data.frame() %>% rownames_to_column() %>%
  rename(location_name = 1, cut_p1 = 2)
hc1_cut2 <- 
  cutree(hc1, h=2) %>% as.data.frame() %>% rownames_to_column() %>% 
  rename(location_name = 1, cut_p2 = 2)
hc1_cut3 <- 
  cutree(hc1, h=3) %>% as.data.frame() %>% rownames_to_column() %>% 
  rename(location_name = 1, cut_p3 = 2)
hc1_cut4 <- 
  cutree(hc1, h=4) %>% as.data.frame() %>% rownames_to_column() %>% 
  rename(location_name = 1, cut_p4 = 2)

pvalues_allcuts <-
  hc1_cut0 %>% 
  left_join(hc1_cut1, by="location_name") %>% 
  left_join(hc1_cut2, by="location_name") %>% 
  left_join(hc1_cut3, by="location_name") %>% 
  left_join(hc1_cut4, by="location_name")
  
#hc1_tree <- cutree(hc1, k=3)
#hc1A <- names(hc1_tree[hc1_tree == 1])
#hc1A <- replace(hc1A, "USA", "United States of America")
#names(hc1A) <- NULL

###################
kslopes <- 
  read.csv(paste0(file_loc, "/output/intercept_slopes_Kanae.csv")) %>%
  arrange(location_name) %>%
  rename(intercept_k = intercept, slope_k = slope) %>% 
  mutate(location_name =  gsub(" ", ".", location_name, fixed = TRUE)) #%>%
#View()

kbetas_mat <- 
  #calculate absolute differences between intercepts
  matrix(
    outer(kslopes$slope_k, kslopes$slope_k, "-"),
    nrow = length(kslopes$slope_k),
    dimnames = list(kslopes$location_name, kslopes$location_name)
  ) %>%
  abs()

kbetas_hc <- 
  hclust(dist(kbetas_mat), method = "complete")

plot(as.dendrogram(kbetas_hc))

kbetas_k5 <- 
  cutree(kbetas_hc, k=5 ) %>% 
  as.data.frame() %>% rownames_to_column() %>%
  rename(location_name = 1, kbetas_k5 = 2)


##########################
df_hc1A <- 
  bigfile %>% 
  ungroup() %>% filter(subvariant == "BA.4 BA.5" & location_name %in% hc1A) %>%  
  select(location_name, region, continent, date, daily_cases, daily_cases_mill, idr_v5, subvariant) %>%
  rename(Location = location_name)

p0 <- 
  ggplot(data=df_hc1A, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color=region)) +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")
p0

p0 + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  geom_smooth(method = "lm", aes(color=Location), se = FALSE)

df_hc1B <- 
  bigfile %>% 
  ungroup() %>% filter(subvariant == "BA.4 BA.5" & location_name %in% hc1A) %>%  
  select(location_name, region, continent, date, daily_cases, daily_cases_mill, idr_v5, subvariant) %>%
  rename(Location = location_name)

p0 <- 
  ggplot(data=df_hc1A, aes(x= daily_cases_mill, y=idr_v5)) + 
  geom_point(aes(color=Location)) +
  #facet_wrap(~location_name) + 
  labs(x = "Daily reported infections per million",
       y = "Infection detection ratio")
p0

p0 + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  geom_smooth(method = "lm", aes(color=Location), se = FALSE)



###################################
mat_ba45 <- as.matrix(var_ba45[ ,2:28])

colnames(mat_ba45) <- colnames(mat_ba45) %>% gsub("idr_", "", ., fixed = TRUE)
rownames(mat_ba45) <- colnames(mat_ba45)

hc <- hclust(dist(mat_ba45^-1))

plot(as.dendrogram(hc))
abline(h=130, col = "blue", lty = 3)


write.csv(mat_ba45,
          file = paste0(getwd(), "/../output/" ,"mat_ba45_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)


#######################################
bigcorr <- 
  bigfile %>% 
  ungroup() %>%
  mutate(location_name=replace(location_name, 
                               location_name == "United States of America", "USA")
  ) %>%
  mutate(short_loc = substr(location_name, 1, 5),
         subvariant = case_when( subvariant == "BA.1" ~ "BA1",
                                 subvariant == "BA.2" ~ "BA2",
                                 subvariant == "BA.4 BA.5" ~ "BA45",
                                 .default = subvariant
                                 
         )) %>% 
  select(date, short_loc, subvariant, idr_v5, inf_mean, daily_cases_15dc) %>%
  rename(cases = daily_cases_15dc, idr = idr_v5, inf = inf_mean) %>% 
  filter(!is.na(subvariant), !is.na(idr), !is.na(inf)) %>%
  pivot_wider(names_from = c("short_loc"), 
              values_from =  c("idr", "inf", "cases")) %>% 
  #replace all NULL values with NA
  mutate(across(idr_South:cases_Nethe, ~replace(., lengths(.) == 0, NA))) %>%
  as.data.frame()

#https://stackoverflow.com/questions/24829027/unimplemented-type-list-when-trying-to-write-table
#bigcorr[3: length(names(bigcorr))] <- apply(bigcorr[3: length(names(bigcorr))],2,as.character)
#bigcorr[3: length(names(bigcorr))] <- apply(bigcorr[3: length(names(bigcorr))],2,as.numeric)


var_ba1 <- bigcorr %>% filter(subvariant == "BA1") %>% 
  select(starts_with("idr_")) %>% cor(., use = "pairwise.complete.obs") %>% 
  as.data.frame() %>% rownames_to_column() %>% 
  mutate(subvariant = "BA1")

var_ba2 <- bigcorr %>% filter(subvariant == "BA2") %>% 
  select(starts_with("idr_")) %>% cor(., use = "pairwise.complete.obs") %>% 
  as.data.frame() %>% rownames_to_column() %>% 
  mutate(subvariant = "BA2")

var_ba45 <- bigcorr %>% filter(subvariant == "BA45") %>% 
  select(starts_with("idr_")) %>% cor(., use = "pairwise.complete.obs") %>% 
  as.data.frame() %>% rownames_to_column() %>% 
  mutate(subvariant = "BA45")

#correlate every country idr vs case_load group_by subvariant
#Generate a series of models and alphas, betas and 

correlation_subvar <- 
  rbind(var_ba1, var_ba2, var_ba45) %>%
  pivot_wider(names_from = "subvariant", 
              values_from =  starts_with("idr"))


#convert correlation matrix to distance matrix for hclust
mat_ba45 <- as.matrix(var_ba45[ ,2:28])

colnames(mat_ba45) <- colnames(mat_ba45) %>% gsub("idr_", "", ., fixed = TRUE)
rownames(mat_ba45) <- colnames(mat_ba45)

hc <- hclust(dist(mat_ba45))

plot(as.dendrogram(hc))
abline(h=2)


write.csv(mat_ba45,
          file = paste0(getwd(), "/../output/" ,"mat_ba45_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)

#install.packages("hclust", dependencies = TRUE)
#library(hclust)

