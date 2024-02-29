
place_list <- 
for (xx in list27) for (yy in list27) print("xx = ",xx, "yy = ",yy)


dir.create(tempdir())

x <- 
bigfile %>%
  filter(subvariant == "BA.4 BA.5") %>% 
  group_by(location_name) %>% 
#  mutate(number = n()) %>%
#  filter( number > 0) %>% 
  summarize(model = list(lm(idr_v5 ~ daily_cases_mill, data = cur_data())),
            coef = list(coef(model[[1]])),
            Rsqrd = summary(model[[1]])$r.sq)%>%
  unnest_wider(coef, names_repair = 'unique') %>% 
  #mutate(model = as.character(model)) %>% 
  select(-model)

write.csv(x,
          file = paste0(getwd(), "/output/" ,"intercepts_slopes_BA45_", 
                        paste(format(Sys.time(), "%Y-%m-%d-%I%p")), ".csv"),
          quote = TRUE, na = "", row.names = FALSE)

set_a <- 
  c("Singapore",	"Luxembourg",	"Portugal",	"Japan",	"Italy",	"Greece",
           "France",	"Germany",	"Austria")

set_b <- 
  c("Belgium", "Sweden", "Spain", "United States of America", "Canada", 
           "Denmark", "United Kingdom", "Ireland", "Netherlands", "Finland")

set_c <- 
  c("Brazil", "Colombia", "Israel", "Malaysia", "Qatar", "Chile", "China", 
    "South Africa")

