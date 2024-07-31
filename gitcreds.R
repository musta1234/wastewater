library(usethis)
#create_github_token()
library(gitcreds)
gitcreds_set()

#> which git
#> git --version
#edit_git_config()
# usegit()


usa_dataset %>% 
  filter(!is.na(viral_load)) %>% 
  select(viral_load, hosp_new) %>% 
  mutate( c=rollapply(., 
            width = 20, 
            FUN = function(x) cor(x[, 1], x[, 2]), 
            by.column = FALSE, 
            align = "right"
  )
  )%>% plot()

