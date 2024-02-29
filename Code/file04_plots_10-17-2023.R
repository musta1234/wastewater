#ggplots
#author: "Mustapha Mustapha"
#date: "2023-04-06"
#output: html_document
---
##################
## Make line plots and barcharts
#```{r plots, echo=FALSE}
#1 Create line plots for each country and #2) save as pdfs
# create plot
#install.packages("ggpmisc")
#library(ggpmisc)
#library(ggplot2)
#library(gridExtra)
##############################

#1 Normalize per million - Done
#2 Multiple lines (2-6) on each plot - In progress
#3 Second y-axis=total estimated infections - In progress
#5 Cap total lines -  Group by threes [[function or loop]]
#6 Table Row=country/Column= subvariant - Done
#4 Place table underneath the plot - In progress
#7 Main title = 
#8 Use 15dt infections {{Keep 7d trailing infections}} Done
#9 ggsave

batches <- list(c("United States of America", "Canada"),
          c("Japan", "Malaysia", "Singapore", "China"),
          c("United Kingdom", "Ireland", "Belgium"),
          c("Italy", "Spain", "Portugal" ),
          c("Denmark", "Finland", "Sweden"),
          c("Netherlands", "Germany", "Greece"),
          c("Austria", "Luxembourg"),
          c("Qatar", "Israel", "South Africa"),
          c("Chile", "Colombia", "Brazil"))         

for (i in 1:length(batches)){
  
  selected = batches[i] %>% unlist()
  df <- 
    bigfile %>% filter(location_name %in% selected) %>%
    filter(date >= mdy("01-01-2022" ))
 
  maxy <- max(df$daily_cases_15dc_mill)
  miny <- max(df$daily_cases_15dc_mill)*0.8
  maxx <- max(df$date)
  minx <- maxx - 40
  divisor = 1
  
  vartable <- 
    df %>% 
    select(date, location_name,idr_v5, subvariant) %>%
    filter(!is.na(subvariant) & subvariant != "NA subvariant" ) %>%
    group_by(location_name, subvariant) %>%
    summarize(median_IDR = median(idr_v5, na.rm = TRUE)) %>% 
    #mutate(subvariant = case_when(subvariant == "BA.4 BA.5" ~ "BA.4/BA.5",
    # .default = subvariant)) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    pivot_wider(. , names_from = subvariant, values_from = median_IDR)
  
  names(vartable)[1] <- "Country"
  
  p1 <- 
    ggplot(df, aes(x=date, color=location_name)) +
    geom_line(aes(y=daily_cases_15dc_mill), linetype = "longdash") +
    geom_line(aes(y=inf_mean_mill/divisor)) +
    scale_y_continuous("Reported infections per million population\n(centered 15-day average)", 
                       labels = label_number(),
                       sec.axis = sec_axis(~ . *divisor, name = "Daily estimated Infections per million population", 
                                           labels = label_number())
    ) +
  labs(x = "") +
  theme(legend.title=element_blank()) +
  annotation_custom(
      tableGrob( vartable, rows=NULL), 
      xmax=maxx,
      xmin= minx, 
      ymax = maxy, 
      ymin=miny
    )
  p1

  ggsave(paste0("batches_inset00_", i, ".png"), plot = p1, device = "png")
  
}


vartable <- 
  df %>% 
  select(date, location_name,idr_v5, subvariant) %>%
  filter(!is.na(subvariant) & subvariant != "NA subvariant" ) %>%
  group_by(location_name, subvariant) %>%
  summarize(median_IDR = median(idr_v5, na.rm = TRUE)) %>% 
  #mutate(subvariant = case_when(subvariant == "BA.4 BA.5" ~ "BA.4/BA.5",
  # .default = subvariant)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  pivot_wider(. , names_from = subvariant, values_from = median_IDR)

names(vartable)[1] <- "Country"
#try<- data.frame(x=0:1, y=0:1)

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                     base_size = 10,
                     padding = unit(c(2, 4), "mm"))
tbl <- tableGrob(df1, rows=NULL, theme=tt)

png("E:/temp/test.png", width = 1000, height = 1000)
grid.arrange(plot1, plot2, tbl, 
             nrow = 3, heights = c(2, 2, 0.5))
dev.off()



plot1t <- 
  plot1 + annotation_custom(
    tableGrob( mytable, rows=NULL), 
    xmax=maxxhttp://127.0.0.1:36505/help/library/gridExtra/html/tableGrob.html,
    xmin= minx, 
    ymax = maxy, 
    ymin=miny
  )
grid.arrange(plot1t, plot1t, plot1t, plot1t)

#####################################

selected = c("Netherlands", "Belgium", "Sweden")

df <- 
  bigfile %>% filter(location_name %in% selected) %>%
  filter(date >= mdy("01-01-2022" ))

divisor = 1

#p1 <- 
ggplot(df, aes(x=date, color=location_name)) +
  geom_line(aes(y=daily_cases_15dc_mill), linetype = "longdash") +
  geom_line(aes(y=inf_mean_mill/divisor)) +
  scale_y_continuous("Reported infections per million population\n(centered 15-day average)", 
                     labels = label_number(),
                     sec.axis = sec_axis(~ . *divisor, name = "Daily estimated Infections per million population", 
                                         labels = label_number())
  ) +
  labs(x = "")
theme(legend.title=element_blank()) 


ggsave("test2.png", device = "png")


################################
  vartable <- 
    df %>% 
    select(date, location_name,idr_v5, subvariant) %>%
    filter(!is.na(subvariant) & subvariant != "NA subvariant" ) %>%
    group_by(location_name, subvariant) %>%
    summarize(median_IDR = median(idr_v5, na.rm = TRUE)) %>% 
    #mutate(subvariant = case_when(subvariant == "BA.4 BA.5" ~ "BA.4/BA.5",
    # .default = subvariant)) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    
    pivot_wider(. , names_from = subvariant, values_from = median_IDR)
  
  names(vartable)[1] <- "Country"

ggplot(df, aes(x=date, y=daily_cases_15dt_mill, color=location_name)) +
  geom_line()


daily_cases_15dt_mill = daily_cases_15dt * population_2023/1000000,
inf_mean_mill

mytable <- 
  bigfile %>% 
  ungroup() %>% 
  filter(location_name == "Japan" & date >= mdy("01-01-2022") & 
           !is.na(subvariant)) %>% 
  select(date, location_name, daily_cases_7dt, idr_v5, subvariant) %>%
  group_by(subvariant) %>%
  summarize(median_IDR = median(idr_v5, na.rm = TRUE)) %>% 
  filter(subvariant %in% c("BA.1", "BA.2", "BA.4 BA.5")) %>%
  mutate(subvariant = case_when(subvariant == "BA.4 BA.5" ~ "BA.4/BA.5",
                                .default = subvariant)) %>%
  mutate(across(where(is.numeric), round, 2)) 

maxy <- max(df2$daily_cases_7dt)
miny <- max(df2$daily_cases_7dt)*0.8
maxx <- max(df2$date)
minx <- maxx - 40


df2 <- bigfile %>% 
  ungroup() %>% 
  filter(location_name == "Japan" & date > mdy("01-01-2022")) %>% 
  select(date, location_name, daily_cases_7dt, idr_v5)

plot1 <- ggplot(df2, aes(x = date, y = daily_cases_7dt)) + geom_line() +
  labs(x = "Date", y = "Daily reported cases") +
  scale_y_continuous(labels = scales::label_number()) +
  ggtitle("Number of daily Reported Cases, Japan, 2022") #+
#  facet_wrap(~location_name, scales = "free_y")
#as.numeric(df2$date) %>% median()
#[1] 19165.5


tt <- ttheme_default(base_size = 6, base_colour = "black", base_family = "",
                     parse = FALSE, padding = unit(c(2, 2), "mm") )

plot1t <- 
  plot1 + annotation_custom(
    tableGrob( mytable, rows=NULL), xmax=maxx, xmin= minx, 
                          ymax = maxy, 
                          ymin=miny
                          )
grid.arrange(plot1t, plot1t, plot1t, plot1t)

ttheme_default(base_size = 12, base_colour = "black", base_family = "",
               parse = FALSE, padding = unit(c(4, 4), "mm"), ...)




df <- bigfile %>% 
  #ungroup() %>% 
  #filter(location_name == "United States of America") %>% 
  filter(continent == "Asia" & date > mdy("01-01-2022")) %>% 
  select(date, location_name, daily_cases_7dt, idr_v5)

ggplot(df, aes(x = date, y = daily_cases_7dt)) + geom_line() +
  
  labs(x = "Date", y = "Under reports for USA") +
  ggtitle("Number of cases over time") +
  facet_wrap(~location_name, scales = "free_y")

```


```

plot(df[[1]], df[[2]], type = "l")

ggplot(df, 
       aes(date(), under_report)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



  geom_line() +
  labs(x = "Date", y = "Under reports for USA") +
  ggtitle("Number of cases over time")



```


##Older code
```{r backup, echo=TRUE}
# View the imported data
View(country_groups)

# check overlaps
# need a clean list of countries by region using world bank format
# a clean list of US states
#All location names
location_vec <- data_under[["location_name"]] %>% unique()

#All countries from World Bank
world <- 
  country_groups %>% 
	filter(GroupName == "World") %>%
	select(CountryName) %>%
	as.vector() %>%
  unlist()

# How many countries are correctly spelt? 
length(location_vec)
intersect(world, location_vec) %>% length() #145

# A list of poor spellings and countries that do not have a model
world_mismatch <- 
  setdiff(world, location_vec) %>% sort() %>% as.data.frame()

location_mismatch <- 
  setdiff(location_vec, world) %>% sort() %>% as.data.frame()

#remove US states
location_mismatch2 <- setdiff(location_mismatch[[1]], as.data.frame(state.name)[[1]]) %>% as.data.frame()


#####################
length(state.name)

setdiff(as.data.frame(state.name)[[1]], location_mismatch[[1]])

```

```{r location_clean, echo=TRUE}
# need to make sure Georgia, USA is not mixed up with Republic of Georgia
data_under %>% 
  #filter(grepl('Macao', location_name)) %>% 
  select(location_name) %>% 
  #View() %>%
  #remove everything in parentheses
  mutate(location_clean = gsub("\\ \\(.*\\)", "", location_name)) %>%
  mutate(location_clean = gsub("^Macao.*", "Macao", location_clean)) %>%
  select(location_clean) %>% table() %>% View()
```
## Group IHME data by location and calculate summary statistics
```{r summary_table}
# 1) for each location summarize Max date, min date, mean, median, max, min, upper and lower quartile
summary_table <- data_under %>% 
  filter(!is.na(inf_mean))
  group_by(location_name) %>%
  summarize(
    n = sum(!is.na(inf_mean)),
    min_date = min(as.Date(date), na.rm = TRUE),
    max_date = max(as.Date(date), na.rm = TRUE),
    mean = mean(under_report, na.rm = TRUE),
    max = max(under_report, na.rm = TRUE),
    min = min(under_report, na.rm = TRUE),
    median = median(under_report, na.rm = TRUE),
    upper_quartile = quantile(under_report, 0.75,na.rm = TRUE),
    lower_quartile = quantile(under_report, 0.25, na.rm = TRUE)
  )

# View the resulting summary table
View(summary_table)
```

## Calculate breakpoints for time series data
```{r summary_table}
#To conduct statistical methods detection of breakpoints in time series data using Chow test, CUSUM test, and Pettitt's test, you can use the strucchange package in R. Here's an example code for each of the three methods:

#Chow test:
library(strucchange)
# create a time series object
ts_data <- ts(your_data, start = start_year, frequency = 12)

# perform Chow test
chow_test <- breakpoints(ts_data ~ 1)
summary(chow_test)

#CUSUM test:
# perform CUSUM test
cusum_test <- sctest(ts_data ~ 1, type = "CUSUM")
summary(cusum_test)

#Pettitt's test:
# perform Pettitt's test
pettitt_test <- sctest(ts_data ~ 1, type = "Pettitt")
summary(pettitt_test)
#Note that in the above code, your_data should be replaced with the name of your time series data, and start_year should be replaced with the start year of your time series. Also, you can adjust the frequency argument depending on the frequency of your time series (12 if it is monthly, 4 if it is quarterly, etc.).

```

## Including Plots



You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this: