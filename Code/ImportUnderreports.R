# April 2023: Undercounting COVID Cases
# clear R's brain
rm( list = ls())

# load libraries
preq = c( "dplyr", "ggplot2", "ggfortify", "readxl", "readstata13", 
          "readr", "stats", "tidyverse", "haven", "Matrix", "foreign")
for (y in preq) install.packages(y, dep = TRUE)
sapply(preq, library, character.only=T)

# Set the directory where the files are located
setwd("G:/My\ Drive/Documents/R_github/underreporting")

# Create a list of the file names
files <- list.files(path = "../Data/IHME/",  pattern = ".csv")
# Use grep() function to select strings starting with "myfile"
selected <- files[grep("^data_download", files)]
selected <- week <- paste( "../Data/IHME/", selected, sep="")
# View the selected strings
selected
# Read in the files and store in a list
data_list <- lapply(selected, read.csv)

View(data_list[[1]])
#lapply(data_list, colnames)

#check if all columns are identical
all(
  colnames(data_list[[1]]) == colnames(data_list[[2]]) 
  )
all(
  colnames(data_list[[1]]) == colnames(data_list[[3]])
  )

# Bind the data frames into a single data frame
data_stack <- do.call(rbind, data_list)

View(data_stack)
# need to make sure Georgia, USA is not mixed up with Republic of Georgia
data_stack[["location_name"]] %>% unique() %>% sort()

# generate under_reporting variabl[e
data_stack["under_report"] <- 
  data_stack["inf_mean"] / data_stack["cases_mean"]

# Calculate column C = A/B only if A and B are both non-missing and not zero
data_under <- data_stack %>%
  mutate(
    under_report = 
      ifelse(!is.na(inf_mean) & inf_mean != 0 & 
                 !is.na(cases_mean) & cases_mean != 0, 
               inf_mean/cases_mean, NA)
    )
#To do:
# 1) Max date and min date for each location
# 2) summarize mean, median, max, min, upper and lower quartile for each location
# 3) Generate table with #1 and #2 above
# Group the data by location and calculate summary statistics
summary_table <- data_under %>%
  group_by( {{ location_name }}) %>%
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

#load World bank data
# Define the file path and name
file_WB <- "../Data/WorldBank/CLASS.xlsx"
# Read the "Group" tab into a data frame
group_data <- read_excel(file_WB, sheet = "Groups")
# View the imported data
View(group_data)

# Use grep() function to select strings starting with "myfile"
selected <- files[grep("^data_download", files)]
# View the selected strings
selected

#merge world bank data with IHME
# create a variable for country, continent, region1, region2, region3


/*

# Create an empty list to store the data
data_list <- list()

# Loop through the file names and read in each file
for (file in files) {
  data <- read.csv(file)
  data_list[[file]] <- data
}

# Example list of strings
mylist <- c("myfile1.txt", "myfile2.csv", "yourfile1.txt", "yourfile2.csv")


selected

# Example list of files
files <- list("file1.csv", "file2.csv", "file3.csv")

# Merge the files by column
merged_data <- Reduce(function(x, y) merge(x, y, by = "column_name"), data_list)

# View the merged data frame
merged_data