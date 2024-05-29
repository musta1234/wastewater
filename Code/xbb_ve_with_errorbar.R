
##############################################################################
data <- read.csv(paste0(getwd(), "/data_downloads/flu_covid_usa.csv"))

print(data)


# Reshape the data to long format
data_long <- data %>%
  pivot_longer(cols = c("Covid", "Influenza"), names_to = "Virus", values_to = "Value")
print(data_long)
data_long$date <- as.Date(data_long$date)
# Plot the data

# Plot the data
# Plot the data
ggplot(data_long, aes(x = date, y = Value, color = Virus, group = Virus)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 2) +  # Add points
  labs(title = "Influenza and Updated 2023-2024 COVID-19 Vaccine Coverage, 2023-2024, 
       Adults 18 Years and Older, United States",
       x = "Current Season Week Ending Date",
       y = "Percent vaccinated",
       color = "Virus") +
  ylim(0, 100) +  # Set y-axis range from 0 to 100
  scale_x_date(date_labels = "%m=%d-%Y", date_breaks = "1 months") +  # Format x-axis date labels and set breaks
  theme_minimal() +
  theme(
    text = element_text(size = 12), 
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  )


##############################################################################
data <- read.csv(paste0(getwd(), "/data_downloads/xbb_ve_andersson.csv"))

head(data)

#import the dataset above

library(ggplot2)


# Assuming the data is already loaded into the 'data' dataframe
data$variant <- factor(data$variant, levels = c("Overall", "XBB", "JN.1"))
data$outcome <- factor(data$outcome, levels = c("Hospitalization", "Death"))

# Create the dot plot with error bars grouped by outcome
# Create the dot plot with error bars, data labels, and fixed y-axis range
ggplot(data, aes(x = variant, y = ve, color = variant)) +
  geom_point(size = 5, position = position_dodge(width = 0.3)) +  # Add points
  geom_text(aes(label = ve), hjust = -0.7, position = position_dodge(width = 0.3)) +  # Add data labels
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.3)) +  # Add error bars
  facet_grid(~ outcome, scales = "fixed") +  # Facet by outcome with fixed scales
  theme_minimal() +  # Use a minimal theme
  labs(#title = "VE Estimates with 95% CI by Variant and Outcome", 
    x = "Variant", 
    y = "Vaccine effectiveness, %") +  # Add labels
  theme(text = element_text(size = 12), legend.position = "bottom") +  # Adjust text size and position legend
  guides(color = guide_legend(title = NULL)) +  # Remove legend title
  ylim(0, 100)  # Set y-axis range from 0 to 100

