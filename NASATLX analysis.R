  #Elliptical speech NASA-TLX analysis
  # 09-13-24

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

TLX <- read.csv('/Users/coletrent/Library/CloudStorage/Box-Box/BRAiN Lab/current projects/elliptical-speech-project/code/NASATLX analysis/NASATLX.csv')

# Assuming your dataframe is named TLX
TLX_summary <- TLX %>%
  group_by(Session) %>%
  summarise(
    Avg_Mental_Demand = mean(Mental_Demand, na.rm = TRUE),
    Avg_Success = mean(Success, na.rm = TRUE),
    Avg_Hard = mean(Hard, na.rm = TRUE),
    Avg_Physical = mean(Physical, na.rm = TRUE),
    Avg_Hurried = mean(Hurried, na.rm = TRUE),
    Avg_Mental_Feel = mean(Mental_Feel, na.rm = TRUE)
  )

# Print the summary
print(TLX_summary)

ggplot(TLX_summary, aes(x = as.factor(Session), y = Avg_Mental_Demand)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Mental Demand by Session", x = "Session", y = "Mental Demand")



# Assuming TLX_summary is your dataframe
# Reshaping the data into long format
TLX_long <- TLX %>%
  pivot_longer(cols = c(Mental_Demand, Success, Hard, Physical, Hurried, Mental_Feel),
               names_to = "Variable",
               values_to = "Value")

# Plotting all variables in one boxplot
ggplot(TLX_long, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Multiple Variables", x = "Variables", y = "Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Session)


# Running ANOVA for each variable
anova_results <- list()

variables <- c("Mental_Demand", "Success", "Hard", "Physical", "Hurried", "Mental_Feel")

for (var in variables) {
  formula <- as.formula(paste(var, "~ Session"))
  anova_results[[var]] <- aov(formula, data = TLX)
}

# Display ANOVA summaries for each variable
for (var in variables) {
  cat("\nANOVA for", var, ":\n")
  print(summary(anova_results[[var]]))
}
