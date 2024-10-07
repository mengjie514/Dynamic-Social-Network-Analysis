# Load necessary libraries
library(dplyr)

# Define file paths
file1 <- "G:/My Drive/PSIII-VI/PSVI/PSVI_Raw_2018Midterm_SNA_SentiGPT_New.csv"
file2 <- "G:/My Drive/PSIII-VI/PSVI/PSVI_Raw_2022Midterm_SNA_SentiGPT_New.csv"

# Import datasets
data_2018 <- read.csv(file1)
data_2022 <- read.csv(file2)

# Select the required columns for each dataset
selected_2018 <- data_2018 %>%
  select(screen_name_party, Group, StandardizedSentiment)

selected_2022 <- data_2022 %>%
  select(screen_name_party, Group, StandardizedSentiment)

# View the first few rows of each dataset
head(selected_2018)
head(selected_2022)

# Load necessary libraries
library(dplyr)

# Define a function to calculate standard error
standard_error <- function(x) {
  n <- sum(!is.na(x))  # Count non-NA observations
  if (n < 2) {
    return(NA)  # Return NA if fewer than 2 valid observations
  } else {
    return(sd(x, na.rm = TRUE) / sqrt(n))
  }
}

# Modify the StandardizedSentiment column for both datasets
# Recode: Positive = 1, Negative = -1, Neutral = 0
recoded_2018 <- selected_2018 %>%
  mutate(StandardizedSentiment = case_when(
    StandardizedSentiment == "Positive" ~ 1,
    StandardizedSentiment == "Negative" ~ -1,
    StandardizedSentiment == "Neutral" ~ 0
  ))

recoded_2022 <- selected_2022 %>%
  mutate(StandardizedSentiment = case_when(
    StandardizedSentiment == "Positive" ~ 1,
    StandardizedSentiment == "Negative" ~ -1,
    StandardizedSentiment == "Neutral" ~ 0
  ))

########################
# Descriptive Analysis #
########################
summary_2018 <- recoded_2018 %>%
  group_by(screen_name_party, Group) %>%
  summarise(
    mean_sentiment = mean(StandardizedSentiment, na.rm = TRUE),
    se_sentiment = standard_error(StandardizedSentiment)
  )

summary_2022 <- recoded_2022 %>%
  group_by(screen_name_party, Group) %>%
  summarise(
    mean_sentiment = mean(StandardizedSentiment, na.rm = TRUE),
    se_sentiment = standard_error(StandardizedSentiment)
  )

# View the summary for both datasets
print("Summary for 2018 Data:")
print(summary_2018)

print("Summary for 2022 Data:")
print(summary_2022)

##################################
# t-test & Mann-Whitney U test I #
##################################
perform_tests <- function(data, party) {
  # Filter data for the specified party
  party_data <- data %>%
    filter(screen_name_party == party)
  
  # Independent t-test for differences between Ingroup and Outgroup
  t_test_result <- t.test(StandardizedSentiment ~ Group, data = party_data)
  
  # Mann-Whitney U Test (Wilcoxon rank-sum test) for non-normal data
  mw_test_result <- wilcox.test(StandardizedSentiment ~ Group, data = party_data)
  
  return(list(t_test = t_test_result, mann_whitney = mw_test_result))
}

# Run the tests for both parties in the 2018 dataset
print("2018 Data: Democratic Party")
test_results_2018_D <- perform_tests(recoded_2018, "D")
test_results_2018_D

print("2018 Data: Republican Party")
test_results_2018_R <- perform_tests(recoded_2018, "R")
test_results_2018_R

# Run the tests for both parties in the 2022 dataset
print("2022 Data: Democratic Party")
test_results_2022_D <- perform_tests(recoded_2022, "D")
test_results_2022_D

print("2022 Data: Republican Party")
test_results_2022_R <- perform_tests(recoded_2022, "R")
test_results_2022_R

###################################
# t-test & Mann-Whitney U test II #
###################################
# Function to perform tests between 2018 and 2022 for ingroup and outgroup of a party 
perform_yearly_comparison <- function(data_2018, data_2022, party, group_type) {
  # Filter data for the specified party and group (Ingroup/Outgroup)
  data_2018_filtered <- data_2018 %>%
    filter(screen_name_party == party, Group == group_type)
  
  data_2022_filtered <- data_2022 %>%
    filter(screen_name_party == party, Group == group_type)
  
  # Combine data for both years and add a 'year' column
  combined_data <- bind_rows(
    data_2018_filtered %>% mutate(year = 2018),
    data_2022_filtered %>% mutate(year = 2022)
  )
  
  # Independent t-test for differences between 2018 and 2022
  t_test_result <- t.test(StandardizedSentiment ~ year, data = combined_data)
  
  # Mann-Whitney U Test (Wilcoxon rank-sum test) for non-normal data
  mw_test_result <- wilcox.test(StandardizedSentiment ~ year, data = combined_data)
  
  return(list(t_test = t_test_result, mann_whitney = mw_test_result))
}

# Run comparisons for Democratic Party
print("Democratic Party: Ingroup (2018 vs. 2022)")
test_results_D_Ingroup <- perform_yearly_comparison(recoded_2018, recoded_2022, "D", "Ingroup")
test_results_D_Ingroup

print("Democratic Party: Outgroup (2018 vs. 2022)")
test_results_D_Outgroup <- perform_yearly_comparison(recoded_2018, recoded_2022, "D", "Outgroup")
test_results_D_Outgroup

# Run comparisons for Republican Party
print("Republican Party: Ingroup (2018 vs. 2022)")
test_results_R_Ingroup <- perform_yearly_comparison(recoded_2018, recoded_2022, "R", "Ingroup")
test_results_R_Ingroup

print("Republican Party: Outgroup (2018 vs. 2022)")
test_results_R_Outgroup <- perform_yearly_comparison(recoded_2018, recoded_2022, "R", "Outgroup")
test_results_R_Outgroup