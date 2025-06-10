#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(splines)
library(tidyverse)


#########################
### PERMUTATION TESTS ###
#########################

# load data
diving_data = read_csv("../data/07_diving.csv")
names(diving_data)

# find number of unique judges
num_judges = length(unique(diving_data$Judge)) # 25

# for each judge (using a for loop), make another dataframe with all rows for that judge
# then inside the for loop: for each judge, run a permutation test by randomly permuting the "country" column 10,000 times. each time this happens, 
# record the mean score for that particular "DiveNo" from the entire dataset. then, find the 
# deviation from the judge's for that "DiveNo" and compare it to the original mean score.
# find the average deviation for this judge for divers whose "Country" matches the judge country "JCountry"
# and find another average from the deviations where "Country" is different from "JCountry"
# then, for each judge, record (1) number of matched dives (from same country), (2) average deviation for those matched dives,
# (3) number of unmatched dives (from different country), and (4) average deviation for those unmatched dives.
# fianlly, find the p-value of the judge's actual deviation from the mean score for matched dives vs. the mean score for unmatched dives. 
# create a function to run the permutation test for each judge

# first, group by DiveNo, Round, and Diver. then, find the mean score for each of these rows.
# then, run the permutation tests as described above for each judge.

dives_grouped <- diving_data %>%
  group_by(DiveNo, Round, Diver) %>%
  summarise(mean_score = mean(JScore, na.rm = TRUE), .groups = 'drop') %>% 
  # add the rest of the original dataframe back in
  left_join(diving_data, by = c("DiveNo", "Round", "Diver")) %>% 
  mutate(discrepancy = mean_score - JScore)

for (judge in unique(diving_data$Judge)[1]) {
  judge_data <- dives_grouped %>% filter(Judge == judge)
  
  # add match or no match
  judge_data <- judge_data %>%
    mutate(match = ifelse(Country == JCountry, "matched", "unmatched"))
  
  # compute judge's mean discrepancy from mean_score for matched and unmatched dives (new columns for both)
  judge_data <- judge_data %>%
    group_by(DiveNo, match) %>%
    summarise(mean_discrepancy = mean(discrepancy, na.rm = TRUE), .groups = 'drop')
  
  # record judge's average of all the "mean_discrepancy" for matched and unmatched dives
  matched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$match == "matched"], na.rm = TRUE)
  unmatched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$match == "unmatched"], na.rm = TRUE)
  actual_diff <- matched_avg_discrepancy - unmatched_avg_discrepancy
  
  # then, perform permutation test by randomly permuting the "match" column 10,000 times and storing the 
  # average of all the "mean_discrepancy" for matched and unmatched dives each 10,000 times (maybe make a new temp dataframe for this)
  num_permutations <- 50
  # make new dataframe to store the permuted discrepancies
  permuted_discrepancies <- data.frame(
    matched_avg = numeric(num_permutations),
    unmatched_avg = numeric(num_permutations),
    diff = numeric(num_permutations)
  )
  
  for (i in 1:num_permutations) {
    # set.seed(i)
    permuted_match <- sample(judge_data$match)
    judge_data$permuted_match <- permuted_match
    
    permuted_discrepancies[i,1] <- mean(judge_data$mean_discrepancy[judge_data$permuted_match == "matched"], na.rm = TRUE)
    permuted_discrepancies[i,2] <- mean(judge_data$mean_discrepancy[judge_data$permuted_match == "unmatched"], na.rm = TRUE)
    permuted_discrepancies[i,3] <- permuted_discrepancies[i,1] - permuted_discrepancies[i,2]
    
    temp_matched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$permuted_match == "matched"], na.rm = TRUE)
    temp_unmatched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$permuted_match == "unmatched"], na.rm = TRUE)
  }
  
  # compute p-value that judge's actual_diff is equal to what it is, given the distribution of numbers
  # in the permuted_discrepancies$diff column
  
  # count number of rows where actual_diff >= permuted_discrepancies$diff
  matched_avg_discrepancy <- mean(judge_data$mean_discrepancy[judge_data$match == "matched"], na.rm = TRUE)
  
  # calculate p-values for 
  p_value_matched <- sum(permuted_discrepancies$matched_avg >= matched_avg_discrepancy) / num_permutations
  p_value_unmatched <- sum(permuted_discrepancies$unmatched_avg >= unmatched_avg_discrepancy) / num_permutations
  
  
}

  
  
  
  
# Function to run permutation test for a specific judge
run_permutation_test <- function(data, judge_name, num_permutations = 10000) {
  judge_data <- data %>% filter(Judge == judge_name)
  
  # Get unique DiveNo for the judge
  unique_dives <- unique(judge_data$DiveNo)
  
  matched_deviations <- c()
  unmatched_deviations <- c()
  
  for (dive in unique_dives) {
    # Get the actual score for this dive
    actual_score <- mean(judge_data$Score[judge_data$DiveNo == dive])
    
    # Permute the country column
    permuted_scores <- replicate(num_permutations, {
      permuted_countries <- sample(data$Country)
      mean(data$Score[data$DiveNo == dive & data$Country == permuted_countries])
    })
    
    # Calculate deviations
    matched_deviation <- abs(actual_score - mean(permuted_scores[data$Country == judge_data$JCountry[1]]))
    unmatched_deviation <- abs(actual_score - mean(permuted_scores[data$Country != judge_data$JCountry[1]]))
    
    matched_deviations <- c(matched_deviations, matched_deviation)
    unmatched_deviations <- c(unmatched_deviations, unmatched_deviation)
  }
  
  # Calculate p-values
  p_value_matched <- sum(matched_deviations >= mean(matched_deviations)) / num_permutations
  p_value_unmatched <- sum(unmatched_deviations >= mean(unmatched_deviations)) / num_permutations
  
  return(list(
    matched_count = length(matched_deviations),
    matched_avg_dev = mean(matched_deviations),
    unmatched_count = length(unmatched_deviations),
    unmatched_avg_dev = mean(unmatched_deviations),
    p_value_matched = p_value_matched,
    p_value_unmatched = p_value_unmatched
  ))
}
# Run the permutation test for each judge and store results
results <- lapply(unique(diving_data$Judge), function(judge) {
  run_permutation_test(diving_data, judge)
})
# Convert results to a data frame
results_df <- do.call(rbind, lapply(unique(diving_data$Judge), function(judge, res) {
  c(Judge = judge, res[[judge]])
}, res = results))
results_df <- as.data.frame(results_df, stringsAsFactors = FALSE)
# Convert numeric columns to appropriate types
results_df$matched_count <- as.numeric(results_df$matched_count)
results_df$matched_avg_dev <- as.numeric(results_df$matched_avg_dev)
results_df$unmatched_count <- as.numeric(results_df$unmatched_count)
results_df$unmatched_avg_dev <- as.numeric(results_df$unmatched_avg_dev)
results_df$p_value_matched <- as.numeric(results_df$p_value_matched)
results_df$p_value_unmatched <- as.numeric(results_df$p_value_unmatched)



############################
### PARAMETRIC INFERENCE ###
############################

# load data
mlb_data = read_csv("../data/07_tto.csv")