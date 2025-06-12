#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(9)

# 28 red, 82 not red
skittles <- data.frame(
  color = c(rep("red", 28), rep("not_red", 82))
)
# calculate the true probability of getting a red skittle
p_red = sum(skittles$color == "red") / nrow(skittles)

# Construct a 95% Wald and Agresti-Coull confidence interval for the true probability of getting a red skittle
wald_lower_bound = p_red - qnorm(0.975) * sqrt((p_red * (1 - p_red)) / nrow(skittles))
wald_upper_bound = p_red + qnorm(0.975) * sqrt((p_red * (1 - p_red)) / nrow(skittles))
# print wald confidence interval and its length
wald_ci = c(wald_lower_bound, wald_upper_bound)
wald_length = wald_upper_bound - wald_lower_bound
cat("Wald Confidence Interval: [", wald_lower_bound, ",", wald_upper_bound, "] Length:", wald_length, "\n")

# Agresti-Coull confidence interval
p_hat_tilde <- (sum(skittles$color == "red")+2) / (nrow(skittles) +4)

agresti_coull_lower_bound = p_hat_tilde - qnorm(0.975) * sqrt((p_hat_tilde * (1 - p_hat_tilde)) / (nrow(skittles) + 4))
agresti_coull_upper_bound = p_hat_tilde + qnorm(0.975) * sqrt((p_hat_tilde * (1 - p_hat_tilde)) / (nrow(skittles) + 4))
# print agresti-coull confidence interval and its length
agresti_coull_ci = c(agresti_coull_lower_bound, agresti_coull_upper_bound)
agresti_coull_length = agresti_coull_upper_bound - agresti_coull_lower_bound
cat("Agresti-Coull Confidence Interval: [", agresti_coull_lower_bound, ",", agresti_coull_upper_bound, "] Length:", agresti_coull_length, "\n")

## Use the bootstrap to construct a 95% confidence interval.
# regenerate skittles data by sampling m observations from skittles with replacement. then, record the
# estimated p-hat from each sample. do this B times. record all the p-hat's.
B <- 1000 # number of bootstrap samples
bootstrap_samples <- replicate(B, {
  sample(skittles$color, size = nrow(skittles), replace = TRUE)
})
# calculate the proportion of red skittles in each bootstrap sample
bootstrap_proportions <- apply(bootstrap_samples, 2, function(sample) {
  sum(sample == "red") / length(sample)
})
# calculate the 2.5th and 97.5th percentiles of the bootstrap proportions
bootstrap_lower_bound <- quantile(bootstrap_proportions, 0.025)
bootstrap_upper_bound <- quantile(bootstrap_proportions, 0.975)
# print bootstrap confidence interval and its length
bootstrap_ci <- c(bootstrap_lower_bound, bootstrap_upper_bound)
bootstrap_length <- bootstrap_upper_bound - bootstrap_lower_bound
cat("Bootstrap Confidence Interval: [", bootstrap_lower_bound, ",", bootstrap_upper_bound, "] Length:", bootstrap_length, "\n")

# Compare the width of the bootstrap interval to the Wald and Agresti-Coull intervals
cat("Wald Interval Length:", wald_length, "\n")
cat("Agresti-Coull Interval Length:", agresti_coull_length, "\n")
cat("Bootstrap Interval Length:", bootstrap_length, "\n")

## bootstrap CI is widest
# but sample of 110 skittles is super small compared to population of all skittles in the world

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/09_nba-free-throws.csv", delim = ";")


