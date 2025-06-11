library(tidyverse)
#Task 1
diving <- read_csv(file.choose())

head(diving)
diving <- diving %>%
  mutate(match = if_else(Country == JCountry, 1, 0))
diving <- diving %>%
  group_by(Event, Diver, DiveNo) %>%
  mutate(mean_score = mean(JScore),
         discrepancy = JScore - mean_score) %>%
  ungroup()

judge_dod <- diving %>%
  group_by(Judge) %>%
  summarise(
    matched = mean(discrepancy[match == 1]),
    non_matched = mean(discrepancy[match == 0]),
    DoD = matched - non_matched,
    n_matched = sum(match),
    n_nonmatched = sum(match == 0)
  )
permute_DoD <- function(judge_data, n_perm = 1000) {
  observed_DoD <- mean(judge_data$discrepancy[judge_data$match == 1]) -
    mean(judge_data$discrepancy[judge_data$match == 0])
  
  n_match <- sum(judge_data$match)
  
  perm_dods <- replicate(n_perm, {
    permuted_match <- sample(judge_data$match, replace = FALSE)
    mean(judge_data$discrepancy[permuted_match == 1]) -
      mean(judge_data$discrepancy[permuted_match == 0])
  })
  p_val <- mean(abs(perm_dods) >= abs(observed_DoD))
  return(p_val)
}
judge_p <- diving %>%
  group_by(Judge) %>%
  nest() %>%
  mutate(
    p_value = map_dbl(data, ~permute_DoD(.x, n_perm = 1000))
  ) %>%
  select(Judge, p_value)
final_result <- left_join(judge_dod, judge_p, by = "Judge")
print(final_result)

#Task2
tto <- read_csv(file.choose()) 
tto <- tto %>%
  mutate(
    y = EVENT_WOBA_19,
    ti = BATTER_SEQ_NUM,
    twoTTO = if_else(ti >= 2, 1, 0),
    threeTTO = if_else(ti >= 3, 1, 0),
    hand = HAND_MATCH,
    home = BAT_HOME_IND
  )
model1 <- lm(y ~ twoTTO + threeTTO + hand + home, data = tto)
model2 <- lm(y ~ ti + twoTTO + threeTTO + hand + home, data = tto)
summary(model1)
summary(model2)

