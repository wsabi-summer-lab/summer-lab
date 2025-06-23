#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "glmnet", "tidyverse"))
library(ggplot2)
library(glmnet)
library(tidyverse)
library(broom)
library(tibble)

# set seed
set.seed(15)

####################
### NBA LINEUPS ###
####################

# load NBA lineups data
nba_lineups = readRDS("../data/15_nba-lineups.rds")
# preview data
head(nba_lineups)
names(nba_lineups)

####################
# one-hot encoding

## i think each lineup (offence & defense) has five players
# possession ID
nba_lineups <- nba_lineups %>%
  mutate(poss_id = row_number())

# offensive 
offensive_long <- nba_lineups %>%
  select(poss_id, pts_poss, lineup_team) %>%
  separate_rows(lineup_team, sep = ", ") %>%
  mutate(lineup_team = str_remove(lineup_team, "^[0-9]+ "),  # Remove numeric ID
         var = paste0(lineup_team, "_off"),
         value = 1)

# defensive
defensive_long <- nba_lineups %>%
  select(poss_id, lineup_opp) %>%
  separate_rows(lineup_opp, sep = ", ") %>%
  mutate(lineup_opp = str_remove(lineup_opp, "^[0-9]+ "),  # Remove numeric ID
         var = paste0(lineup_opp, "_def"),
         value = 1)

# combine off and def
combined <- bind_rows(
  offensive_long %>% select(poss_id, var, value),
  defensive_long %>% select(poss_id, var, value)
)

# pivot wider
X_wide <- combined %>%
  pivot_wider(names_from = var, values_from = value, values_fill = 0)

# add outcome and intercept
nba_design <- nba_lineups %>%
  select(poss_id, pts_poss) %>%
  left_join(X_wide, by = "poss_id") %>%
  mutate(intercept = 1) %>%
  relocate(intercept, .before = pts_poss)

####################
# OLS model
ols_data <- nba_design %>% select(-poss_id)

ols_model <- lm(pts_poss ~ . , data = ols_data) # this takes a while
summary(ols_model)

####################
# ridge regression model

# model matrix and outcome vector
X <- nba_design %>% select(-poss_id, -pts_poss) %>% as.matrix()
y <- nba_design$pts_poss

lambdas <- 10^seq(-3, 3, by = 0.2)

ridge_model <- cv.glmnet(  # also takes a while to run
  x = X,
  y = y,
  alpha = 0,
  lambda = lambdas,
  nfolds = 5,
  standardize = FALSE
)

best_lambda <- ridge_model$lambda.min
print(best_lambda)

ridge_coefs <- coef(ridge_model, s = "lambda.min")
plot(ridge_model)

####################
# compare models
# tidy OLS coefs
ols_coefs <- tidy(ols_model) %>%
  select(term, estimate) %>%
  rename(ols = estimate)
ols_coefs$term <- gsub("`", "", ols_coefs$term)

# tidy ridge coefs
ridge_coefs_df <- as.matrix(ridge_coefs) %>%
  as.data.frame() %>%
  rownames_to_column("term")

colnames(ridge_coefs_df)

names(ridge_coefs_df)[2] <- "ridge"

# join coefs
combined_coefs <- full_join(ols_coefs, ridge_coefs_df, by = "term") %>%
  filter(term != "(Intercept)")  # optional: exclude intercept from plot

# plot ridge vs. ols
combined_coefs <- combined_coefs %>%
  filter(term != "(Intercept)") %>%
  filter(!is.na(ols), !is.na(ridge)) %>%
  mutate(player = gsub("_off|_def", "", term))

long_coefs <- combined_coefs %>%
  select(player, ols, ridge) %>%
  pivot_longer(cols = c(ols, ridge), names_to = "model", values_to = "coefficient")

# wow thats a lot of players
ggplot(long_coefs, aes(x = coefficient, y = reorder(player, coefficient), fill = model)) +
  geom_col(position = "dodge") +
  labs(
    title = "OLS vs Ridge Coefficients by Player",
    x = "Coefficient",
    y = "Player"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7)  # adjust for readability
  )

# 20 random players
sample_players <- combined_coefs %>%
  distinct(player) %>%
  slice_sample(n = 20)

long_coefs_sample <- combined_coefs %>%
  filter(player %in% sample_players$player) %>%
  select(player, ols, ridge) %>%
  pivot_longer(cols = c(ols, ridge), names_to = "model", values_to = "coefficient")

ggplot(long_coefs_sample, aes(x = coefficient, y = reorder(player, coefficient), fill = model)) +
  geom_col(position = "dodge") +
  labs(
    title = "Sample of 20 Players: OLS vs Ridge Coefficients",
    x = "Coefficient",
    y = "Player"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))
