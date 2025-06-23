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

ridge_model <- cv.glmnet(
  x = X,
  y = y,
  alpha = 0,                # Ridge regression
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

# tidy ridge coefs
ridge_coefs_df <- as.matrix(ridge_coefs) %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  rename(ridge = `1`)

# join coefs
combined_coefs <- full_join(ols_coefs, ridge_coefs_df, by = "term") %>%
  filter(term != "(Intercept)")  # optional: exclude intercept from plot

# plot ridge vs. ols
ggplot(combined_coefs, aes(x = ols, y = ridge)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Ridge vs OLS Coefficients",
    x = "OLS Coefficient",
    y = "Ridge Coefficient"
  ) +
  theme_minimal()