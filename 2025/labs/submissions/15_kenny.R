library(glmnet)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

set.seed(15)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

nba_lineups = readRDS("15_nba-lineups.rds")

head(nba_lineups)

nba_lineups <- nba_lineups %>%
  mutate(poss_id = row_number())

offensive_long <- nba_lineups %>%
  select(poss_id, pts_poss, lineup_team) %>%
  separate_rows(lineup_team, sep = ", ") %>%
  mutate(
    player_name = str_remove(lineup_team, "^[0-9]+\\s*"),  # Remove jersey number
    value = 1
  ) %>%
  select(poss_id, pts_poss, player_name, value)

defensive_long <- nba_lineups %>%
  select(poss_id, lineup_opp) %>%
  separate_rows(lineup_opp, sep = ", ") %>%
  mutate(
    player_name = str_remove(lineup_opp, "^[0-9]+\\s*"),
    value = -1
  ) %>%
  select(poss_id, player_name, value)

players_long <- bind_rows(offensive_long, defensive_long)

players_wide <- players_long %>%
  pivot_wider(names_from = player_name, values_from = value, values_fill = 0)

player_columns <- setdiff(names(players_wide), "pts_poss")

player_counts <- sapply(players_wide[player_columns], function(col) sum(col != 0))

player_counts_df <- data.frame(
  player_name = names(player_counts),
  possessions_played = as.integer(player_counts)
)

eligible_players <- player_counts_df %>%
  filter(possessions_played >= 200) %>%
  pull(player_name)

players_wide_filtered <- players_wide %>%
  select(pts_poss, all_of(eligible_players))

# OLS Model

ols_model <- lm(pts_poss ~ ., data = players_wide_filtered)

summary(ols_model)

coefs <- coef(summary(ols_model))
coefs_df <- as.data.frame(coefs)
coefs_df$Player <- rownames(coefs_df)
coefs_df <- coefs_df %>% 
  filter(Player != "(Intercept)") %>%
  arrange(desc(Estimate))

head(coefs_df, 10)
tail(coefs_df, 10)

# Ridge Model

lambdas <- 10^seq(-3, 3, by = 0.2)

x <- as.matrix(players_wide_filtered %>% 
                 select(-pts_poss, -poss_id))
y <- players_wide_filtered$pts_poss

valid_idx <- which(!is.na(y) & !is.infinite(y))

x_clean <- x[valid_idx, , drop = FALSE]
y_clean <- y[valid_idx]

ridge_model = cv.glmnet(x = x_clean, y = y_clean,
                        nfolds = 5, alpha = 0, family = "gaussian",
                        lambda = lambdas, standardize = FALSE)

ridge_model$lambda.min
plot(ridge_model)

# Comparison

ridge_coefs <- coef(ridge_model, s = "lambda.min")
ridge_df <- as.data.frame(as.matrix(ridge_coefs)) %>%
  tibble::rownames_to_column("Player") %>%
  rename(Ridge_Estimate = s1)

head(ridge_df)

ols_df <- coefs_df %>%
  select(Player, OLS_Estimate = Estimate)

ols_df <- ols_df %>%
  mutate(Player = str_remove_all(Player, "`"))

head(ols_df)

comparison_df <- inner_join(ols_df, ridge_df, by = "Player")

ggplot(comparison_df, aes(x = OLS_Estimate, y = Ridge_Estimate)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "OLS vs Ridge Player Coefficients",
       x = "OLS Estimate",
       y = "Ridge Estimate")

