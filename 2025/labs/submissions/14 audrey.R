library(ggplot2)
library(rstan)
library(tidyverse)
set.seed(14)

nfl_data <- read_csv(file.choose())
head(nfl_data)


season_levels <- sort(unique(nfl_data$season)) 
team_levels <- sort(unique(c(nfl_data$home_team, nfl_data$away_team)))
team_lookup <- tibble(
  team = team_levels,
  team_idx = 1:length(team_levels)
)

nfl_data <- nfl_data %>%
  left_join(team_lookup, by = c("home_team" = "team")) %>%
  rename(H = team_idx) %>%
  left_join(team_lookup, by = c("away_team" = "team")) %>%
  rename(A = team_idx) %>%
  mutate(y = pts_H_minus_A) 
season_levels <- sort(unique(nfl_data$season))
nfl_data <- nfl_data %>% mutate(S = match(season, season_levels))

# Stan 
stan_data <- list(
  N_games = nrow(nfl_data),
  N_teams = length(team_levels),
  N_seasons = length(season_levels),
  y = nfl_data$y,
  H = nfl_data$H,
  A = nfl_data$A,
  S = nfl_data$S
)

# T1
print("Compiling Stan model...")
model <- stan_model("/Users/kuanai/Desktop/audrey_.stan")

print(model)
fit <- sampling(
  model,
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 14
)
print(fit, pars = c("alpha", "sigma_HFA", "beta_0", "sigma_games"))

# T2
print("Extracting home field advantage results...")

alpha_samples <- rstan::extract(fit, pars = "alpha_team")$alpha_team

#Summary
hfa_summary <- tibble(
  team = team_levels,
  mean_hfa = apply(alpha_samples, 2, mean),
  lower_95 = apply(alpha_samples, 2, quantile, 0.025),
  upper_95 = apply(alpha_samples, 2, quantile, 0.975)
) %>%
  arrange(desc(mean_hfa))

#HFA
p1 <- ggplot(hfa_summary, aes(x = reorder(team, mean_hfa), y = mean_hfa)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.3) +
  coord_flip() +
  labs(
    title = "Team-Specific HFA",
    x = "Team",
    y = "Home Field Advantage (α_j)"
  ) +
  theme_minimal()

print(p1)

# T3 
print("Extracting team strength results...")

betas_samples <- rstan::extract(fit, pars = "betas")$betas

n_teams <- length(team_levels)
n_seasons <- length(season_levels)

strength_summary <- tibble(
  team = rep(team_levels, n_seasons),
  season = rep(season_levels, each = n_teams),
  mean_strength = as.vector(apply(betas_samples, c(2,3), mean)),
  lower_95 = as.vector(apply(betas_samples, c(2,3), quantile, 0.025)),
  upper_95 = as.vector(apply(betas_samples, c(2,3), quantile, 0.975))
) %>%
  group_by(team) %>%
  summarise(
    overall_strength = mean(mean_strength),
    overall_lower = mean(lower_95),
    overall_upper = mean(upper_95),
    .groups = "drop"
  ) %>%
  arrange(desc(overall_strength))

p2 <- ggplot(strength_summary, aes(x = reorder(team, overall_strength), y = overall_strength)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = overall_lower, ymax = overall_upper), width = 0.3) +
  coord_flip() +
  labs(
    title = "Team Strength",
    x = "Team",
    y = "Team Strength (β_j)"
  ) +
  theme_minimal()

print(p2)

#lab ans
cat("\n=== T2 ===\n")
cat("\nLargest hfa:\n")
print(head(hfa_summary[c("team", "mean_hfa")], 5))

cat("\nTeams with smallest hfa:\n")
print(tail(hfa_summary[c("team", "mean_hfa")], 5))

cat("\n=== T3 ===\n")
cat("\nstrongest:\n")
print(head(strength_summary[c("team", "overall_strength")], 5))

cat("\nWeak teams:\n")
print(tail(strength_summary[c("team", "overall_strength")], 5))