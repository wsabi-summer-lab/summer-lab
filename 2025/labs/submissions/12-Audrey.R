#############
### SETUP ###
#############
library(tidyverse)
library(splines)

# NBA
nba_data <- read_csv(file.choose())
nba_data <- nba_data %>%
  mutate(ppp = pts / possessions)

head(nba_data)

player_games <- nba_data %>% count(namePlayer)
eligible_players <- player_games %>% filter(n >= 15) %>% pull(namePlayer)
nba_filtered <- nba_data %>% filter(namePlayer %in% eligible_players)

# Task 1: Estimate mu and prior variance
player_avgs <- nba_filtered %>%
  group_by(namePlayer) %>%
  summarise(mean_ppp = mean(ppp, na.rm = TRUE), .groups = "drop")

mu_hat <- mean(player_avgs$mean_ppp)
nu2_hat <- var(player_avgs$mean_ppp)

# Task 2: Estimate sigma²
close_to_avg <- player_avgs %>%
  filter(abs(mean_ppp - mu_hat) < 0.05) %>%
  pull(namePlayer)

vi_data <- nba_filtered %>%
  filter(namePlayer %in% close_to_avg) %>%
  group_by(namePlayer) %>%
  summarise(var_ppp = var(ppp), .groups = "drop")

sigma2_hat <- mean(vi_data$var_ppp, na.rm = TRUE)

# Task 3: Estimate tau² 
tau2_grid <- 10^seq(-7, -3, length.out = 20)

get_rmse <- function(tau2) {
  player_rmses <- nba_filtered %>%
    group_by(namePlayer) %>%
    filter(n() >= 20) %>%
    group_split()
  
  rmse_list <- c()
  
  for (df in player_rmses) {
    df <- df %>% arrange(dateGame)
    n_games <- nrow(df)
    k <- floor(n_games / 2)
    
    if (k < 5) next
    
    mu_prev <- mu_hat
    for (j in 1:k) {
      X <- df$ppp[j]
      P <- df$possessions[j]
      w1 <- P / sigma2_hat
      w2 <- 1 / (if (j == 1) nu2_hat else tau2)
      mu_post <- (w1 * X + w2 * mu_prev) / (w1 + w2)
      mu_prev <- mu_post
    }
    
    future_vals <- df$ppp[(k + 1):n_games]
    rmse <- sqrt(mean((mu_post - future_vals)^2, na.rm = TRUE))
    rmse_list <- c(rmse_list, rmse)
  }
  
  if (length(rmse_list) == 0) return(NA)
  mean(rmse_list, na.rm = TRUE)
}

rmse_results <- tibble(
  tau2 = tau2_grid,
  avg_rmse = map_dbl(tau2_grid, get_rmse)
)

rmse_results <- rmse_results %>% filter(!is.na(avg_rmse))

tau2_hat <- rmse_results %>%
  filter(avg_rmse == min(avg_rmse)) %>%
  pull(tau2)

# Task 4: Plot RMSE
ggplot(rmse_results, aes(x = tau2, y = avg_rmse)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  labs(title = "Average RMSE by Tau²", x = expression(tau^2), y = "Avg RMSE") +
  theme_minimal()

# Task 5: Compute player trajectories
nba_results <- list()

for (p in unique(nba_filtered$namePlayer)) {
  df <- nba_filtered %>%
    filter(namePlayer == p) %>%
    arrange(dateGame)
  
  mu_vals <- c()
  mu_prev <- mu_hat
  
  for (i in 1:nrow(df)) {
    X <- df$ppp[i]
    P <- df$possessions[i]
    w1 <- P / sigma2_hat
    w2 <- 1 / (if (i == 1) nu2_hat else tau2_hat)
    mu_post <- (w1 * X + w2 * mu_prev) / (w1 + w2)
    mu_vals <- c(mu_vals, mu_post)
    mu_prev <- mu_post
  }
  
  df$mu_hat <- mu_vals
  nba_results[[p]] <- df
}

nba_final <- bind_rows(nba_results)

# Task 6: Plot player trajectories
available_players <- unique(nba_final$namePlayer)
n_to_sample <- min(5, length(available_players))

nba_final %>%
  group_by(namePlayer) %>%
  mutate(GameNum = row_number()) %>%
  filter(namePlayer %in% sample(available_players, n_to_sample)) %>%
  ggplot(aes(x = GameNum, y = mu_hat, color = namePlayer)) +
  geom_line() +
  labs(title = "Scoring Quality Over Time", x = "Game #", y = expression(hat(mu))) +
  theme_minimal()

# NFL MODEL
nfl_data <- read_csv(file.choose())
head(nfl_data)
# Task 1: Fit spline model
fg_model <- glm(fg_made ~ bs(ydl, df = 4), data = nfl_data, family = "binomial")
nfl_data <- nfl_data %>%
  mutate(P0 = predict(fg_model, type = "response"))

# Task 2: Calculate FGPA
nfl_data <- nfl_data %>%
  mutate(FGPA = fg_made - P0)

# Task 3: Calculate nfler quality
alpha <- 0.98

nfl_data <- nfl_data %>%
  arrange(kicker, season, week) %>%
  group_by(kicker) %>%
  mutate(KickNumber = row_number()) %>%
  mutate(KQ = {
    out <- numeric(n())
    out[1] <- FGPA[1]
    if (n() > 1) {
      for (i in 2:n()) {
        out[i] <- alpha * out[i - 1] + FGPA[i]
      }
    }
    out
  }) %>%
  ungroup()

# Task 4: Plot kicker trajectories
available_kickers <- unique(nfl_data$kicker)
n_kickers_to_sample <- min(4, length(available_kickers))

nfl_data %>%
  filter(kicker %in% sample(available_kickers, n_kickers_to_sample)) %>%
  ggplot(aes(x = KickNumber, y = KQ, color = kicker)) +
  geom_line() +
  labs(title = "Kicker Quality Over Time", x = "Kick #", y = "KQ") +
  theme_minimal()