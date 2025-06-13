library(tidyverse)

# Parameters
probs <- c(0.55, 0.25, 0.1)
alphas <- c(3, 3.2, 8)
M <- 100   # number of simulations
N <- 1000  # number of bets per simulation

# Kelly bet sizes (fraction of bankroll)
kelly_bets <- pmax(0, (probs * alphas - 1) / (alphas - 1))

# Normalize if total bet > 1
if (sum(kelly_bets) > 1) {
  kelly_bets <- kelly_bets / sum(kelly_bets)
}

# Only +EV bets strategy
evs <- probs * alphas - 1
plus_ev_bets <- ifelse(evs > 0, kelly_bets, 0)
if (sum(plus_ev_bets) > 0) {
  plus_ev_bets <- plus_ev_bets / sum(plus_ev_bets)
}

# Simulation function for bankroll and log_bankroll
simulate_bankrolls <- function(fractions, strategy_name, M, N) {
  res <- vector("list", M)
  for (m in seq_len(M)) {
    bankroll <- 1
    logs <- numeric(N)
    raw <- numeric(N)
    for (n in seq_len(N)) {
      winner <- sample(1:3, size = 1, prob = probs)
      total_bet <- sum(fractions)
      gain <- 1 - total_bet + fractions[winner] * alphas[winner]
      bankroll <- bankroll * gain
      logs[n] <- log(bankroll)
      raw[n] <- bankroll
    }
    res[[m]] <- tibble(
      step = 1:N,
      log_bankroll = logs,
      bankroll = raw,
      sim = m,
      strategy = strategy_name
    )
  }
  bind_rows(res)
}

set.seed(42)
full_kelly_sim <- simulate_bankrolls(kelly_bets, "Full Kelly Classic", M, N)
plus_ev_sim <- simulate_bankrolls(plus_ev_bets, "Only +EV Classic", M, N)

# Combine all simulations
all_sim <- bind_rows(full_kelly_sim, plus_ev_sim)

# Prepare data for faceted plot (unlogged and logged)
facet_data <- all_sim %>%
  pivot_longer(cols = c(bankroll, log_bankroll),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         bankroll = "Unlogged Bankroll",
                         log_bankroll = "Log Bankroll"))

# Facet grid plot: 4 panels (metric × strategy)
facet_plot_4 <- facet_data %>%
  ggplot(aes(x = step, y = value, color = strategy, group = interaction(strategy, sim))) +
  geom_line(alpha = 0.3) +
  facet_grid(metric ~ strategy, scales = "free_y") +
  labs(title = "Bankroll Evolution: Logged and Unlogged by Strategy",
       x = "Race",
       y = "Value",
       color = "Strategy") +
  theme_minimal() +
  theme(legend.position = "none")

print(facet_plot_4)

# Calculate average log bankroll at final bet (step N)
avg_log_bankroll <- all_sim %>%
  filter(step == N) %>%
  group_by(strategy) %>%
  summarise(
    avg_log_bankroll = mean(log_bankroll),
    .groups = "drop"
  ) %>%
  mutate(
    avg_bankroll = exp(avg_log_bankroll)
  )

print(avg_log_bankroll)

# Print best strategy info and difference
best_strategy <- avg_log_bankroll %>%
  filter(avg_bankroll == max(avg_bankroll))

diff_dollars <- diff(avg_log_bankroll$avg_bankroll)

cat("Strategy yielding more wealth on average:", best_strategy$strategy, "\n")
cat("Average bankroll:", round(best_strategy$avg_bankroll, 4), "dollars\n")
cat("Difference between strategies:", round(abs(diff_dollars), 4), "dollars\n")

# Plot average bankroll growth over all simulations (exponentiated mean log bankroll)
avg_log_growth <- all_sim %>%
  group_by(strategy, step) %>%
  summarise(avg_log_bankroll = mean(log_bankroll), .groups = "drop")

ggplot(avg_log_growth, aes(x = step, y = exp(avg_log_bankroll), color = strategy)) +
  geom_line(size = 1) +
  labs(
    title = "Average Bankroll Growth Across Simulations",
    subtitle = paste0("After ", N, " bets"),
    x = "Number of Bets",
    y = "Average Bankroll (dollars)",
    color = "Strategy"
  ) +
  theme_minimal()
