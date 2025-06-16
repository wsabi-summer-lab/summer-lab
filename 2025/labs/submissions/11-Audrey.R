#############
### SETUP ###
#############

library(tidyverse)
library(boot)
set.seed(11)
nba_data <- read_delim(file.choose(), delim = ";") %>%
  mutate(
    G = as.numeric(G),
    FT = as.numeric(FT),
    FTA = as.numeric(FTA),
    FT_total = round(FT * G),
    FTA_total = round(FTA * G)
  ) %>%
  group_by(Player) %>%
  summarise(
    FT_total = sum(FT_total, na.rm = TRUE),
    FTA_total = sum(FTA_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(FTA_total >= 25) %>%
  mutate(
    FT_percent = FT_total / FTA_total,
    Player = as.factor(Player)
  )

W <- sum(nba_data$FT_total)
L <- sum(nba_data$FTA_total) - W
n <- W + L
p_hat <- W / n


alpha_vals <- seq(0.5, 5, length.out = 10)
beta_vals <- seq(0.5, 5, length.out = 10)
prior_grid <- expand.grid(alpha = alpha_vals, beta = beta_vals)

posterior_results <- prior_grid %>%
  pmap_dfr(function(alpha, beta) {
    nba_data %>%
      mutate(
        alpha = alpha,
        beta = beta,
        lower_post = qbeta(0.025, alpha + W, beta + L),
        upper_post = qbeta(0.975, alpha + W, beta + L),
        width_post = upper_post - lower_post,
        prior_label = paste0("Beta(", round(alpha, 2), ", ", round(beta, 2), ")")
      ) %>%
      select(Player, alpha, beta, prior_label,
             lower_post, upper_post, width_post)
  })

# Wald CI
wald_ci <- nba_data %>%
  mutate(
    se_wald = sqrt(p_hat * (1 - p_hat) / (W + L)),
    lower_wald = p_hat - 1.96 * se_wald,
    upper_wald = p_hat + 1.96 * se_wald,
    width_wald = upper_wald - lower_wald
  )

# AC CI
z <- 1.96
wald_ci <- wald_ci %>%
  mutate(
    p_tilde = (W + z^2 / 2) / (W + L + z^2),
    se_tilde = sqrt(p_tilde * (1 - p_tilde) / (W + L + z^2)),
    lower_agresti = p_tilde - z * se_tilde,
    upper_agresti = p_tilde + z * se_tilde,
    width_agresti = upper_agresti - lower_agresti
  )

# Bootstrap CI  
bootstrap_ci <- function(successes, trials, n_boot = 250) {
  boot_samples <- replicate(n_boot, rbinom(1, trials, successes / trials))
  boot_props <- boot_samples / trials
  quantile(boot_props, probs = c(0.025, 0.975))
}

# ALL BOOTS
bootstrap_results <- nba_data %>%
  rowwise() %>%
  mutate(
    boot_ci = list(bootstrap_ci(W, W + L)),
    lower_boot = boot_ci[[1]][1],
    upper_boot = boot_ci[[1]][2],
    width_boot = upper_boot - lower_boot
  ) %>%
  ungroup()

interval_widths <- wald_ci %>%
  select(Player, width_wald, width_agresti) %>%
  left_join(bootstrap_results %>% select(Player, width_boot), by = "Player") %>%
  left_join(posterior_results %>%
              group_by(Player) %>%
              summarise(width_bayes = mean(width_post), .groups = "drop"), 
            by = "Player") %>%
  pivot_longer(cols = -Player, names_to = "method", values_to = "width") %>%
  mutate(method = recode(method,
                         width_wald = "Wald",
                         width_agresti = "Agresti-Coull",
                         width_boot = "Bootstrap",
                         width_bayes = "Bayesian"))

# VISUALS
ggplot(posterior_results, aes(x = p_hat, y = width_post, color = prior_label)) +
  geom_point(alpha = 0.5) +
  labs(title = "Posterior Interval by Prior",
       x = "Observed FT%", y = "Posterior Width") +
  theme_minimal()

ggplot(interval_widths, aes(x = method, y = width, fill = method)) +
  geom_boxplot() +
  labs(title = "Comparison of CI by Method",
       x = "Method", y = "Interval Width") +
  theme_minimal()

nba_data %>%
  select(Player, p_hat) %>%
  left_join(interval_widths, by = "Player") %>%
  ggplot(aes(x = p_hat, y = width, color = method)) +
  geom_point(alpha = 0.6) +
  labs(title = "Interval Width vs. Free Throw %",
       x = "Observed FT%", y = "Interval Width") +
  theme_minimal()
