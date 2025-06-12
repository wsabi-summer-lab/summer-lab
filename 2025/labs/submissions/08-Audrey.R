library(tidyverse)
nba <- read_delim(file.choose(), delim = ";")

colnames(nba)
#8.1.2
nba <- nba %>%
  mutate(
    FT_total = FT * G,
    FTA_total = FTA * G,
    FT_percent = FT_total / FTA_total
  ) %>%
  filter(FTA_total >= 25) 
nba_ci <- nba %>%
  mutate(
    n = FTA_total,
    x = FT_total,
    p_hat = FT_percent,
    
    se_wald = sqrt(p_hat * (1 - p_hat) / n),
    wald_lower = p_hat - 1.96 * se_wald,
    wald_upper = p_hat + 1.96 * se_wald,

    x_ac = x + 2,
    n_ac = n + 4,
    p_ac = x_ac / n_ac,
    se_ac = sqrt(p_ac * (1 - p_ac) / n_ac),
    ac_lower = p_ac - 1.96 * se_ac,
    ac_upper = p_ac + 1.96 * se_ac
  )
nba_ci <- nba_ci %>%
  mutate(Player_ID = paste(Player, Tm, sep = " - "))
nba_ci %>%
  mutate(Player_ID = paste(Player, Tm, sep = " - ")) %>%  # make unique labels
  arrange(p_hat) %>%
  mutate(Player_ID = factor(Player_ID, levels = Player_ID)) %>%  # safe to factor now
  ggplot(aes(x = p_hat, y = Player_ID)) +
  geom_point(color = "black") +
  geom_errorbar(aes(xmin = wald_lower, xmax = wald_upper), color = "red", width = 0.25) +
  geom_errorbar(aes(xmin = ac_lower, xmax = ac_upper), color = "blue", width = 0.1) +
  labs(
    title = "Free Throw % 95% CI",
    x = "Free Throw % (p̂)",
    y = "Player"
  ) +
  theme_minimal()
#OMG THIS IS OUTPUT IS SUPER SCARY

#8.2.1
library(tidyr) 
p_values <- seq(0, 1, length.out = 100)
n_values <- c(10, 50, 100, 250, 500, 1000)
M <- 100 
all_p <- c()
all_n <- c()
wald_cover <- c()
agresti_cover <- c()

for (n in n_values) {
  for (p in p_values) {
    wald_success <- 0
    agresti_success <- 0
    
    for (i in 1:M) {
      x <- rbinom(1, n, p)
      p_hat <- x / n
      
      se <- sqrt(p_hat * (1 - p_hat) / n)
      lower <- p_hat - 1.96 * se
      upper <- p_hat + 1.96 * se
      if (p >= lower && p <= upper) {
        wald_success <- wald_success + 1
      }
      
      x_ac <- x + 2
      n_ac <- n + 4
      p_ac <- x_ac / n_ac
      se_ac <- sqrt(p_ac * (1 - p_ac) / n_ac)
      lower_ac <- p_ac - 1.96 * se_ac
      upper_ac <- p_ac + 1.96 * se_ac
      if (p >= lower_ac && p <= upper_ac) {
        agresti_success <- agresti_success + 1
      }
    }
    
    all_p <- c(all_p, p)
    all_n <- c(all_n, n)
    wald_cover <- c(wald_cover, wald_success / M)
    agresti_cover <- c(agresti_cover, agresti_success / M)
  }
}

df <- data.frame(
  p = all_p,
  n = all_n,
  wald = wald_cover,
  agresti = agresti_cover
)
 
df_100 <- df[df$n == 100, ]
plot(df_100$p, df_100$wald, type = "l", col = "red", ylim = c(0.7, 1),
     xlab = "True p", ylab = "Coverage", main = "n = 100")
lines(df_100$p, df_100$agresti, col = "blue")
abline(h = 0.95, lty = 2)
legend("bottomright", legend = c("Wald", "Agresti-Coull"),
       col = c("red", "blue"), lty = 1)


#8.3.2
library(ggplot2)
p_BE <- 110 / (110 + 100)  
print(paste("BE win rate:", round(p_BE, 4)))

p_values <- seq(p_BE, 1, length.out = 100)

min_n_needed <- c()

for (p in p_values) {
  found <- FALSE
  
  for (n in seq(10, 1000, 1)) {
    x <- round(p * n)   
    p_hat <- x / n
    se <- sqrt(p_hat * (1 - p_hat) / n)
    lower <- p_hat - 1.96 * se
    
    if (lower > p_BE) {
      min_n_needed <- c(min_n_needed, n)
      found <- TRUE
      break
    }
  }
  
  if (!found) {
    min_n_needed <- c(min_n_needed, NA)  
  }
}

results <- data.frame(
  true_p = p_values,
  min_n = min_n_needed
)

ggplot(results, aes(x = true_p, y = min_n)) +
  geom_line(color = "blue") +
  labs(
    title = "Bets Beat Bookie (95% CI)",
    x = "True Win Probability (p)",
    y = "Min # of Bets Needed"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "red")

