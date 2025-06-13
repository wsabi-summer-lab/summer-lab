rm(list=ls())

library(dplyr)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------

p <- c(0.6, 0.3, 0.1)
alpha <- c(2, 3, 8) # odds

ev <- p * alpha - 1
ev # second and third are negative

## function to find vector of f's, kelly bet allocations
kelly_allocation <- function(p, alpha) {
  m <- length(p)
  index <- order(p * alpha, decreasing = TRUE)
  p_sorted <- p[index]
  alpha_sorted <- alpha[index]
  
  pt <- cumsum(p_sorted)
  sigma_t <- cumsum(1 / alpha_sorted)
  
  b_vals <- (1 - pt) / (1 - sigma_t)
  b <- min(b_vals[b_vals > 0])  # Only positive b
  
  f_sorted <- pmax(0, p_sorted - b / alpha_sorted)
  f <- numeric(m)
  f[index] <- f_sorted
  return(f)
}

f_kelly <- kelly_allocation(p, alpha) 
f_kelly # second and third are positive

#-----------------------------------------------------------------------------------------------------

## strategy 2: only bet on positive EV

f_kelly_posEV <- ifelse(p * alpha - 1 > 0, f_kelly, 0)
# f_kelly_posEV <- f_kelly_posEV / sum(f_kelly_posEV)  # sum to 1

## simulation
set.seed(1)
simulate_strategy <- function(f_strategy, p, alpha, N = 1000, M = 100) {
  bankrolls <- numeric(M)
  
  for (j in 1:M) {
    bankroll <- 1
    for (i in 1:N) {
      outcome <- sample(1:3, size = 1, prob = p)
      payout <- f_strategy[outcome] * alpha[outcome]
      loss <- sum(f_strategy)
      bankroll <- bankroll * (1 - loss + payout)
    }
    bankrolls[j] <- bankroll
  }
  return(bankrolls)
}

bank_kelly <- simulate_strategy(f_kelly, p, alpha)
bank_kelly_posEV <- simulate_strategy(f_kelly_posEV, p, alpha)

df_plot <- data.frame(
  FinalBankroll = c(bank_kelly, bank_kelly_posEV),
  Strategy = rep(c("Full Kelly", "Kelly on +EV Only"), each = length(bank_kelly))
)

ggplot(df_plot, aes(x = Strategy, y = FinalBankroll, fill = Strategy)) +
  geom_boxplot() +
  labs(title = "Final Bankrolls After 1000 Bets (100 Simulations)",
       y = "Final Bankroll", x = "") +
  theme_minimal()

# Which yields more wealth on average? How much more?
mean_kelly <- mean(bank_kelly)
mean_kelly_posEV <- mean(bank_kelly_posEV)
mean_kelly > mean_kelly_posEV # true
