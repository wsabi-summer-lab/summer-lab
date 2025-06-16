
# 10.1.2

# 1

kelly_betting <- function(p, alpha) {
  stopifnot(length(p) == length(alpha))
  m <- length(p)
  
  expected_returns <- p * alpha
  sorted_indices <- order(expected_returns, decreasing = TRUE)
  p_sorted <- p[sorted_indices]
  alpha_sorted <- alpha[sorted_indices]
  
  b_values <- numeric(m)
  for (t in 1:m) {
    pt <- sum(p_sorted[1:t])
    sigma_t <- sum(1 / alpha_sorted[1:t])
    denom <- 1 - sigma_t
    if (denom <= 0) {
      b_values[t] <- NA
    } else {
      b_values[t] <- (1 - pt) / denom
    }
  }
  
  b_candidates <- b_values[!is.na(b_values) & b_values > 0]
  if (length(b_candidates) == 0) {
    b <- 1
  } else {
    b <- min(b_candidates)
  }
  
  f_sorted <- p_sorted - b / alpha_sorted
  f_sorted <- pmax(f_sorted, 0)
  
  f <- numeric(m)
  f[sorted_indices] <- f_sorted
  
  return(list(fractions = f, reserve = b))
}


p <- c(0.6, 0.3, 0.1)
alpha <- c(2, 3, 8)


show_expected_values <- function(p, alpha, fractions) {
  expected_return <- p * alpha
  expected_profit <- expected_return - 1
  
  df <- data.frame(
    Horse = paste0("Horse_", seq_along(p)),
    Probability = p,
    Odds = alpha,
    Fraction_Bet = fractions,
    Expected_Return = expected_return,
    Expected_Profit = expected_profit
  )
  
  return(df)
}

result <- kelly_betting(p, alpha)
ev_df <- show_expected_values(p, alpha, result$fractions)

print(ev_df)

# 2

set.seed(42)

kelly_betting <- function(p, alpha) {
  stopifnot(length(p) == length(alpha))
  m <- length(p)
  
  expected_returns <- p * alpha
  sorted_indices <- order(expected_returns, decreasing = TRUE)
  p_sorted <- p[sorted_indices]
  alpha_sorted <- alpha[sorted_indices]
  
  b_values <- numeric(m)
  for (t in 1:m) {
    pt <- sum(p_sorted[1:t])
    sigma_t <- sum(1 / alpha_sorted[1:t])
    denom <- 1 - sigma_t
    if (denom <= 0) {
      b_values[t] <- NA
    } else {
      b_values[t] <- (1 - pt) / denom
    }
  }
  
  b_candidates <- b_values[!is.na(b_values) & b_values > 0]
  if (length(b_candidates) == 0) {
    b <- 1
  } else {
    b <- min(b_candidates)
  }
  
  f_sorted <- p_sorted - b / alpha_sorted
  f_sorted <- pmax(f_sorted, 0)
  
  f <- numeric(m)
  f[sorted_indices] <- f_sorted
  
  return(list(fractions = f, reserve = b))
}

simulate_kelly_run <- function(p, alpha, N = 100, initial_bankroll = 1) {
  res <- kelly_betting(p, alpha)
  f <- res$fractions
  b <- res$reserve
  
  bankroll <- initial_bankroll
  bankroll_history <- numeric(N)
  
  for (i in 1:N) {
    winner <- sample(1:length(p), size = 1, prob = p)
    gain_factor <- b + f[winner] * alpha[winner]
    bankroll <- bankroll * gain_factor
    bankroll_history[i] <- bankroll
  }
  
  return(list(final_bankroll = bankroll, history = bankroll_history))
}

sim_result <- simulate_kelly_run(p, alpha, N = 100, initial_bankroll = 1)
sim_result

M <- 100
N <- 100

p <- c(0.6, 0.3, 0.1)
alpha <- c(2, 3, 8)

all_histories <- matrix(NA, nrow = N, ncol = M)

for (sim in 1:M) {
  sim_result <- simulate_kelly_run(p, alpha, N = N, initial_bankroll = 1)
  all_histories[, sim] <- sim_result$history
}

plot(1:N, all_histories[,1], type = "l", col = rgb(0,0,1,0.3),
     ylim = range(all_histories), xlab = "Round", ylab = "Bankroll",
     main = paste("Kelly Betting Simulation -", M, "runs"))
for (sim in 2:M) {
  lines(1:N, all_histories[,sim], col = rgb(0,0,1,0.3))
}

# Only positive EV bets

positive_ev_bet <- function(p, alpha) {
  stopifnot(length(p) == length(alpha))
  m_original <- length(p)
  
  expected_returns <- p * alpha
  positive_ev <- expected_returns > 1
  original_indices <- which(positive_ev)
  p_filtered <- p[positive_ev]
  alpha_filtered <- alpha[positive_ev]
  m_filtered <- length(p_filtered)
  
  if (m_filtered == 0) {
    return(list(fractions = rep(0, m_original), reserve = 1))
  }
  
  expected_returns_filtered <- p_filtered * alpha_filtered
  sorted_indices <- order(expected_returns_filtered, decreasing = TRUE)
  p_sorted <- p_filtered[sorted_indices]
  alpha_sorted <- alpha_filtered[sorted_indices]
  
  b_values <- numeric(m_filtered)
  for (t in 1:m_filtered) {
    pt <- sum(p_sorted[1:t])
    sigma_t <- sum(1 / alpha_sorted[1:t])
    denom <- 1 - sigma_t
    b_values[t] <- if (denom <= 0) NA else (1 - pt) / denom
  }
  
  b_candidates <- b_values[!is.na(b_values) & b_values > 0]
  b <- if (length(b_candidates) == 0) 1 else min(b_candidates)
  
  f_sorted <- pmax(p_sorted - b / alpha_sorted, 0)
  
  f_filtered <- numeric(m_filtered)
  f_filtered[sorted_indices] <- f_sorted
  f <- numeric(m_original)
  f[original_indices] <- f_filtered
  
  return(list(fractions = f, reserve = b))
}

simulate_positive_ev_run <- function(p, alpha, N = 100, initial_bankroll = 1) {
  res <- positive_ev_bet(p, alpha)
  f <- res$fractions
  b <- res$reserve
  
  bankroll <- initial_bankroll
  bankroll_history <- numeric(N)
  
  for (i in 1:N) {
    winner <- sample(1:length(p), size = 1, prob = p)
    gain_factor <- b + f[winner] * alpha[winner]
    bankroll <- bankroll * gain_factor
    bankroll_history[i] <- bankroll
  }
  
  return(list(final_bankroll = bankroll, history = bankroll_history))
}

all_histories_ev <- matrix(NA, nrow = N, ncol = M)

for (sim in 1:M) {
  result <- simulate_positive_ev_run(p, alpha, N = N, initial_bankroll = 1)
  all_histories_ev[, sim] <- result$history
}

plot(1:N, all_histories_ev[,1], type = "l", col = rgb(0, 0, 1, 0.3),
     ylim = range(all_histories_ev), xlab = "Bet Number", ylab = "Bankroll",
     main = paste("Bankroll Over Time -", M, "Simulations"))

for (sim in 2:M) {
  lines(1:N, all_histories_ev[, sim], col = rgb(0, 0, 1, 0.3))
}

# Comparing after N bets

rounds_to_check <- c(10, 25, 50, 100)

avg_kelly <- sapply(rounds_to_check, function(r) mean(all_histories[r, ]))
avg_pos_ev <- sapply(rounds_to_check, function(r) mean(all_histories_ev[r, ]))

df_avg <- data.frame(
  Round = rounds_to_check,
  Kelly_Avg = avg_kelly,
  Positive_EV_Avg = avg_pos_ev
)

print(df_avg)
