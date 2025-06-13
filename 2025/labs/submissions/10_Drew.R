set.seed(123)

# -----------------------------------------------------------------------------
# Task setup: choose m = 3, find p and α so that one horse is −EV but still gets f>0
# -----------------------------------------------------------------------------
p     <- c(0.50, 0.30, 0.20)   # (p1, p2, p3)
alpha <- c(1.8, 4.0, 6.0)      # (α1, α2, α3)
# Check EVs: pi * αi − 1
ev    <- p * alpha - 1
print(ev)  # horse 1 has −EV but we will see f1>0
# -----------------------------------------------------------------------------
# Step 1 of algorithm: Permute indices so that p[i]*α[i] ≥ p[i+1]*α[i+1]
# -----------------------------------------------------------------------------
kelly_alloc <- function(p, alpha) {
  m   <- length(p)
  # 1. Order by descending edge = p*α
  idx   <- order(p * alpha, decreasing = TRUE)
  p_ord <- p[idx]
  a_ord <- alpha[idx]
  
  # -----------------------------------------------------------------------------
  # Step 2: Compute cumulative sums and find b = min_t>0 (1 - P[t])/(1 - Σ[t])
  #   where P[t] = sum_{i=1}^t p_ord[i],
  #         Σ[t] = sum_{i=1}^t 1/α_ord[i]
  # -----------------------------------------------------------------------------
  Pcum    <- cumsum(p_ord)
  Scum    <- cumsum(1 / a_ord)
  b_vals  <- (1 - Pcum) / (1 - Scum)
  t_star  <- which.min(b_vals)         # index t achieving the minimum
  b_star  <- b_vals[t_star]            # reserve fraction
  
  # -----------------------------------------------------------------------------
  # Step 3: Set f_ord[i] = max(0, p_ord[i] - b_star / a_ord[i])
  # -----------------------------------------------------------------------------
  f_ord <- p_ord - b_star / a_ord
  f_ord[f_ord < 0] <- 0
  
  # Restore original order
  f <- numeric(m)
  f[idx] <- f_ord
  return(f)
}

# Test that our allocation gives f>0 on a −EV horse
f_test <- kelly_alloc(p, alpha)
print(f_test)
print(ev < 0 & f_test > 0)  # should be TRUE for at least one horse

# -----------------------------------------------------------------------------
# Simulation function: one path of N bets under either strategy
# -----------------------------------------------------------------------------
simulate_bankroll <- function(use_full_kelly = TRUE, N = 1000) {
  bank    <- numeric(N + 1)
  bank[1] <- 1
  
  # Precompute full Kelly allocation
  f_full <- kelly_alloc(p, alpha)
  
  # Strategy 2 adjustment: zero out −EV horses, then renormalize total stake to sum(f_full)
  f_pos  <- ifelse(p * alpha - 1 > 0, f_full, 0)
  if (!use_full_kelly && sum(f_pos) > 0) {
    f_pos <- f_pos / sum(f_pos) * sum(f_full)
  }
  
  # Choose allocation vector based on strategy flag
  f_strat <- if (use_full_kelly) f_full else f_pos
  
  # Run N independent bets
  for (n in 1:N) {
    winner    <- sample(1:3, 1, prob = p)
    # return = sum over horses of [stake fraction * odds if win], plus the unbet reserve
    ret       <- sum(f_strat * ifelse(seq_along(p) == winner, alpha, 0))
    bank[n+1] <- bank[n] * (ret + (1 - sum(f_strat)))
  }
  
  return(bank)
}

# -----------------------------------------------------------------------------
# Task 2: Run M = 100 simulations of N = 1000 bets for each strategy
# -----------------------------------------------------------------------------
M <- 100
N <- 1000

traj_full <- matrix(0, nrow = M, ncol = N + 1)
traj_pos  <- matrix(0, nrow = M, ncol = N + 1)

for (i in 1:M) {
  traj_full[i, ] <- simulate_bankroll(TRUE,  N)
  traj_pos[i, ]  <- simulate_bankroll(FALSE, N)
}

# Compute average bankroll trajectories
mean_full <- colMeans(traj_full)
mean_pos  <- colMeans(traj_pos)

# -----------------------------------------------------------------------------
# Task 3: Plot the average bankroll after N bets for each strategy
# -----------------------------------------------------------------------------
plot(
  0:N, mean_full, type = "l", lwd = 2,
  xlab = "Bet Number", ylab = "Average Bankroll",
  main = "Full Kelly vs. +EV-Only Kelly (M = 100, N = 1000)",
  ylim = range(mean_full, mean_pos)
)
lines(0:N, mean_pos, lwd = 2, lty = 2)
legend(
  "topleft",
  legend = c("Full Kelly", "+EV-Only Kelly"),
  lwd    = 2,
  lty    = c(1, 2)
)